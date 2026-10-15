//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const Allocator = std.mem.Allocator;
const private_file_permissions: std.Io.Dir.Permissions = if (@hasDecl(std.Io.Dir.Permissions, "fromMode")) .fromMode(0o600) else .default_file;
const base = @import("base");
const unbundle = @import("unbundle.zig");
const localhost = @import("localhost.zig");

// Buffer size for file I/O operations (8KB is efficient for typical filesystem block sizes)
const IO_BUFFER_SIZE: usize = 8 * 1024;

// Maximum retries for temp file creation (handles rare collisions)
const MAX_TEMP_FILE_RETRIES: usize = 10;

// Length of random suffix for temp filenames
const RANDOM_SUFFIX_LEN: usize = 16;
/// Maximum redirect responses followed during one package download.
pub const MAX_REDIRECTS: usize = 3;

/// Resolve and validate a redirect before the next connection is opened.
/// The returned URI borrows memory from `allocator`.
pub fn resolveRedirect(allocator: Allocator, current: std.Uri, location: []const u8) DownloadError!std.Uri {
    const current_url = try std.fmt.allocPrint(allocator, "{f}", .{std.Uri.fmt(&current, .all)});
    const buffer = try allocator.alloc(u8, current_url.len + location.len + 1);
    @memcpy(buffer[0..location.len], location);
    var remaining = buffer;
    const next = current.resolveInPlace(location.len, &remaining) catch return error.InvalidUrl;
    const next_url = try std.fmt.allocPrint(allocator, "{f}", .{std.Uri.fmt(&next, .all)});
    _ = try validateUrl(next_url);
    return next;
}

/// How one request of a download reaches its server.
pub const Route = union(enum) {
    /// Plain HTTP to this machine, sent over a connection to exactly this
    /// address. A `localhost` host has already been verified to resolve to it.
    loopback_http: std.Io.net.IpAddress,
    /// HTTPS to this machine. TLS authenticates the server, and no proxy is
    /// used because a proxy's loopback is a different machine.
    loopback_https,
    /// HTTPS to any other host, through the configured proxies.
    remote_https,
};

/// Choose how to reach `uri`, which must already have passed `validateUrl`.
pub fn routeFor(uri: std.Uri) DownloadError!Route {
    const host = switch (uri.host.?) {
        .raw, .percent_encoded => |text| text,
    };
    const is_localhost = std.mem.eql(u8, host, "localhost");
    if (std.mem.eql(u8, uri.scheme, "https")) {
        return if (is_localhost or std.mem.eql(u8, host, "127.0.0.1")) .loopback_https else .remote_https;
    }
    if (!std.mem.eql(u8, uri.scheme, "http")) base.invariant("validated download URL has scheme {s}", .{uri.scheme});
    const port = uri.port orelse 80;
    if (is_localhost) {
        return .{ .loopback_http = switch (try localhost.resolveLoopback()) {
            .ip4 => .{ .ip4 = .loopback(port) },
            .ip6 => .{ .ip6 = .loopback(port) },
        } };
    }
    if (std.mem.eql(u8, host, "127.0.0.1")) return .{ .loopback_http = .{ .ip4 = .loopback(port) } };
    if (!std.mem.eql(u8, host, "[::1]")) base.invariant("validated plain HTTP download URL has host {s}", .{host});
    return .{ .loopback_http = .{ .ip6 = .loopback(port) } };
}

/// Generate a random alphanumeric suffix for unique temp filenames.
/// Uses cryptographically secure random bytes mapped to alphanumeric characters.
fn generateRandomSuffix(io: std.Io, buf: *[RANDOM_SUFFIX_LEN]u8) void {
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    io.random(buf);
    for (buf) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }
}

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    InvalidVersion,
    AmbiguousVersion,
    LocalhostWasNotLoopback,
    InvalidHash,
    HttpError,
    NoHashInUrl,
    NetworkError,
    FileError,
    InvalidProxyUrl,
} || unbundle.UnbundleError || std.mem.Allocator.Error;

pub const Version = base.url.Version;

pub const ParsedUrl = base.url.ParsedUrl;

pub const parseUrlPath = base.url.parseUrlPath;

/// Parse URL and validate it meets our security requirements.
/// Returns the parsed hash and optional version from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError!ParsedUrl {
    // The shared allowlist gate: https anywhere, or http only to loopback
    // hosts. A plain HTTP `localhost` URL is additionally verified to resolve
    // to a loopback address in `routeFor` before any request is made.
    if (!base.url.isSafeUrl(url)) {
        return error.InvalidUrl;
    }

    return parseUrlPath(url);
}

/// Options controlling download and extraction.
pub const DownloadOptions = struct {
    /// Maximum allowed decompressed size of the bundle in bytes, or null for
    /// an explicit opt-out. Defaults to the shared 512 MiB bundle limit.
    max_expanded_bytes: ?u64 = base.max_bundle_expanded_bytes,
};

/// Download and extract a bundled tar.zst file from a URL.
/// Returns the total decompressed size of the bundle in bytes.
///
/// The URL must:
/// - Pass `base.url.isSafeUrl`: https, or http to a loopback host
/// - Optionally have a MAJOR.MINOR.PATCH path segment before the hash
/// - Have the base58-encoded blake3 hash as the last path segment
/// - Point to a tar.zst file created with `roc bundle`
///
/// `dest_path` must be an existing directory path. Downloads to a temp file
/// first, then streams from that file for extraction.
pub fn downloadAndExtract(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    dest_path: []const u8,
    options: DownloadOptions,
) DownloadError!u64 {
    var extract_dir = std.Io.Dir.cwd().openDir(io, dest_path, .{}) catch return error.FileError;
    defer extract_dir.close(io);

    // Validate URL and extract hash
    const parsed_url = try validateUrl(url);
    const base58_hash = parsed_url.hash;

    // Validate the hash before starting any I/O
    const expected_hash = (try unbundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Construct the final filename (for caching after successful extraction)
    var filename_buf: [64]u8 = undefined;
    const filename = std.fmt.bufPrint(&filename_buf, "{s}.tar.zst", .{base58_hash}) catch {
        return error.InvalidHash;
    };

    // Download to a temp file with unique random suffix
    // Buffer size: . + hash(~44) + _ + random(16) + .tmp + null = ~70 bytes, use 96 for safety
    var temp_filename_buf: [96]u8 = undefined;
    const temp_filename = try downloadToFile(allocator, io, url, extract_dir, base58_hash, &temp_filename_buf);

    // Open the downloaded file for reading
    var temp_file = extract_dir.openFile(io, temp_filename, .{}) catch {
        return error.FileError;
    };
    defer temp_file.close(io);

    // Create a buffered reader from the file
    var read_buffer: [IO_BUFFER_SIZE]u8 = undefined;
    var file_reader = temp_file.reader(io, &read_buffer);

    // Setup directory extract writer
    var dir_writer = unbundle.DirExtractWriter.init(extract_dir, io, allocator.*);
    defer dir_writer.deinit();

    // Extract the content using the streaming architecture
    const expanded_bytes = unbundle.unbundleStream(allocator.*, &file_reader.interface, dir_writer.extractWriter(), &expected_hash, null, .{
        .max_expanded_bytes = options.max_expanded_bytes,
    }) catch |err| {
        // Clean up temp file on error
        extract_dir.deleteFile(io, temp_filename) catch {};
        return err;
    };

    // Rename temp file to final name (keeps bundle cached)
    extract_dir.rename(temp_filename, extract_dir, filename, io) catch {
        // If rename fails, just delete the temp file
        extract_dir.deleteFile(io, temp_filename) catch {};
    };

    return expanded_bytes;
}

/// Download HTTP response body to a file with a unique random suffix.
/// Creates a temp file with format: .{prefix}_{random16}.tmp
/// Uses exclusive file creation to prevent race conditions between processes.
/// Returns the actual filename created via the filename_out buffer.
fn downloadToFile(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    dir: std.Io.Dir,
    filename_prefix: []const u8,
    filename_out: []u8,
) DownloadError![]const u8 {
    // Proxy configuration must outlive the clients, so its arena is declared first.
    var proxy_arena = std.heap.ArenaAllocator.init(allocator.*);
    defer proxy_arena.deinit();

    // HTTPS to other hosts honors the standard proxy environment variables so
    // downloads work in proxied/network-restricted environments (same
    // variables `zig fetch` uses).
    var remote_client = std.http.Client{ .allocator = allocator.*, .io = io };
    defer remote_client.deinit();
    try initProxiesFromEnv(&remote_client, proxy_arena.allocator());

    // HTTPS to this machine never goes through a proxy.
    var loopback_client = std.http.Client{ .allocator = allocator.*, .io = io };
    defer loopback_client.deinit();

    // Redirect URI components live for the entire request chain.
    var redirect_arena = std.heap.ArenaAllocator.init(allocator.*);
    defer redirect_arena.deinit();
    var uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Try to create temp file with unique random suffix
    var attempts: usize = 0;
    while (attempts < MAX_TEMP_FILE_RETRIES) : (attempts += 1) {
        // Generate random suffix for this attempt
        var random_suffix: [RANDOM_SUFFIX_LEN]u8 = undefined;
        generateRandomSuffix(io, &random_suffix);

        // Build filename: .{prefix}_{random}.tmp
        const filename = std.fmt.bufPrint(filename_out, ".{s}_{s}.tmp", .{
            filename_prefix,
            random_suffix,
        }) catch return error.FileError;

        // Try to create file with exclusive flag (fails if file already exists)
        var file = dir.createFile(io, filename, .{ .exclusive = true, .permissions = private_file_permissions }) catch |err| switch (err) {
            error.AccessDenied,
            error.AntivirusInterference,
            error.BadPathName,
            error.Canceled,
            error.DeviceBusy,
            error.FileBusy,
            error.FileLocksUnsupported,
            error.FileNotFound,
            error.FileTooBig,
            error.IsDir,
            error.NameTooLong,
            error.NetworkNotFound,
            error.NoDevice,
            error.NoSpaceLeft,
            error.NotDir,
            error.PermissionDenied,
            error.PipeBusy,
            error.ProcessFdQuotaExceeded,
            error.ReadOnlyFileSystem,
            error.SymLinkLoop,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.Unexpected,
            error.WouldBlock,
            => return error.FileError,
            error.PathAlreadyExists => continue, // Retry with new random suffix
        };
        var file_closed = false;
        errdefer {
            if (!file_closed) file.close(io);
            dir.deleteFile(io, filename) catch {};
        }

        // Create a writer for the file
        var write_buffer: [IO_BUFFER_SIZE]u8 = undefined;
        var file_writer = file.writer(io, &write_buffer);

        var redirects: usize = 0;
        while (true) {
            const next = switch (try routeFor(uri)) {
                .loopback_http => |address| try getLoopbackHttp(allocator.*, io, address, redirect_arena.allocator(), uri, redirects, &file_writer.interface),
                .loopback_https => try getHttps(allocator.*, &loopback_client, redirect_arena.allocator(), uri, redirects, &file_writer.interface),
                .remote_https => try getHttps(allocator.*, &remote_client, redirect_arena.allocator(), uri, redirects, &file_writer.interface),
            };
            uri = next orelse break;
            redirects += 1;
        }

        // Flush the writer before closing
        file_writer.interface.flush() catch {
            return error.FileError;
        };

        // Close file after fetch completes
        file.close(io);
        file_closed = true;

        return filename;
    }

    // Exhausted all retries (extremely unlikely with 16-char random suffix)
    return error.FileError;
}

/// Request `uri` over HTTPS with the standard client, writing an OK body to
/// `out`. Returns the validated next URI when the server redirects.
fn getHttps(
    allocator: Allocator,
    client: *std.http.Client,
    redirect_arena: Allocator,
    uri: std.Uri,
    redirects: usize,
    out: *std.Io.Writer,
) DownloadError!?std.Uri {
    var request = client.request(.GET, uri, .{ .redirect_behavior = .unhandled }) catch return error.HttpError;
    defer request.deinit();
    request.sendBodiless() catch return error.HttpError;
    var head_buffer: [16 * 1024]u8 = undefined;
    const response = request.receiveHead(&head_buffer) catch return error.HttpError;
    return followResponse(allocator, redirect_arena, uri, redirects, response.head, &request.reader, out);
}

/// Request `uri` over plain HTTP on a connection to exactly `address`, writing
/// an OK body to `out`. Returns the validated next URI when the server
/// redirects.
fn getLoopbackHttp(
    allocator: Allocator,
    io: std.Io,
    address: std.Io.net.IpAddress,
    redirect_arena: Allocator,
    uri: std.Uri,
    redirects: usize,
    out: *std.Io.Writer,
) DownloadError!?std.Uri {
    const stream = address.connect(io, .{ .mode = .stream }) catch return error.HttpError;
    defer stream.close(io);

    var write_buffer: [1024]u8 = undefined;
    var stream_writer = stream.writer(io, &write_buffer);
    writeLoopbackRequest(&stream_writer.interface, uri) catch return error.HttpError;

    var read_buffer: [16 * 1024]u8 = undefined;
    var stream_reader = stream.reader(io, &read_buffer);
    var reader: std.http.Reader = .{
        .in = &stream_reader.interface,
        .state = .ready,
        // Populated by `bodyReaderDecompressing`.
        .interface = undefined,
        .max_head_len = read_buffer.len,
    };
    const head_bytes = reader.receiveHead() catch return error.HttpError;
    const head = std.http.Client.Response.Head.parse(head_bytes) catch return error.HttpError;
    return followResponse(allocator, redirect_arena, uri, redirects, head, &reader, out);
}

/// Write a GET request for `uri` whose one Host header is the URL's own
/// authority, so the server sees the host the URL names.
pub fn writeLoopbackRequest(w: *std.Io.Writer, uri: std.Uri) std.Io.Writer.Error!void {
    try w.writeAll("GET ");
    try uri.writeToStream(w, .{ .path = true, .query = true });
    try w.writeAll(" HTTP/1.1\r\nhost: ");
    try uri.writeToStream(w, .{ .authority = true });
    try w.writeAll("\r\nconnection: close\r\naccept-encoding: zstd, gzip, deflate\r\n\r\n");
    try w.flush();
}

/// Act on a received response head: resolve and validate a redirect, or
/// stream an OK body (decoding its content encoding) to `out`.
fn followResponse(
    allocator: Allocator,
    redirect_arena: Allocator,
    uri: std.Uri,
    redirects: usize,
    head: std.http.Client.Response.Head,
    reader: *std.http.Reader,
    out: *std.Io.Writer,
) DownloadError!?std.Uri {
    if (head.status.class() == .redirect) {
        if (redirects == MAX_REDIRECTS) return error.HttpError;
        const location = head.location orelse return error.HttpError;
        return try resolveRedirect(redirect_arena, uri, location);
    }
    if (head.status != .ok) return error.HttpError;

    var transfer_buffer: [64]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    const decompress_len: usize = switch (head.content_encoding) {
        .identity => 0,
        .zstd => std.compress.zstd.default_window_len,
        .deflate, .gzip => std.compress.flate.max_window_len,
        .compress => return error.HttpError,
    };
    const decompress_buffer = try allocator.alloc(u8, decompress_len);
    defer allocator.free(decompress_buffer);
    const body = reader.bodyReaderDecompressing(
        &transfer_buffer,
        head.transfer_encoding,
        head.content_length,
        head.content_encoding,
        &decompress,
        decompress_buffer,
    );
    _ = body.streamRemaining(out) catch return error.HttpError;
    return null;
}

/// Populate the client's proxy configuration from the standard proxy
/// environment variables (http_proxy/HTTP_PROXY, https_proxy/HTTPS_PROXY,
/// all_proxy/ALL_PROXY). `arena` owns the proxy allocations and must outlive
/// `client`.
fn initProxiesFromEnv(client: *std.http.Client, arena: Allocator) DownloadError!void {
    var environ_map = std.process.Environ.Map.init(arena);
    const proxy_env_vars = [_][:0]const u8{
        "http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY", "all_proxy", "ALL_PROXY",
    };
    for (proxy_env_vars) |name| {
        const value = std.c.getenv(name.ptr) orelse continue;
        const value_slice = std.mem.span(value);
        if (value_slice.len == 0) continue;
        environ_map.put(name, value_slice) catch return error.OutOfMemory;
    }
    client.initDefaultProxies(arena, &environ_map) catch |err| switch (err) {
        error.InvalidFormat,
        error.InvalidHostName,
        error.NameTooLong,
        error.InvalidPort,
        error.UnexpectedCharacter,
        error.UriMissingHost,
        => return error.InvalidProxyUrl,
        error.Canceled => return error.NetworkError,
        error.OutOfMemory => return error.OutOfMemory,
    };
}
