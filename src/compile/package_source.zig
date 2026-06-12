//! Source URL identity for packages downloaded from URLs: the full URL plus
//! the span of its url id (the part identifying the package across versions).

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;

/// A borrowed view of a package's source URL and the span of its url id.
pub const UrlSourceView = struct {
    url: []const u8,
    url_id: base.url.UrlId,
};

/// An owned copy of a package's source URL and the span of its url id.
pub const UrlSource = struct {
    url: []u8,
    url_id: base.url.UrlId,

    pub fn init(gpa: Allocator, source_view: UrlSourceView) Allocator.Error!UrlSource {
        return .{
            .url = try gpa.dupe(u8, source_view.url),
            .url_id = source_view.url_id,
        };
    }

    pub fn deinit(self: *UrlSource, gpa: Allocator) void {
        gpa.free(self.url);
    }

    pub fn view(self: *const UrlSource) UrlSourceView {
        return .{
            .url = self.url,
            .url_id = self.url_id,
        };
    }

    pub fn urlId(self: *const UrlSource) []const u8 {
        return self.url_id.slice(self.url);
    }
};

test "UrlSource returns package URL id" {
    const url = "https://example.com/foo/bar/1.2.3/hash";
    var source = try UrlSource.init(std.testing.allocator, .{
        .url = url,
        .url_id = .{ .start = 8, .len = 19 },
    });
    defer source.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("example.com/foo/bar", source.urlId());
}
