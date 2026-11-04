//! Build-time conversion of a target's embedded support objects (the
//! builtins and the Boxy runtime) into one unit image (`UnitImage`), which
//! a fast dev link places beside the program's unit images. The image is a
//! function of the objects alone, so it is made once here, in Roc's build
//! graph, and embedded next to them.
//!
//! The objects must use exactly the sections, symbols and relocations
//! `UnitImage.build` handles; anything else fails Roc's build.
//!
//! Usage: support_image <output.rbxu> <object.o>...

const std = @import("std");
const UnitImage = @import("UnitImage");

const MainError = std.process.Args.ToSliceError || std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.WriteFileError || UnitImage.BuildError || error{ExpectedOutputAndObjects};

/// Convert the objects named on the command line into one image file.
pub fn main(init: std.process.Init) MainError!void {
    const gpa = init.gpa;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 3) return error.ExpectedOutputAndObjects;
    const objects = try gpa.alloc([]const u8, args.len - 2);
    defer gpa.free(objects);
    var read: usize = 0;
    defer for (objects[0..read]) |bytes| gpa.free(bytes);
    for (args[2..], objects) |path, *bytes| {
        bytes.* = try std.Io.Dir.cwd().readFileAlloc(init.io, path, gpa, .unlimited);
        read += 1;
    }
    const image = UnitImage.build(gpa, objects, .{ .keep_debug = false, .atoms = .whole }) catch |err| {
        std.log.err("cannot convert support objects into a unit image: {s}", .{@errorName(err)});
        return err;
    };
    defer gpa.free(image);
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[1], .data = image });
}
