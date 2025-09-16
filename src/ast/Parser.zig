const std = @import("std");

const ArrayList = std.ArrayList;

pub const Self = @This();

unclosed_braces: ArrayList(BraceType),

pub const BraceType = enum {
    record,
    block,
    tuple,
    square, // List expression, list pattern, or tag union type
};

fn currentUnclosedBrace(self: *Self) ?*BraceType {
    return self.unclosed_braces.getLastOrNull();
}

pub fn chomp(self: *Self, token: Token, next: Token) void {
    switch (token) {
        .comma => {
            if (currentUnclosedBrace()) |current| {
                // If we were parsing a block and we hit a comma, then it's a record now!
                // Make this change branchlessly because we see a LOT of commas, and this
                // will only change anything in a small percentage of cases.
                const old = *current;
                const new = if (old == .block) .record else old;
                current.* = new;
            } else {
                // We hit a comma when we weren't working on a collection. Error!
                @panic("TODO report an error for comma outside collection literal");
            }

            if (self.collection_type() == .block) {
                self.set_collection_type(.record);
            }
        },
        .thin_arrow => {
            // If we were parsing a record,
            if (self.collection_type() == .block) {
                self.set_collection_type(.record);
            }
        },
    }
}
