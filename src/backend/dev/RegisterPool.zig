//! Temporary-register and code-buffer accessors shared by the x86_64 and
//! aarch64 `CodeGen` types.
//!
//! LirCodeGen keeps every semantic local in its authoritative stable-location
//! table; the masks these functions manage only cover bounded, short-lived
//! instruction-selection temporaries. The host `CodeGen` supplies the fields
//! `emit`, `free_general`, `free_float`, `callee_saved_used`, and
//! `callee_saved_available`, and re-exports the functions as its own methods.

/// The register-pool methods for `Self`, whose callee-saved general registers
/// available for allocation are `callee_saved_mask`.
pub fn Methods(comptime Self: type, comptime GeneralReg: type, comptime FloatReg: type, comptime callee_saved_mask: u32) type {
    return struct {
        /// Get the generated code
        pub fn getCode(self: *Self) []const u8 {
            return self.emit.buf.items;
        }

        /// Get current code offset
        pub fn currentOffset(self: *Self) usize {
            return self.emit.buf.items.len;
        }

        pub fn allocGeneral(self: *Self) ?GeneralReg {
            // Try caller-saved first
            if (allocFromGeneralMask(&self.free_general)) |reg| {
                return reg;
            }
            // Try callee-saved
            if (allocFromGeneralMask(&self.callee_saved_available)) |reg| {
                self.callee_saved_used |= @as(@TypeOf(self.callee_saved_used), 1) << @intCast(@backingInt(reg));
                return reg;
            }
            return null;
        }

        fn allocFromGeneralMask(mask: *u32) ?GeneralReg {
            if (mask.* == 0) return null;
            const bit: u5 = @intCast(@ctz(mask.*));
            mask.* &= ~(@as(u32, 1) << bit);
            return @fromBackingInt(@intCast(bit));
        }

        /// Free a general-purpose register, making it available for allocation.
        pub fn freeGeneral(self: *Self, reg: GeneralReg) void {
            const idx: u5 = @intCast(@backingInt(reg));
            // Return to appropriate pool
            if ((callee_saved_mask & (@as(u32, 1) << idx)) != 0) {
                self.callee_saved_available |= @as(u32, 1) << idx;
            } else {
                self.free_general |= @as(u32, 1) << idx;
            }
        }

        /// Mark a register as in use so it won't be allocated.
        /// Used for return values from function calls that need to persist.
        pub fn markRegisterInUse(self: *Self, reg: GeneralReg) void {
            const idx: u5 = @intCast(@backingInt(reg));
            // Remove from free pool (it's now in use)
            self.free_general &= ~(@as(u32, 1) << idx);
            self.callee_saved_available &= ~(@as(u32, 1) << idx);
        }

        pub fn allocFloat(self: *Self) ?FloatReg {
            if (self.free_float == 0) return null;
            const bit: u5 = @intCast(@ctz(self.free_float));
            self.free_float &= ~(@as(u32, 1) << bit);
            return @fromBackingInt(@intCast(bit));
        }

        pub fn freeFloat(self: *Self, reg: FloatReg) void {
            const idx: u5 = @intCast(@backingInt(reg));
            self.free_float |= @as(u32, 1) << idx;
        }
    };
}
