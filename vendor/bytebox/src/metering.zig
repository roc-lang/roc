const config = @import("config");
const Instruction = @import("definition.zig").Instruction;

pub const enabled = config.enable_metering;

pub const Meter = if (enabled) usize else void;

pub const initial_meter = if (enabled) 0 else {};

pub const MeteringTrapError = if (enabled) error{TrapMeterExceeded} else error{};

pub fn reduce(fuel: Meter, instruction: Instruction) Meter {
    if (fuel == 0) {
        return fuel;
    }
    return switch (instruction.opcode) {
        .Invalid, .Unreachable, .DebugTrap, .Noop, .Block, .Loop, .If, .IfNoElse, .Else, .End, .Branch, .Branch_If, .Branch_Table, .Drop => fuel,
        else => fuel - 1,
    };
}
