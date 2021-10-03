use parity_wasm::elements::{Instruction, Instruction::*};
use roc_module::symbol::Symbol;

pub struct CodeBuilder {
    stack: Vec<Option<(Symbol, usize)>>,
    code: Vec<Instruction>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        CodeBuilder {
            stack: Vec::with_capacity(32),
            code: Vec::with_capacity(1024),
        }
    }

    pub fn clear(&mut self) {
        self.stack.clear();
        self.code.clear();
    }

    pub fn push(&mut self, inst: Instruction) {
        let (pops, push) = get_pops_and_pushes(&inst);
        let new_len = self.stack.len() - pops as usize;
        self.stack.truncate(new_len);
        if push {
            self.stack.push(None);
        }
        self.code.push(inst);
    }

    pub fn extend(&mut self, instructions: &[Instruction]) {
        let old_len = self.stack.len();
        let mut len = old_len;
        let mut min_len = len;
        for inst in instructions {
            let (pops, push) = get_pops_and_pushes(&inst);
            len -= pops as usize;
            if len < min_len {
                min_len = len;
            }
            if push {
                len += 1;
            }
        }
        self.stack.truncate(min_len);
        self.stack.resize(len, None);
        self.code.extend_from_slice(instructions);
    }

    pub fn drain(&mut self) -> std::vec::Drain<Instruction> {
        self.code.drain(0..)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn set_top_symbol(&mut self, sym: Symbol) {
        let len = self.stack.len();
        let code_index = self.code.len();
        self.stack[len - 1] = Some((sym, code_index));
    }

    pub fn get_top_symbol(&self) -> Option<(Symbol, usize)> {
        let len = self.stack.len();
        self.stack[len - 1]
    }
}

fn get_pops_and_pushes(inst: &Instruction) -> (u8, bool) {
    match inst {
        Unreachable => (0, false),
        Nop => (0, false),
        Block(_) => (0, false),
        Loop(_) => (0, false),
        If(_) => (0, false),
        Else => (0, false),
        End => (0, false),
        Br(_) => (0, false),
        BrIf(_) => (0, false),
        BrTable(_) => (0, false),
        Return => (0, false),

        Call(_) => (0, false), // depends on the function! handle this elsewhere
        CallIndirect(_, _) => (0, false),

        Drop => (1, false),
        Select => (3, true),

        GetLocal(_) => (0, true),
        SetLocal(_) => (1, false),
        TeeLocal(_) => (1, true),
        GetGlobal(_) => (0, true),
        SetGlobal(_) => (1, false),

        I32Load(_, _) => (1, true),
        I64Load(_, _) => (1, true),
        F32Load(_, _) => (1, true),
        F64Load(_, _) => (1, true),
        I32Load8S(_, _) => (1, true),
        I32Load8U(_, _) => (1, true),
        I32Load16S(_, _) => (1, true),
        I32Load16U(_, _) => (1, true),
        I64Load8S(_, _) => (1, true),
        I64Load8U(_, _) => (1, true),
        I64Load16S(_, _) => (1, true),
        I64Load16U(_, _) => (1, true),
        I64Load32S(_, _) => (1, true),
        I64Load32U(_, _) => (1, true),
        I32Store(_, _) => (2, false),
        I64Store(_, _) => (2, false),
        F32Store(_, _) => (2, false),
        F64Store(_, _) => (2, false),
        I32Store8(_, _) => (2, false),
        I32Store16(_, _) => (2, false),
        I64Store8(_, _) => (2, false),
        I64Store16(_, _) => (2, false),
        I64Store32(_, _) => (2, false),

        CurrentMemory(_) => (0, true),
        GrowMemory(_) => (1, true),
        I32Const(_) => (0, true),
        I64Const(_) => (0, true),
        F32Const(_) => (0, true),
        F64Const(_) => (0, true),

        I32Eqz => (1, true),
        I32Eq => (2, true),
        I32Ne => (2, true),
        I32LtS => (2, true),
        I32LtU => (2, true),
        I32GtS => (2, true),
        I32GtU => (2, true),
        I32LeS => (2, true),
        I32LeU => (2, true),
        I32GeS => (2, true),
        I32GeU => (2, true),

        I64Eqz => (1, true),
        I64Eq => (2, true),
        I64Ne => (2, true),
        I64LtS => (2, true),
        I64LtU => (2, true),
        I64GtS => (2, true),
        I64GtU => (2, true),
        I64LeS => (2, true),
        I64LeU => (2, true),
        I64GeS => (2, true),
        I64GeU => (2, true),

        F32Eq => (2, true),
        F32Ne => (2, true),
        F32Lt => (2, true),
        F32Gt => (2, true),
        F32Le => (2, true),
        F32Ge => (2, true),

        F64Eq => (2, true),
        F64Ne => (2, true),
        F64Lt => (2, true),
        F64Gt => (2, true),
        F64Le => (2, true),
        F64Ge => (2, true),

        I32Clz => (1, true),
        I32Ctz => (1, true),
        I32Popcnt => (1, true),
        I32Add => (2, true),
        I32Sub => (2, true),
        I32Mul => (2, true),
        I32DivS => (2, true),
        I32DivU => (2, true),
        I32RemS => (2, true),
        I32RemU => (2, true),
        I32And => (2, true),
        I32Or => (2, true),
        I32Xor => (2, true),
        I32Shl => (2, true),
        I32ShrS => (2, true),
        I32ShrU => (2, true),
        I32Rotl => (2, true),
        I32Rotr => (2, true),

        I64Clz => (1, true),
        I64Ctz => (1, true),
        I64Popcnt => (1, true),
        I64Add => (2, true),
        I64Sub => (2, true),
        I64Mul => (2, true),
        I64DivS => (2, true),
        I64DivU => (2, true),
        I64RemS => (2, true),
        I64RemU => (2, true),
        I64And => (2, true),
        I64Or => (2, true),
        I64Xor => (2, true),
        I64Shl => (2, true),
        I64ShrS => (2, true),
        I64ShrU => (2, true),
        I64Rotl => (2, true),
        I64Rotr => (2, true),

        F32Abs => (1, true),
        F32Neg => (1, true),
        F32Ceil => (1, true),
        F32Floor => (1, true),
        F32Trunc => (1, true),
        F32Nearest => (1, true),
        F32Sqrt => (1, true),
        F32Add => (2, true),
        F32Sub => (2, true),
        F32Mul => (2, true),
        F32Div => (2, true),
        F32Min => (2, true),
        F32Max => (2, true),
        F32Copysign => (2, true),

        F64Abs => (1, true),
        F64Neg => (1, true),
        F64Ceil => (1, true),
        F64Floor => (1, true),
        F64Trunc => (1, true),
        F64Nearest => (1, true),
        F64Sqrt => (1, true),
        F64Add => (2, true),
        F64Sub => (2, true),
        F64Mul => (2, true),
        F64Div => (2, true),
        F64Min => (2, true),
        F64Max => (2, true),
        F64Copysign => (2, true),

        I32WrapI64 => (1, true),
        I32TruncSF32 => (1, true),
        I32TruncUF32 => (1, true),
        I32TruncSF64 => (1, true),
        I32TruncUF64 => (1, true),
        I64ExtendSI32 => (1, true),
        I64ExtendUI32 => (1, true),
        I64TruncSF32 => (1, true),
        I64TruncUF32 => (1, true),
        I64TruncSF64 => (1, true),
        I64TruncUF64 => (1, true),
        F32ConvertSI32 => (1, true),
        F32ConvertUI32 => (1, true),
        F32ConvertSI64 => (1, true),
        F32ConvertUI64 => (1, true),
        F32DemoteF64 => (1, true),
        F64ConvertSI32 => (1, true),
        F64ConvertUI32 => (1, true),
        F64ConvertSI64 => (1, true),
        F64ConvertUI64 => (1, true),
        F64PromoteF32 => (1, true),

        I32ReinterpretF32 => (1, true),
        I64ReinterpretF64 => (1, true),
        F32ReinterpretI32 => (1, true),
        F64ReinterpretI64 => (1, true),
    }
}
