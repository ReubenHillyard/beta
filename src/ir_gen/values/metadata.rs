use crate::ir_gen::Compiler;
use inkwell::values::{InstructionOpcode, InstructionValue};

impl<'ctx> Compiler<'ctx> {
    /// Adds the metadata `!invariant.group !{}` to a load or store.
    ///
    /// Panics if the instruction is not a load or store.
    ///
    /// Requires the instruction does not already have metadata.
    pub fn add_invariant_group_metadata(&self, instruction_value: InstructionValue<'ctx>) {
        let opcode = instruction_value.get_opcode();
        assert!(opcode == InstructionOpcode::Load || opcode == InstructionOpcode::Store);
        instruction_value
            .set_metadata(
                self.context.metadata_node(&[]),
                self.context.get_kind_id("invariant.group"),
            )
            .unwrap();
    }
}
