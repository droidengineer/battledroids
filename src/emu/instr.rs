


impl InstructionTable {
    pub fn load(&self) {
        let mut instr_table: InstructionTable = InstructionTable::new();
        instr_table.insert(InstructionRecord {
            mnemonic: "NOOP",
            opcode: 0,
            ext: Some(0),
            operands: "",
            operation: "",
            flags: None,
            description: "No operation.",
            format: InstructionType::R, 
        });
        instr_table.insert(InstructionRecord {
            mnemonic: "ADD",
            opcode: 1,
            ext: Some(0),
            operands: "Rd, Rs",
            operation: "Rd = Rd + Rs",
            flags: Some("ZCNVSH"),
            description: "Add",
            format: InstructionType::R, 
        });
    }
}

