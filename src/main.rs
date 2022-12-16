
use log::{log, debug, error, log_enabled, info, warn, trace, Level, max_level};
use battledroids::{emu::machine::Machine, emu::cpu::{Register, isa::{Instruction, InstructionTable, InstructionType}}, types::Value};

fn main() {
    pretty_env_logger::init();
    log!(Level::Info, "Max logging level: {}", max_level().as_str());


    log!(Level::Info ,"Starting up...");
    let mut mc = Machine::new(true);

    if mc.init() {
        info!("Machine initialization complete.");
    } else {
        error!("Machine initialization problem.");
    }

    let rc = mc.tick();
    trace!("{:?}", rc);

    if log_enabled!(Level::Debug) {
        debug!("{:?}", Instruction::ADD(Register::R0, Register::R1));
        warn!("{:?}", Instruction::ADDI(Register::R0, 8));
        println!("{:?}", Instruction::NOP);
    }

    println!("Completed.");
}

