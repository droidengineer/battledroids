
use log::{log, debug, error, log_enabled, info, warn, trace, Level, LevelFilter, max_level, set_max_level};
use battledroids::{emu::machine::Machine, emu::cpu::{Register, isa::{InstructionSet, InstructionTable, InstructionRecord, InstructionType}}, types::Value};

fn main() {
    pretty_env_logger::init();
    println!("Max logging level: {}", max_level());
    // set_max_level(LevelFilter::Trace);
    log!(Level::Info ,"Max logging level: {}", max_level().as_str());


    log!(Level::Error ,"Starting up...");
    let mut mc = Machine::new(true);

    if mc.init() {
        info!("Machine initialization complete.");
    } else {
        error!("<ERROR> Machine initialization problem.");
    }

    let rc = mc.tick();
    trace!("{:?}", rc);

    debug!("{:?}", InstructionSet::ADD(Register::R0, Register::R1));
    warn!("{:?}", InstructionSet::ADDI(Register::R0, Value::Immediate(8)  ));
    println!("{:?}", InstructionSet::NOP);


    println!("Completed.");
}

