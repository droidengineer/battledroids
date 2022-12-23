
use std::str::FromStr;

use log::{log, debug, error, log_enabled, info, warn, trace, Level, max_level};
use battledroids::{emu::{machine::Machine, builder::Builder}, emu::cpu::{Register, isa::{Instruction, InstructionTable, InstructionType}}, types::Value};

use regex::Regex;


fn main() {
    pretty_env_logger::init();
    log!(Level::Info, "Max logging level: {}", max_level().as_str());

    //let i = "ADD R0, R1".parse::<Instruction>().unwrap();
    //let i = Instruction::from_str("ADD R0, R1").unwrap();
  //  trace!("{:?}",i);    
  
    // let mut i = "ADD R0, R1".split_whitespace();
    
    // let op = i.next();
    // let arg1 = i.next();
    // let arg2 = i.next();
    // trace!("op: {} arg1: {} arg2: {}",op.unwrap(),arg1.unwrap(),arg2.unwrap());
    let src = "SUBI R0, 5";
    let sep = Regex::new(r"([ ,]+)").expect("Invalid regex");
    let splits: Vec<_> = sep.split(src).into_iter().collect();
    for split in splits {
        println!("\"{}\"",split);
    }
    let instr = Instruction::from_str(src);
    println!("src: \"{}\" => {:?}",src,instr);
   
    let mut builder = Builder::new();
    builder.push("LDI R0, 20");
    builder.push(src);
    println!("{:?}",builder);

    battledroids::emu::builder::Builder::asm_to_file("LDI R0, 20\nSUBI R0, 5");


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

