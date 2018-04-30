extern crate csv;
extern crate libc;
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

use std::error::Error;
use std::process;
use csv::Reader;

// https://stackoverflow.com/a/24148033
use libc::c_char;
use std::ffi::CString;
use std::ffi::CStr;
use std::slice;

#[derive(Deserialize)]
enum HorizontalConstraint {
    NoHorizontal,
    LeftAny,
    Left(i32),
    RightAny,
    Right(i32),
}

#[derive(Deserialize)]
enum VerticalConstraint {
    NoVertical,
    UpAny,
    Up(i32),
    DownAny,
    Down(i32),
}

#[derive(Deserialize)]
struct FlareNode {
    regex: String, // TODO We'll need to get the function pointer, I think
    capture: bool,
    horizontal_constraint: HorizontalConstraint,
    vertical_constraint: VerticalConstraint,
}

// This should be a useful resource https://blog.rust-lang.org/2015/04/24/Rust-Once-Run-Everywhere.html

extern {
    // https://stackoverflow.com/a/47455865
    fn matchregex(s: *const c_char) -> i32;

    // See  https://llvm.moe/ocaml/Llvm.html
    //     val block_address : llvalue -> llbasicblock -> llvalue
    //
    // block_address f bb returns the address of the basic block bb in the function f. See the method llvm::BasicBlock::get.
    fn getregexpointers() -> *const (extern fn(*const c_char) -> i32);
    fn getregexpointerslength() -> i32;
    fn getflarenodes() -> *const c_char;
}

fn example(path: &str) -> Result<(), Box<Error>> {
    println!("path: {:?}", path);
    let mut rdr = Reader::from_path(path)?;
    let mut matches = vec![];
    let mut y = 0;
    for record in rdr.records() {
        let mut x = 0;
        let record_contents = record?;
        for cell in record_contents.iter() {
            // println!("{:?} matches?", cell);
            if unsafe { matchregex(CString::new(cell).unwrap().as_ptr()) } != 0 {
                // println!("yes");
                matches.push((x, y, cell.to_owned()));
            } else {
                // println!("no");
            }
            x += 1;
        }
        y += 1;
    }
    println!("matches: {:?}", matches);
    Ok(())
}

fn get_regex_pointers_wrapper<'a>() -> &'a [extern fn(*const c_char) -> i32] {
    unsafe { slice::from_raw_parts(getregexpointers(), getregexpointerslength() as usize) }
}

// TODO we should add a second parameter which is a vector of fn pointers to the regexes
#[no_mangle]
pub extern fn rustmain(cpath: *const c_char) { // TODO check if we should use extern "system" for 32-bit compat
    // https://doc.rust-lang.org/book/first-edition/ffi.html#foreign-calling-conventions
    let path = unsafe { CStr::from_ptr(cpath) }.to_str().unwrap();
    println!("path from c: {:?}", path);

    let regexptrs = get_regex_pointers_wrapper();
    println!("starting to print regex evals");
    for regexptr in regexptrs.iter() {
        // println!("The function pointer is {:x}", regexptr);
        let string_to_check = CString::new("aaaaaa").unwrap().as_ptr();
        let fptr_result = regexptr(string_to_check);
        println!("result of call: {:?}", fptr_result);
    }
    println!("ending printing regex evals");

    // We'll have one map for


    if let Err(err) = example(path) {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
