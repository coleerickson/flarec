extern crate csv;
extern crate libc;

use std::error::Error;
use std::process;
use csv::Reader;

// https://stackoverflow.com/a/24148033
use libc::c_char;
use std::ffi::CString;
use std::ffi::CStr;

// TODO Compile this as a library, exposing the main parse functions
// Then `rustc` doesn't need to be invoked in `flarec`; `flarec` just needs to link against the library
// This should be a useful resource https://blog.rust-lang.org/2015/04/24/Rust-Once-Run-Everywhere.html

// // Returns true if `s` matches the regex.
// fn matchregex(s: &str) -> bool {
// // TODO make this link to the LLVM output of flarec.
//     true
// }

extern {
    // https://stackoverflow.com/a/47455865
    fn matchregex(s: *const c_char) -> i32;
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

#[no_mangle]
pub extern fn rustmain(cpath: *const c_char) { // TODO check if we should use extern "system" for 32-bit compat
    // https://doc.rust-lang.org/book/first-edition/ffi.html#foreign-calling-conventions
    let path = unsafe { CStr::from_ptr(cpath) }.to_str().unwrap();
    println!("path from c: {:?}", path);
    if let Err(err) = example(path) {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
