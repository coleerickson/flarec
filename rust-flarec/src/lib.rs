extern crate csv;
extern crate libc;
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

use std::error::Error;
use std::process;
use csv::Reader;
use csv::Writer;

// https://stackoverflow.com/a/24148033
use libc::c_char;
use std::ffi::CString;
use std::ffi::CStr;
use std::slice;

#[derive(Serialize, Deserialize, Debug)]
enum HorizontalConstraint {
    NoHorizontal,
    LeftAny,
    Left(i32),
    RightAny,
    Right(i32),
}

#[derive(Serialize, Deserialize, Debug)]
enum VerticalConstraint {
    NoVertical,
    UpAny,
    Up(i32),
    DownAny,
    Down(i32),
}

#[derive(Serialize, Deserialize, Debug)]
struct FlareNode {
    id: usize, // TODO We'll need to get the function pointer, I think
    is_capture: bool,
    horizontal_constraint: HorizontalConstraint,
    vertical_constraint: VerticalConstraint,
    successors: Vec<usize>,
}

// This should be a useful resource https://blog.rust-lang.org/2015/04/24/Rust-Once-Run-Everywhere.html

extern {
    // https://stackoverflow.com/a/47455865
    // fn matchregex(s: *const c_char) -> i32;

    fn getregexpointers() -> *const (extern fn(*const c_char) -> i32);
    fn getregexpointerslength() -> i32;
    fn getflarenodes() -> *const c_char;
}

fn get_regex_pointers_wrapper<'a>() -> &'a [extern fn(*const c_char) -> i32] {
    let pointers_length = unsafe { getregexpointerslength() } as usize;
    println!("Retrieved {} function pointers for compiled regexes from LLIR unit.", pointers_length);
    unsafe { slice::from_raw_parts(getregexpointers(), pointers_length) }
}

fn get_flare_nodes_wrapper<'a>() -> &'a str {
     unsafe { CStr::from_ptr(getflarenodes()) }.to_str().unwrap()
}

// static mut flare_nodes =

fn parse_csv_to_entries_array(path: &str) -> Result<Vec<Vec<String>>, Box<Error>> {
    // Take an initial pass over the CSV to get its dimensions.
    let mut rdr = Reader::from_path(path)?;
    let mut max_x = 0;
    let mut y = 0;
    for record in rdr.records() {
        y += 1;

        let mut x = 0;
        let record_contents = record?;
        for _ in record_contents.iter() {
            x += 1;
        }

        if x > max_x {
            max_x = x;
        }

    }
    let height = y;
    let width = max_x;
    println!("height: {:?}, width: {:?}", height, width);

    // Create an array containing the elements of the CSV.
    let mut rdr = Reader::from_path(path)?;
    let mut entries = vec![vec!["".to_string(); height]; width];
    let mut y = 0;
    for record in rdr.records() {
        let mut x = 0;
        let record_contents = record?;
        for cell in record_contents.iter() {
            entries[x][y] = cell.to_owned();
            x += 1;
        }
        y += 1;
    };
    Ok(entries)
}

#[derive(Clone, Debug)]
struct Cursor<'a> {
    x: usize,
    y: usize,
    matches: Vec<(usize, usize, &'a str)>,
    current_node_id: usize,
}

impl<'a> Cursor<'a> {
    fn new(x: usize, y: usize) -> Cursor<'a> {
        Cursor {
            x: x,
            y: y,
            matches: vec![],
            current_node_id: 0,
        }
    }
}

fn run_flare(path: &str) -> Result<(), Box<Error>> {
    println!("Path: {:?}", path);


    let regexptrs = get_regex_pointers_wrapper();
    println!("starting to print regex evals");
    for regexptr in regexptrs.iter() {
        // println!("The function pointer is {:x}", regexptr);
        let string_to_check = CString::new("42695").unwrap().as_ptr();
        let fptr_result = regexptr(string_to_check);
        println!("result of call on 42695: {:?}", fptr_result);

        let string_to_check = CString::new("asdjkfajskdf").unwrap().as_ptr();
        let fptr_result = regexptr(string_to_check);
        println!("result of call on asdjkfajskdf: {:?}", fptr_result);
    }
    println!("ending printing regex evals");

    let flare_nodes_str = get_flare_nodes_wrapper();
    println!("flare nodes string:\n{}", flare_nodes_str);
    let flare_nodes: Vec<FlareNode> = serde_json::from_str(flare_nodes_str)?;
    println!("flare nodes vec: {:?}", flare_nodes);

    let entries = parse_csv_to_entries_array(path)?;

    let mut cursors: Vec<Cursor>  = vec![];
    for (x, column) in entries.iter().enumerate() {
        for (y, cell) in column.iter().enumerate() {
            cursors.push(Cursor::new(x, y));
        }
    }

    let width = entries.len();
    let height = entries[0].len();
    // println!("height: {}, width: {}", height, width);

    let mut wtr = Writer::from_path("a.csv")?;


    let mut iteration = 0;
    let mut results: Vec<Cursor> = vec![];
    let mut is_done = false;
    while !is_done {
        iteration += 1;
        is_done = true;
        let mut new_cursors: Vec<Cursor> = vec![];
        for mut cursor in cursors {
            // println!("Stepping cursor {:?}", cursor);
            // TODO allow margin
            if cursor.x >= width || cursor.x <= 0 || cursor.y >= height || cursor.y <= 0 {
                // println!("Skipping cursor because it left the bounds of the spreadsheet.");
                continue;
            }
            let entry = &entries[cursor.x][cursor.y];
            // println!("Corresponding sheet entry is {:?}", entry);
            let regex = &regexptrs[cursor.current_node_id];

            let numeric_regex_result = regex(CString::new(entry.clone()).unwrap().as_ptr());
            let is_match = 0 != numeric_regex_result;

            // println!("Match? {:?} (from {})", is_match, numeric_regex_result);
            if !is_match {
                // println!("No match. Skipping to next cursor.\n");
                continue;
            }

            // Record the contents of the cell if this is a capturing Flare node.
            let current_node = &flare_nodes[cursor.current_node_id];
            if current_node.is_capture {
                // println!("Captured cell ({}, {}) with contents {}", cursor.x, cursor.y, entry);
                cursor.matches.push((cursor.x, cursor.y, entry));
            }

            // TODO add spatial check
            if current_node.successors.len() == 0 {
                // println!("Match. No successors. This cursor has reached a final matching state.\n");


                let match_entries: Vec<&str> = cursor.matches.iter().map(|&(x, y, entry)| entry).collect();
                wtr.write_record(match_entries)?;
                wtr.flush()?;

                results.push(cursor);
            } else {
                // println!("Match. {} successors. Generating cursors.", current_node.successors.len());

                // If we have to go to successors, then we're not done
                is_done = false;

                for successor_id in current_node.successors.iter() {
                    // println!("Generating successor cursors that transition to Flare node {}.", successor_id);
                    let mut horizontal_cursors = vec![];
                    let ref successor = flare_nodes[*successor_id];

                    match successor.horizontal_constraint {
                        HorizontalConstraint::NoHorizontal => {
                            let mut horizontal_cursor = cursor.clone();
                            horizontal_cursor.current_node_id = *successor_id;
                            horizontal_cursors.push(horizontal_cursor);
                        },
                        HorizontalConstraint::Right(n) => {
                            // println!("Found a right-{} constraint.", n);
                            let mut horizontal_cursor = cursor.clone();
                            horizontal_cursor.current_node_id = *successor_id;
                            horizontal_cursor.x += n as usize;
                            horizontal_cursors.push(horizontal_cursor);
                        },
                        HorizontalConstraint::Left(n) => {
                            // println!("Found a left-{} constraint.", n);
                            let mut horizontal_cursor = cursor.clone();
                            horizontal_cursor.current_node_id = *successor_id;
                            horizontal_cursor.x -= n as usize;
                            horizontal_cursors.push(horizontal_cursor);
                        },
                        HorizontalConstraint::RightAny => {
                            // println!("Found a right-any constraint.");
                            for new_x in (cursor.x)..width {
                                let mut horizontal_cursor = cursor.clone();
                                horizontal_cursor.current_node_id = *successor_id;
                                horizontal_cursor.x = new_x as usize;
                                horizontal_cursors.push(horizontal_cursor);
                            }
                        },
                        HorizontalConstraint::LeftAny => {
                            // println!("Found a left-any constraint.");
                            for new_x in 0..(cursor.x + 1) {
                                let mut horizontal_cursor = cursor.clone();
                                horizontal_cursor.current_node_id = *successor_id;
                                horizontal_cursor.x = new_x as usize;
                                horizontal_cursors.push(horizontal_cursor);
                            }
                        },
                    }

                    match successor.vertical_constraint {
                        VerticalConstraint::Up(n) => {
                            // println!("Found an up-{} constraint.", n);
                            for mut vertical_cursor in horizontal_cursors {
                                vertical_cursor.y -= n as usize;
                                new_cursors.push(vertical_cursor);
                            }
                        },
                        VerticalConstraint::Down(n) => {
                            // println!("Found a down-{} constraint.", n);
                            for mut vertical_cursor in horizontal_cursors {
                                vertical_cursor.y += n as usize;
                                new_cursors.push(vertical_cursor);
                            }
                        },
                        VerticalConstraint::UpAny => {
                            // println!("Found an up-any constraint.");
                            for horizontal_cursor in horizontal_cursors {
                                for new_y in 0..(cursor.y + 1) {
                                    let mut vertical_cursor = horizontal_cursor.clone();
                                    vertical_cursor.y = new_y as usize;
                                    new_cursors.push(vertical_cursor);
                                }
                            }
                        },
                        VerticalConstraint::DownAny => {
                            // println!("Found a down-any constraint.");
                            for horizontal_cursor in horizontal_cursors {
                                for new_y in (cursor.y)..height {
                                    let mut vertical_cursor = horizontal_cursor.clone();
                                    vertical_cursor.y = new_y as usize;
                                    new_cursors.push(vertical_cursor);
                                }
                            }
                        },
                        VerticalConstraint::NoVertical => {
                            new_cursors.extend(horizontal_cursors);
                        },
                    }

                    // println!("Generated successor cursors {:?}", new_cursors);
                }
            }
        }
        println!("Iteration {} complete. Finished a pass through all the cursors.\n", iteration);
        cursors = new_cursors; // TODO swap instead
    }
    println!("Completed with {} match(es):\n", results.len());
    // for result in &results {
    //     println!("\t{:?}", result);
    // }
    //
    // let mut wtr = Writer::from_path("a.csv")?;
    // for result in results {
    //     let match_entries: Vec<&str> = result.matches.iter().map(|&(x, y, entry)| entry).collect();
    //     wtr.write_record(match_entries)?;
    // }
    // wtr.flush()?;

    Ok(())
}

#[no_mangle]
pub extern fn rustmain(cpath: *const c_char) { // TODO check if we should use extern "system" for 32-bit compat
    // https://doc.rust-lang.org/book/first-edition/ffi.html#foreign-calling-conventions
    let path = unsafe { CStr::from_ptr(cpath) }.to_str().unwrap();
    println!("path from c: {:?}", path);



    // let fnode = FlareNode {
    //     regex: "test".to_string(),
    //     capture: false,
    //     horizontal_constraint: HorizontalConstraint::NoHorizontal,
    //     vertical_constraint: VerticalConstraint::NoVertical,
    // };
    // println!("serialized node: {:?}", serde_json::to_string(&fnode).unwrap());

    if let Err(err) = run_flare(path) {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
