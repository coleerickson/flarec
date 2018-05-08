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

use std::collections::VecDeque;

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
    description: String,
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
    x: i32,
    y: i32,
    matches: Vec<(i32, i32, &'a str)>,
    current_node_id: usize,
    revisitation_points: VecDeque<(i32, i32, usize)>,
}

impl<'a> Cursor<'a> {
    fn new(x: i32, y: i32) -> Cursor<'a> {
        Cursor {
            x: x,
            y: y,
            matches: vec![],
            current_node_id: 0,
            revisitation_points: VecDeque::new(),
        }
    }
}

// TODO investigate why cursor should have anything to do with lifetime of returned value
fn spatially_expand<'a>(cursor: &Cursor<'a>, successor_id: usize, flare_nodes: &Vec<FlareNode>, width: i32, height: i32) -> Vec<Cursor<'a>> {
    let mut horizontal_cursors = vec![];
    let ref successor = flare_nodes[successor_id];
    println!("Performing spatial expanding for node {} using its constraints {:?}, {:?}", successor_id, successor.horizontal_constraint, successor.vertical_constraint);

    match successor.horizontal_constraint {
        HorizontalConstraint::NoHorizontal => {
            let mut horizontal_cursor = cursor.clone();
            horizontal_cursor.current_node_id = successor_id;
            horizontal_cursors.push(horizontal_cursor);
        },
        HorizontalConstraint::Right(n) => {
            // println!("Found a right-{} constraint.", n);
            let mut horizontal_cursor = cursor.clone();
            horizontal_cursor.current_node_id = successor_id;
            horizontal_cursor.x += n;
            horizontal_cursors.push(horizontal_cursor);
        },
        HorizontalConstraint::Left(n) => {
            // println!("Found a left-{} constraint.", n);
            let mut horizontal_cursor = cursor.clone();
            horizontal_cursor.current_node_id = successor_id;
            horizontal_cursor.x -= n;
            horizontal_cursors.push(horizontal_cursor);
        },
        HorizontalConstraint::RightAny => {
            // println!("Found a right-any constraint.");
            for new_x in (cursor.x)..(width) {
                let mut horizontal_cursor = cursor.clone();
                horizontal_cursor.current_node_id = successor_id;
                horizontal_cursor.x = new_x;
                horizontal_cursors.push(horizontal_cursor);
            }
        },
        HorizontalConstraint::LeftAny => {
            // println!("Found a left-any constraint.");
            for new_x in 0..(cursor.x + 1) {
                let mut horizontal_cursor = cursor.clone();
                horizontal_cursor.current_node_id = successor_id;
                horizontal_cursor.x = new_x;
                horizontal_cursors.push(horizontal_cursor);
            }
        },
    }

    let mut vertical_cursors = vec![];
    match successor.vertical_constraint {
        VerticalConstraint::Up(n) => {
            // println!("Found an up-{} constraint.", n);
            for mut vertical_cursor in horizontal_cursors {
                vertical_cursor.y -= n;
                vertical_cursors.push(vertical_cursor);
            }
        },
        VerticalConstraint::Down(n) => {
            // println!("Found a down-{} constraint.", n);
            for mut vertical_cursor in horizontal_cursors {
                vertical_cursor.y += n;
                vertical_cursors.push(vertical_cursor);
            }
        },
        VerticalConstraint::UpAny => {
            // println!("Found an up-any constraint.");
            for horizontal_cursor in horizontal_cursors {
                for new_y in 0..(cursor.y + 1) {
                    let mut vertical_cursor = horizontal_cursor.clone();
                    vertical_cursor.y = new_y;
                    vertical_cursors.push(vertical_cursor);
                }
            }
        },
        VerticalConstraint::DownAny => {
            // println!("Found a down-any constraint.");
            for horizontal_cursor in horizontal_cursors {
                for new_y in (cursor.y)..(height) {
                    let mut vertical_cursor = horizontal_cursor.clone();
                    vertical_cursor.y = new_y;
                    vertical_cursors.push(vertical_cursor);
                }
            }
        },
        VerticalConstraint::NoVertical => {
            vertical_cursors.extend(horizontal_cursors);
        },
    }

    println!("Spatially expanded to retrieve {} new cursors: {:?}", vertical_cursors.len(), vertical_cursors);
    vertical_cursors
}

fn run_flare(path: &str) -> Result<(), Box<Error>> {
    println!("Path: {:?}", path);


    let regexptrs = get_regex_pointers_wrapper();
    println!("starting to print regex evals");
    for regexptr in regexptrs.iter() {
        // println!("The function pointer is {:x}", regexptr);
        let fptr_result = regexptr(CString::new("hi").unwrap().as_ptr());
        println!("result of call on hi: {:?}", fptr_result);

        let fptr_result = regexptr(CString::new("hey").unwrap().as_ptr());
        println!("result of call on hey: {:?}", fptr_result);

        let fptr_result = regexptr(CString::new("hello").unwrap().as_ptr());
        println!("result of call on hello: {:?}", fptr_result);
    }
    println!("ending printing regex evals");

    let flare_nodes_str = get_flare_nodes_wrapper();
    println!("flare nodes string:\n{}", flare_nodes_str);
    let flare_nodes: Vec<FlareNode> = serde_json::from_str(flare_nodes_str)?;
    println!("flare nodes vec: {:?}", flare_nodes);

    let entries = parse_csv_to_entries_array(path)?;

    let mut cursors: Vec<Cursor>  = vec![];
    for (x, column) in entries.iter().enumerate() {
        for (y, _) in column.iter().enumerate() {
            cursors.push(Cursor::new(x as i32, y as i32));
        }
    }

    let width = entries.len() as i32;
    let height = entries[0].len() as i32;
    // println!("height: {}, width: {}", height, width);

    let mut wtr = Writer::from_path("a.csv")?;

    // TODO verify that we don't allow convergence after alternations... e.g., the "r/cole/" in /hey/[r/hi/,d/hello/]r/cole/

    let mut iteration = 0;
    let mut results: Vec<Cursor> = vec![];
    let mut is_done = false;
    while !is_done {
        iteration += 1;
        is_done = true;
        let mut new_cursors: Vec<Cursor> = vec![];
        for mut cursor in cursors {
            println!("Stepping cursor {:?}", cursor);
            // TODO allow margin


            let current_node = &flare_nodes[cursor.current_node_id];
            let expanded_cursors: Vec<Cursor> = spatially_expand(&cursor, cursor.current_node_id, &flare_nodes, width, height);

            let prev_len = expanded_cursors.len();
            let mut expanded_cursors: Vec<Cursor> = expanded_cursors.into_iter().filter(|ref mut c| {
                if c.x >= width || c.y >= height || c.x < 0 || c.y < 0 {
                    println!("Skipping cursor because it left the bounds of the spreadsheet.");
                    return false;
                }

                let entry = &entries[c.x as usize][c.y as usize];
                println!("Corresponding sheet entry is {:?}", entry);
                let regex = &regexptrs[c.current_node_id];

                let numeric_regex_result = regex(CString::new(entry.clone()).unwrap().as_ptr());
                let is_match = 0 != numeric_regex_result;

                is_match
            }).collect();
            let new_len = expanded_cursors.len();
            println!("After matching against the regex, we got rid of {} cursors ({} -> {})", prev_len - new_len, prev_len, new_len);

            // NOTE Match first semantics here. TODO build this into the syntax.
            // expanded_cursors.sort_unstable_by(|ref a, ref b| {
            //
            //     let ordering = ((b.x + b.y) - (cursor.x + cursor.y)).cmp(
            //         &((a.x + a.y) - (cursor.x + cursor.y))
            //     ).then((b.x - cursor.x).cmp(&(a.x - cursor.x)));
            //     println!("{:?} vs {:?} --> {:?}", a, b, ordering);
            //     ordering
            // });
            //
            // let mut first_match_vector = vec![];
            // match expanded_cursors.first() {
            //     Some(first_match) => { first_match_vector.push(first_match.clone()); },
            //     _ => {},
            // }
            // let mut expanded_cursors = first_match_vector;

            for ref mut c in &mut expanded_cursors {
                // Record the contents of the cell if this is a capturing Flare node.
                if (&flare_nodes[c.current_node_id]).is_capture {
                    let entry = &entries[c.x as usize][c.y as usize];
                    println!("Captured cell ({}, {}) with contents {}", c.x, c.y, entry);
                    c.matches.push((c.x, c.y, entry));
                }

                for successor_id in &current_node.successors {
                    c.revisitation_points.push_back((c.x, c.y, *successor_id));
                }
            }

            for ref mut c in &mut expanded_cursors {
                if c.revisitation_points.len() == 0 {
                    println!("Match. No successors. This cursor has reached a final matching state.\n");
                    let match_entries: Vec<&str> = cursor.matches.iter().map(|&(_, _, entry)| entry).collect();
                    wtr.write_record(match_entries)?;
                    wtr.flush()?;

                    results.push(c.clone());
                } else {
                    // If we created cursors, then we're not done
                    is_done = false;

                    let (revisit_x, revisit_y, revisit_node_id) = c.revisitation_points.pop_front().unwrap();
                    println!("No successors at node {}, so we'll go back to ({}, {}) and check node {}", c.current_node_id, revisit_x, revisit_y, revisit_node_id);
                    c.x = revisit_x;
                    c.y = revisit_y;
                    c.current_node_id = revisit_node_id;
                    println!("Added cursor {:?}\n", c);
                    new_cursors.push(c.clone());
                }
            }

            // new_cursors.extend(expanded_cursors);
        }
        println!("Iteration {} complete. Finished a pass through all the cursors.\n", iteration);
        cursors = new_cursors; // TODO swap instead
    }
    println!("Completed with {} match(es):\n", results.len());
    for result in &results {
        println!("\t{:?}", result);
    }
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
