# flarec

Cole Erickson, advised by Professor Dan Barowy

# Usage

Run `flarec.sh` with your Flare program as the sole argument. The Flare program is compiled and written to a binary named `flaresheetmatch`. To use this binary, pass it a single argument: the path to the spreadsheet. The output is stored in `a.csv`.

An example:

```sh
$ ./flarec.sh "</..*/>rr</.*0/>"
$ ./flaresheetmatch test.csv
```

This Flare program captures every pair of cells such that the first cell is nonempty, the second cell is two spaces to the right of the first, and the contents of the second cell end with 0.

For example, the input file `test.csv` has the following contents.
```
city,country,pop
Boston,United States,4628910
Concord,United States,42695
```

The output is:
```
Boston,4628910
```

# Getting a REPL

Run `./utop.sh`.

# Testing

Run `make test`.

# Creating the Development Environment

I've sketched out the steps I used in reproducing my build environment. They are brief, but I believe they are complete. Don't hesitate to let me know if these instructions are unclear or incorrect.

1. Create a virtual machine using the minimal version of Lubuntu 18.04 64-bit in VMWare Player

[Download here](https://lubuntu.net/)

2. Update apt

`sudo apt update`

3. Install git, curl, clang, make, aspcud, m4, cmake, pkg-config

`sudo apt install git curl clang make aspcud m4 cmake pkg-config`

4. Install rust with [rustup](https://rustup.rs/), using default choices when prompted by the install script

5. Install OPAM
`wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin`

6. Downgrade OCaml to 4.04.0 because some packages are not compatible with 4.06.0

`opam switch 4.04.0`

7. Configure OPAM. I give manual instructions here because `opam init` put the following environment variables configuration code in ~/.profile, where it is not by default used in new bash sessions in LUbuntu. I moved it to ~/.bashrc

`echo ". /home/flarec/.opam/opam-init/init.sh > dev/null 2> /dev/null || true" >> ~/.bashrc`

8. Create `~/.ocamlinit` with contents:

```ocaml
let () =
    try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
    with Not_found -> ()
;;
```

9. Open a new terminal

10. Install dependencies from OPAM

`opam install jbuilder core llvm utop ounit`

11. Also install Menhir from OPAM if you can, but I had to use apt

`sudo apt install menhir`

12. Clone the repository

`git clone https://github.com/coleerickson/flarec.git`

13. Enter the repository

`cd flarec`

14. Build, for example, the `flaresheetmatch` target

`make flaresheetmatch`

15. 👍
