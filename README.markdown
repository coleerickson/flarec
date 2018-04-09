# flarec (in development)

To try out regex matching, run

```sh
$ make
$ bin/matchRegex.exe "/ab*/" abbbbb
```

To use `utop`, the principal OCaml REPL, use `jbuilder repl`. The Flare library, located at `src/flare/`, and defined by `src/flare/jbuild`, is the library that contains most of the code shared by the different executables in this project. For example, to match a regular expression in `utop`:

```sh 
$ make
$ jbuilder utop src/flare
utop # open Flare.ParseTools;;
utop # match_regex "/(a|b)*/" "abbbbb";;
- : bool = true
```
