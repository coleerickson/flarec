# This file is just an adaptation of a portion of the Makefile into a bash script so that it can be used

# flarec
echo "Building the flarec compiler..."
jbuilder build src/flarec/flarec.exe && \
mkdir -p bin && \
cp _build/default/src/flarec/flarec.exe bin/flarec.exe

# flarellunit.ll
echo "Compiling the Flare program to LLIR..."
bin/flarec.exe $1 flarellunit.ll && \
(cat flarellunit.ll | grep -v source_filename) > flarellunit.ll.tmp && \
  mv flarellunit.ll.tmp flarellunit.ll

# flarellunit.o
echo "Compiling the Flare program to object code..."
clang -g -O0 -c flarellunit.ll

# cmain.o
echo "Compiling the C startup unit to object code..."
clang -g -O0 -c cmain.c

# rustlib
echo "Compiling the Rust runtime library..."
(cd rust-flarec && \
cargo build)

# flaresheetmatch
echo "Linking all units into \"flaresheetmatch\"..."
clang -o flaresheetmatch cmain.o flarellunit.o ./rust-flarec/target/debug/librustflareclib.so && \
echo "Success!"
