#include "stdio.h"

// Inspired by https://github.com/shadowmint/rust-externcl
extern void rustmain(char*);
int main(int argc, char* argv[]) {
    printf("Entering the main function of cmain.c\n");
    if (argc < 2) {
        printf("You must enter a command line argument indicating the path to the spreadsheet.\n");
        return 1;
    }
    printf("From c: argv[1] is \"%s\"\n", argv[1]);
    rustmain(argv[1]);
    return 0;
}
