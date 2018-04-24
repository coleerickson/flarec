* Move AST definition to its own .ml file
* Rewrite output to output as Lisp -- as a tree
* Graphviz loop

March 5:
* Actually match NFAs
* Complete parsing with question marks
* Flesh out test suite
* Write a graphviz loop (bucklescript or webbrowser)
* Rename start/final

March 12:
* Provide build information, dependencies
* Push-forward
* Regular expression compiler
* Fix warnings

What I'm trying to get done on March 31:
* Instructions on setting up devenv (OPAM, etc)
* Improve test suite
* NFA -> DFA conversion
* (DFA minimization)
* Assembly generation


* Improve syntax error when parsing regex
* Add character classes
* Can a Kleene algebra with tests convert to an NFA (Kozen, Arjun Guha)

April 16:
* Read a CSV
* Match regex against every cell in CSV
* Parse Flare programs

April 24:
* Parse (some) Flare programs
* Execute at least one Flare program

Implementation note. If all regexes in a Flare program exclude the empty string (i.e., we never match on blank cells in the spreadsheet), then the maximum search distance for the `*` quantifier is the greatest (Manhattan) distance between any two non-empty cells. This constraint can be relaxed if you want to allow some regexes to match empty cells (though why would you?). For example, if you want to allow a single regex in the Flare expression to be empty, then the search distance

Dynamic programming optimization. Memoize regex evaluations. Then asymptotic time complexity is O(sum of length of all regexes * size of sheet + number of matches * size of spreadsheet ^ (height of flare tree)), which seems like the best you can really hope for and still have a complete solution. I know number of matches is not really a valid variable (not known beforehand), but I'd guess most users will be satisfied with linearity in number of matches. Kleene stars on both the horizontal and vertical are the worst thing for run time.  Practical performance issues will be left to heuristics, I suppose.
