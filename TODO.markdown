* Move AST definition to its own .ml file
* Rewrite output to output as Lisp -- as a tree
* Test suite
* Week-by-week timeline
* For evaluator of NFAs: favor one path for efficiency
* Implement NFA transformation
* Use OCamlGraph for visualization of automata
* Remove extraneous states
* Graphviz loop

What I'm trying to get done on March 31:
* Instructions for building
	* Clean up directory structure
	* Switch to using jbuilder
	* Fix tests
* NFA -> DFA conversion
* (DFA minimization)
* Assembly generation

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
