# RustRegex
A regex compiler written in Rust

This is my first Rust project, and as such is simply an exercise in learning. It is also an exercise to learn a little about compilers. As such it does not use an FSM structure, but tokenises a regex string, then parses into an AST.

I have tried to write lots of tests, taking advantage of Rust's build in test framework. Let me know if you find an example that does not work, and I may add it to the testsuite.

Currently does not support non capture groups or the | operator
