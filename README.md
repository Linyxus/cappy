# cavia

Experimental compiler for System Capybara.

Features:
- Scala-like syntax
- Capture tracking and therefore ownership and borrow checking
- Using WebAssembly as the target language
- Safe yet versatile!

This is currently a toy.

## Roadmap

- [x] Add tests for type checking
- [x] Support comments `//`
- [x] Handle capture sets for DefDef
- [x] A first prototype of code generation
- [x] Handle self-recursion in local bindings
- [x] Parsing improvement: T -> U, and (T1, T2) -> U
- [x] Parsing improvement: IO^ should be IO^{cap}
- [x] Parsing improvement: `A => B` should be `A ->{cap} B`
- [x] Support `struct` definitions
- [x] Support path expressions, reading and writing fields
- [x] Support translating module-level definitions
- [x] Support translating `struct` definitions
- [x] Support operator parsing
- [x] Support `if` expressions
- [x] Standardise assignment: translate to primitive ops
- [x] Support primitive array types
- [x] Support generic structs
- [x] Instantiate `cap`s
- [x] Handle struct captures more precisely
- [ ] Support extension methods
- [ ] Add boxed int type
- [ ] Support array literal
- [ ] Support `char`
- [ ] Support translating type lambdas

## Issues

- [ ] No negative tests for parsing
- [ ] Parser needs to be refactored: soft or hard failure
- [ ] (!!!) two kinds of capture parameters? (@use vs normal), use spine capture set
- [ ] Autoboxing: automatically boxing and unboxing value types
