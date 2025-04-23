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
- [ ] Support `class` definitions

## Issues

- [ ] No negative tests for parsing
- [ ] Parser needs to be refactored: soft or hard failure
- [ ] (!!!) two kinds of capture parameters? (@use vs normal), use spine capture set
