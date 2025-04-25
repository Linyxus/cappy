# Capture Checking in Cavia

## Contextual Separation

A distinct challenge in the implementation that we will not face in the calculus is that the separation guarantees now can be "stored" by struct definitions. For instance,
```cavia
struct P[C1, C2](a: Op^{C1}, b: Op^{C2})
```
When constructing a `P`, the type arguments `C1` and `C2` are guaranteed to be separate. When getting a `P[C1, C2]^{C1,C2,C3}` in the context, we can of course use the two fields in a non-interfering way.

An equivalent struct definition in the language is
```cavia
struct P(a: Op^{cap}, b: Op^{cap})
```
Here, instead of explicit capture polymorphism, the lightweight notation is used. Then, given a `P^{cap}` in the context, what do we know about its fields?

Intuitively, if a function takes an argument `p: (a: Op^{cap}, b: Op^{cap})^{cap}` (here, the definition is expanded for clarity), what it really means is that it takes `p: (a: Op^{cap$1}, b: Op^{cap$2})^{cap$3}`, where `cap$3` is separate from the rest of the world, and `cap$1` and `cap$2` are two separate "parts" of `cap$3`.

We should get:
- `cap$1` and `cap$2` are separate;
- `{cap$1, cap$2} <: {cap$3}`.

The relation between these three `cap`s are a relation of parts-and-whole: what we will get from composite data types.
