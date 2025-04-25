# Capture Checking in Cavia

## Contextual Separation

A distinct challenge in the implementation that we will not face in the calculus is that the separation guarantees now can be "stored" by struct definitions. For instance,
```cavia
struct P[C1, C2](a: Op^{C1}, b: Op^{C2})
```
When constructing a `P`, the type arguments `C1` and `C2` are guaranteed to be separate. When getting a `P[C1, C2]^{C1,C2}` in the context, we can of course use the two fields in a non-interfering way.

An equivalent struct definition in the language is
```cavia
struct P(a: Op^{cap}, b: Op^{cap})
```
Here, instead of explicit capture polymorphism, the lightweight notation is used.
