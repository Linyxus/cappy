# Arena Allocation

This is to introduce arena allocations into Cavia.

## Arena

An arena is a piece of managed memory on which `struct`s can be allocated. An arena is lexically-scoped: it is always introduced by the following primitive operation:
```scala
arena: (z: Arena^) =>
  ...
```
`z` lives as long as this closure passed to the `arena` function. Structs are bump-allocated on an arena. At the end of an arena, all allocated structs can be deallocated in one go. The arena handle `z: Arena^` shall never escape the scope; so are struct references allocated on this arena.

## Arena Allocation

Arena allocation is possible through an arena handle, for instance:
```scala
struct Pair(x: i32, y: i32)
z.Pair(0, 0)
```
The returned type should be something like
```
Ar[Pair]^{z.fresh}
```
`z.fresh` is a "selection capability": something that is fresh but part of `z`. Different sub-`fresh` instances from the same origin are distinct. It is like siblings of the same parent in a tree-like structure. `Ar` is the marker trait for arena-allocated structs. "Ar" is the abbreviation of _A_rena _r_eference.

An `Ar[Pair]` should be used just like a `Pair`: selection, assignment and pattern matching should work the same.
