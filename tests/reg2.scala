struct Ref:
  var data: i32
def main(): Unit =
  arena: ar =>
    val r1 = ar.Ref(0)
    val r2: Ar[Ref]^{r1} = r1
    val r3: Ar[Ref]^{ar} = r2
    //val r3: Ar[Ref]^{ar} = r2
    ()
