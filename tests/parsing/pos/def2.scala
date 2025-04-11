def foo[cap C](ops: List[() ->{C} Unit]): Unit = test
def bar[cap C, X](comps: List[() ->{C} X]): List[X] = test
def baz[cap C, cap D, A, B](f: (z: A) ->{C} B, g: (z: B) ->{D} A) = test