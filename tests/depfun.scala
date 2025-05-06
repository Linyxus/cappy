struct IO()
type Fun[-A, +B] = (z: A) -> B
def bad(consume z: IO^): Fun[IO^, IO^] =
  (c: IO^) => c
