val CHUNK_SIZE: i32 = 128
struct Vec(var pool: array[i32]^, var size: i32)
def newVec: Vec^ =
  Vec(newArray(CHUNK_SIZE, 0), 0)

extension (self: array[i32]^)
  def copyTo(other: array[i32]^): Unit =
    def recur(idx: i32): Unit =
      if idx < self.size then
        other(idx) = self(idx)
        recur(idx+1)
    recur(0)

extension (self: Vec^)
  def grow(): Unit =
    val newPoolSize = self.pool.size + CHUNK_SIZE
    val newPool = newArray(newPoolSize, 0)
    //self.pool = newPool  // error

def main(): Unit =
  ()
