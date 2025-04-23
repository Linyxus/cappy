struct Point(x: i32, y: i32)
struct Foo(var x: i32, y: Point)
struct Ok(result: i32)
struct Error(err: String)
