import scala.python.runtime.PyBuiltins

@main def pyfacadeBuiltins(): Unit =
  // --- Numeric formatting -------------------------------------------
  println("hex-255:" + PyBuiltins.hex_of(255))
  println("hex-0:" + PyBuiltins.hex_of(0))
  println("hex-16:" + PyBuiltins.hex_of(16))
  println("hex-neg-1:" + PyBuiltins.hex_of(-1))
  println("hex-neg-255:" + PyBuiltins.hex_of(-255))
  println("hex-large:" + PyBuiltins.hex_of(0xCAFEBABEL))

  println("oct-0:" + PyBuiltins.oct_of(0))
  println("oct-8:" + PyBuiltins.oct_of(8))
  println("oct-64:" + PyBuiltins.oct_of(64))
  println("oct-neg-8:" + PyBuiltins.oct_of(-8))

  println("bin-0:" + PyBuiltins.bin_of(0))
  println("bin-1:" + PyBuiltins.bin_of(1))
  println("bin-5:" + PyBuiltins.bin_of(5))
  println("bin-255:" + PyBuiltins.bin_of(255))
  println("bin-neg-5:" + PyBuiltins.bin_of(-5))

  // --- chr / ord -----------------------------------------------------
  println("chr-65:" + PyBuiltins.chr_of(65))
  println("chr-97:" + PyBuiltins.chr_of(97))
  println("chr-48:" + PyBuiltins.chr_of(48))
  println("chr-32:" + PyBuiltins.chr_of(32))
  println("chr-9731:" + PyBuiltins.chr_of(9731)) // snowman U+2603
  println("ord-A:" + PyBuiltins.ord_of("A"))
  println("ord-a:" + PyBuiltins.ord_of("a"))
  println("ord-0:" + PyBuiltins.ord_of("0"))
  println("ord-newline:" + PyBuiltins.ord_of("\n"))
  println("chr-ord-rt:" + (PyBuiltins.chr_of(PyBuiltins.ord_of("Z")) == "Z"))

  // --- Parsing -------------------------------------------------------
  println("int-0:" + PyBuiltins.int_parse("0"))
  println("int-42:" + PyBuiltins.int_parse("42"))
  println("int-neg-7:" + PyBuiltins.int_parse("-7"))
  println("int-hex-ff:" + PyBuiltins.int_parse("ff", 16))
  println("int-hex-FF:" + PyBuiltins.int_parse("FF", 16))
  println("int-bin-101:" + PyBuiltins.int_parse("101", 2))
  println("int-oct-777:" + PyBuiltins.int_parse("777", 8))
  println("int-radix-36:" + PyBuiltins.int_parse("ZZ", 36))
  println("int-neg-hex:" + PyBuiltins.int_parse("-ff", 16))
  println("int-prefixed-base-0:" + PyBuiltins.int_parse("0x2A", 0))

  println("float-1.5:" + PyBuiltins.float_parse("1.5"))
  println("float-neg-3.14:" + PyBuiltins.float_parse("-3.14"))
  println("float-sci:" + PyBuiltins.float_parse("1e3"))
  val nanParsed = PyBuiltins.float_parse("nan")
  println("float-nan-is-nan:" + (nanParsed != nanParsed))
  println("float-inf:" + PyBuiltins.float_parse("inf"))

  // --- Numeric ops ---------------------------------------------------
  println("abs-int-neg-5:" + PyBuiltins.abs_int(-5))
  println("abs-int-0:" + PyBuiltins.abs_int(0))
  println("abs-long-neg-large:" + PyBuiltins.abs_long(-9000000000L))
  println("abs-double-neg-3.5:" + PyBuiltins.abs_double(-3.5))
  println("min-int-3-5:" + PyBuiltins.min_int(3, 5))
  println("min-int-neg-3-neg-5:" + PyBuiltins.min_int(-3, -5))
  println("max-int-3-5:" + PyBuiltins.max_int(3, 5))
  println("max-long:" + PyBuiltins.max_long(1000000L, 999999L))
  println("min-double-1-2:" + PyBuiltins.min_double(1.5, 2.5))
  println("max-double-1-2:" + PyBuiltins.max_double(1.5, 2.5))
  println("round-bankers-0.5:" + PyBuiltins.round_to_long(0.5))  // Python: 0 (banker's)
  println("round-bankers-1.5:" + PyBuiltins.round_to_long(1.5))  // Python: 2
  println("round-bankers-2.5:" + PyBuiltins.round_to_long(2.5))  // Python: 2 (banker's)
  println("round-bankers-neg-0.5:" + PyBuiltins.round_to_long(-0.5))  // Python: 0
  println("round-bankers-3.7:" + PyBuiltins.round_to_long(3.7))  // Python: 4
  println("round-digits:" + PyBuiltins.round_double(3.14159, 2))

  // --- len -----------------------------------------------------------
  println("len-empty:" + PyBuiltins.length_of(""))
  println("len-str:" + PyBuiltins.length_of("hello"))

  // --- str predicates ------------------------------------------------
  println("is-alpha-a:" + PyBuiltins.is_alpha("a"))
  println("is-alpha-A:" + PyBuiltins.is_alpha("A"))
  println("is-alpha-hello:" + PyBuiltins.is_alpha("hello"))
  println("is-alpha-1:" + PyBuiltins.is_alpha("1"))
  println("is-alpha-mix:" + PyBuiltins.is_alpha("abc123"))
  println("is-alpha-empty:" + PyBuiltins.is_alpha(""))

  println("is-digit-7:" + PyBuiltins.is_digit("7"))
  println("is-digit-123:" + PyBuiltins.is_digit("123"))
  println("is-digit-1a:" + PyBuiltins.is_digit("1a"))
  println("is-digit-neg:" + PyBuiltins.is_digit("-1"))

  println("is-alnum-hello7:" + PyBuiltins.is_alnum("hello7"))
  println("is-alnum-hi!:" + PyBuiltins.is_alnum("hi!"))

  println("is-space-space:" + PyBuiltins.is_space(" "))
  println("is-space-tab:" + PyBuiltins.is_space("\t"))
  println("is-space-newline:" + PyBuiltins.is_space("\n"))
  println("is-space-a:" + PyBuiltins.is_space("a"))

  println("is-upper-ABC:" + PyBuiltins.is_upper("ABC"))
  println("is-upper-Abc:" + PyBuiltins.is_upper("Abc"))
  println("is-lower-abc:" + PyBuiltins.is_lower("abc"))
  println("is-lower-abC:" + PyBuiltins.is_lower("abC"))

  println("is-ascii-abc:" + PyBuiltins.is_ascii("abc"))
  println("is-ascii-snowman:" + PyBuiltins.is_ascii(PyBuiltins.chr_of(9731)))

  println("is-decimal-123:" + PyBuiltins.is_decimal("123"))
  println("is-decimal-1.5:" + PyBuiltins.is_decimal("1.5"))
  println("is-numeric-123:" + PyBuiltins.is_numeric("123"))
  println("is-printable-abc:" + PyBuiltins.is_printable("abc"))
  println("is-printable-newline:" + PyBuiltins.is_printable("a\n"))

  // --- str transforms -----------------------------------------------
  println("upper-hello:" + PyBuiltins.to_upper("hello"))
  println("upper-mixed:" + PyBuiltins.to_upper("Hello World"))
  println("lower-WORLD:" + PyBuiltins.to_lower("WORLD"))
  println("lower-MiXeD:" + PyBuiltins.to_lower("MiXeD"))
  println("swapcase:" + PyBuiltins.swap_case("Hello World"))
  println("capitalize-hello:" + PyBuiltins.capitalize("hello world"))
  println("capitalize-HELLO:" + PyBuiltins.capitalize("HELLO"))
  println("title-hello:" + PyBuiltins.title_case("hello world"))

  println("strip-pad:" + PyBuiltins.strip("  hello  "))
  println("strip-tabs:" + PyBuiltins.strip("\t\thi\n"))
  println("lstrip:" + PyBuiltins.lstrip("  hello  "))
  println("rstrip:" + PyBuiltins.rstrip("  hello  "))

  // --- encode --------------------------------------------------------
  val encoded = PyBuiltins.encode("A", "utf-8")
  println("encode-len:" + PyBuiltins.length_of(encoded))
