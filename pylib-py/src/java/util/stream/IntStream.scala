package java.util.stream

/** Link-time stubs for `java.util.stream.{Stream, IntStream, LongStream, DoubleStream}`.
 *  Stdlib's `CharSequence.chars/codePoints` and collection `.stream()` paths
 *  reference these. No pos-py test actually pipelines through streams. */
trait Stream[T]

trait BaseStream[T, S]

trait IntStream extends BaseStream[java.lang.Integer, IntStream]

trait LongStream extends BaseStream[java.lang.Long, LongStream]

trait DoubleStream extends BaseStream[java.lang.Double, DoubleStream]
