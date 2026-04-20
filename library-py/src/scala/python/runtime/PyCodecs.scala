package scala.python.runtime

import scala.python.{PyAny, PyDynamic, extern, name, native}

/** Thin facade over Python's incremental codec machinery.
 *
 *  Wraps the codec-specific `IncrementalDecoder` class so that
 *  `InputStreamReader` can feed byte chunks to the decoder as they
 *  arrive from the underlying stream, without buffering the entire
 *  input first. Partial multi-byte sequences that straddle chunk
 *  boundaries are held inside the decoder until the next chunk
 *  arrives.
 */
object PyCodecs:
  @extern("encodings.utf_8", "IncrementalDecoder")
  final class Utf8IncrementalDecoder extends PyAny:
    /** Decode a chunk of bytes. Pass `finalChunk = true` on the last
     *  call so any partial trailing bytes become the replacement or an
     *  error per the decoder's policy. */
    def decode(data: PyDynamic, finalChunk: Boolean): String = native

    /** Reset decoder state — drops any pending partial sequence. */
    def reset(): Unit = native

  def newUtf8Decoder(): Utf8IncrementalDecoder =
    new Utf8IncrementalDecoder()
