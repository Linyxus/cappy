package java.math

import java.util.Random

private[math] object Primality:
  private val SmallPrimeInts =
    Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)

  private def two: BigInteger = BigInteger.TWO
  private def three: BigInteger = BigInteger.valueOf(3)

  def isProbablePrime(value: BigInteger, certainty: Int): Boolean =
    if certainty <= 0 then
      return true
    if value.signum() <= 0 then
      return false
    if value.equals(two) || value.equals(three) then
      return true
    if !value.testBit(0) || value.equals(BigInteger.ONE) then
      return false

    var i = 0
    while i < SmallPrimeInts.length do
      val prime = BigInteger.valueOf(SmallPrimeInts(i).toLong)
      if value.equals(prime) then
        return true
      if value.mod(prime).signum() == 0 then
        return false
      i += 1

    val nMinusOne = value.subtract(BigInteger.ONE)
    var d = nMinusOne
    var s = 0
    while !d.testBit(0) do
      d = d.shiftRight(1)
      s += 1

    val rounds = java.lang.Math.max(1, (certainty + 1) / 2)
    i = 0
    while i < rounds do
      val base = witnessBase(value, i)
      var x = base.modPow(d, value)
      if !x.equals(BigInteger.ONE) && !x.equals(nMinusOne) then
        var witness = true
        var j = 1
        while j < s && witness do
          x = x.modPow(two, value)
          if x.equals(nMinusOne) then
            witness = false
          j += 1
        if witness then
          return false
      i += 1
    true

  def probablePrime(bitLength: Int, certainty: Int, rnd: Random): BigInteger =
    if bitLength < 2 then
      throw new ArithmeticException("bitLength < 2")

    var candidate = randomCandidate(bitLength, rnd)
    while !isProbablePrime(candidate, java.lang.Math.max(certainty, 40)) do
      candidate = candidate.add(two)
      if candidate.bitLength() != bitLength then
        candidate = randomCandidate(bitLength, rnd)
    candidate

  def nextProbablePrime(start: BigInteger): BigInteger =
    if start.compareTo(two) < 0 then
      return two

    var candidate =
      if start.testBit(0) then start.add(two)
      else start.add(BigInteger.ONE)

    while !isProbablePrime(candidate, 80) do
      candidate = candidate.add(two)
    candidate

  private def witnessBase(value: BigInteger, round: Int): BigInteger =
    val limit = value.subtract(two)
    val raw = BigInteger.valueOf(SmallPrimeInts(round % SmallPrimeInts.length).toLong)
    if raw.compareTo(limit) <= 0 then raw
    else
      raw.mod(value.subtract(three)).add(two)

  private def randomCandidate(bitLength: Int, rnd: Random): BigInteger =
    val topBit = bitLength - 1
    new BigInteger(bitLength, rnd).setBit(topBit).setBit(0)
