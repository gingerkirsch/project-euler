/** The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ? */

val threshold: Long = 13195

def stream(i: Long = 2): Stream[Long] = i #:: stream(i + 1)
def range(threshold: Long): Stream[Long] = stream().takeWhile(_ <= threshold)
def isPrime(number: Long): Boolean = range(number).filter(number % _ == 0).size == 1

def sieve(number: Long, factors: Stream[Long]): Either[String, Long] =
  factors.headOption match {
    case None => Left("no factors to check!")
    case Some(head) if (number % head == 0) => Right(head)
    case _ => sieve(number, factors.tail)
  }

def largestPrime(number: Long): Either[String,Long] = {
  number match {
    case n if isPrime(n) => Right(n)
    case n => sieve(n, range(scala.math.sqrt(n).toLong).filter(isPrime(_)).reverse)
  }
}

val result = largestPrime(threshold)
