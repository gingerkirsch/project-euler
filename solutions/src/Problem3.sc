import scala.annotation.tailrec

/** The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ? */

val threshold: Long = 600851475143L

def factorize(number: Long): List[Long] = {
  @tailrec
  def loop(number: Long, candidate: Long = 2, factors: List[Long] = Nil): List[Long] = candidate * candidate > number match {
    case true => number :: factors
    case _ if number % candidate == 0 => loop(number / candidate, candidate, candidate :: factors)
    case _ => loop(number, candidate + 1, factors)
  }
  loop(number)
}

factorize(threshold).head