package scala_solutions

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class InfiniteDigitalStringTest extends AnyFlatSpec {

  validate("LFM's Scala Solution", scala_solutions.InfiniteDigitalString.indexOf)
  validate("B4B's Java Solution", b4b_solution.InfiniteDigitalString.findPosition)
  validate("LFM's Java Solution", java_solutions.InfiniteDigitalString.findPosition)

  def validate(scenario: String, solution: String => Long): Unit = {

    behavior.of(s"[$scenario] Fixed Inputs")
    Map(
      "456"             -> 3L,
      "454"             -> 79L,
      "455"             -> 98L,
      "910"             -> 8L,
      "9100"            -> 188L,
      "99100"           -> 187L,
      "00101"           -> 190L,
      "001"             -> 190L,
      "00"              -> 190L,
      "123456789"       -> 0L,
      "1234567891"      -> 0L,
      "123456798"       -> 1000000071L,
      "10"              -> 9L,
      "53635"           -> 13034L,
      "040"             -> 1091L,
      "11"              -> 11L,
      "99"              -> 168L,
      "667"             -> 122L,
      "0404"            -> 15050L,
      "949225100"       -> 382689688L,
      "58257860625"     -> 24674951477L,
      "3999589058124"   -> 6957586376885L,
      "555899959741198" -> 1686722738828503L,
      "01"              -> 10L,
      "091"             -> 170L,
      "0910"            -> 2927L,
      "0991"            -> 2617L,
      "09910"           -> 2617L,
      "09991"           -> 35286L,
      "9"               -> 8L,
      "068"             -> 1931L,
      "245116144124"    -> 10503301346L,
      "05706"           -> 2005L
    ).foreach { case (number, expected) => test(number, expected) }

    randomTest(s"[$scenario] Short Random Inputs (length 2 to 9)", randomString(2, 9))
    randomTest(s"[$scenario] Long Random Inputs (length 10 to 15)", randomString(10, 15))

    def randomTest(name: String, rng: => String): Unit = {
      behavior.of(name)
      (0 to 400)
        .map(_ => rng)
        .distinct
        .foreach(number => test(number, Solution.indexOf(number)))
    }

    def test(number: String, expected: Long): Unit = {
      val actual = solution(number)
      it should s"find index($number)" in assert(actual === expected, s"Position of $number SHOULD BE $expected, but WAS $actual")
    }

    def randomString(minInclusive: Int, maxInclusive: Int): String =
      (0 to Random.between(minInclusive, maxInclusive + 1)).map(_ => Random.between(0, 10)).mkString

    object Solution {

      import scala.annotation.tailrec

      /** Finds the position of `n` in the Infinite Digital String 1234567891011...
        *
        * @param n
        *   The string to find
        */
      def indexOf(n: String): Long = {

        /* Cycles through all 'rotations' of n, ignoring rotations that start with '0'.
         * When rotating, tries to 'squish' same digits at the ends of the string.
         * Example: when rotating abcwxyzabc, we form yzabcwx, and not yzabcabcwx
         */
        val cycle =
          (1 until n.length)
            .map(n.splitAt)
            .filterNot(_._2.startsWith("0"))
            .map { case (left, right) =>
              val squish =
                (math.min(left.length, right.length) until 0 by -1)
                  .find { i =>
                    val (overlap, rest) = left.splitAt(i)
                    overlap == right.takeRight(i) && rest.exists(_ != '9')
                  }
                  .getOrElse(0)
              /*
               * For each such rotation, find the index
               * ...yzabcwx|yzabcw(x+1)...
               *      ^ abcwxyzabc
               */
              indexOf(s"$right${left.drop(squish)}".toLong) + right.length - squish
            }

        /** Tries to find a number `start` that is `check` digit long, such that [start-1][start][start+1]... can fit into n
          *
          * @param check
          *   The length of `start`
          * @param skip
          *   The number of digits to ignore from the start. The ignored digits should fit into the end of [start-1]
          * @return
          *   The index of `start` - the number of digits skipped
          */
        @tailrec
        def breakDown(check: Int = 1, skip: Int = 0): Long = {
          val (lead, rest) = n.splitAt(skip)
          val start        = rest.take(check).toLong
          if (start != 0 && grow(start, rest.length) == rest && (start - 1).toString.endsWith(lead))
            indexOf(start) - skip
          else if (check == n.length) Long.MaxValue // stop searching when you reach the string length
          else if (check == skip + 1) breakDown(check + 1)
          else breakDown(check, skip + 1)
        }

        val carry   = "(.)(9+)(.+?)(.)(0*)".r
        val nines   = "(9+)".r
        val flanked = "(0+)(.*?)(0*)".r // NOTE: the second group MUST be lazy, to maximize the number of zeros in the third group

        val specialCase = n match {
          case carry(a, leading9, middle, b, trailing0) if leading9.length >= trailing0.length && b.toLong == a.toLong + 1 =>
            // ...[middle][a][9..9]|[middle][b][0..0]...
            //             ^ [a][9..9][middle][b][0..0]
            //             if b = a+1 and there are more 9s than 0s
            // NOTE: the third group (middle) MUST be lazy, so that the following cases still matches:
            //             v [8][9..9][middle][9][0..0] // here a greedy third group would steal from the fourth group
            // ...[middle][8][9..9]|[middle][9][0..0]...
            //             v [9][9..9][middle][1][0..0] // here a greedy third group would steal from the second group
            // ...[middle][9][9..9]|[middle][1][0..0]...
            indexOf(s"$middle$a$leading9".toLong) + middle.length
          case nines(allNines) =>
            // ...8[9..9]|9[0..0]...
            //      ^ first occurrence of [9..9]9
            indexOf(s"8${allNines.tail}".toLong) + 1
          case flanked(allZeros, "", "") =>
            // n was all zeros
            // ...1[0..0]...
            //      ^ first occurrence of [0..0]
            indexOf(s"1$allZeros".toLong) + 1
          case flanked(leading, middle, trailing) if leading.length > trailing.length =>
            // ...[middle][leading]|[middle][trailing]..1...
            //             ^ when leading > trailing
            indexOf(s"$middle$leading".toLong) + middle.length
          case flanked(leading, middle, trailing) =>
            // ...[middle]0..[leading]|[middle][trailing]1...
            //                ^ when leading <= trailing
            indexOf(s"$middle${trailing}1".toLong) - leading.length
          case _ => Long.MaxValue
        }

        (specialCase +: breakDown() +: cycle).min
      }

      /** The sum of number of digits of all numbers less than a given number
        *
        * @param n
        *   The given number
        */
      private def indexOf(n: Long): Long = {
        val nine = '9' +: n.toString.tail.map(_ => '0')
        val offset = nine.indices.tail.map { i =>
          i * nine.take(i).toLong
        }.sum
        (n - nine.toLong / 9) * nine.length + offset
      }

      /** Starting at `from`, build the string [from][from+1][from+2]... till it's length exceed the desired length Then return
        * the string truncated to the desired length
        *
        * @param from
        *   The starting point
        * @param length
        *   The desired length
        */
      @tailrec
      private def grow(from: Long, length: Int, acc: String = ""): String =
        if (acc.length >= length) acc.take(length)
        else grow(from + 1, length, acc + from)

    }

  }
}
