// Dummy test file - 

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.Test

class TestStuff extends JUnitSuite {

  val calc = new FizzBuzz

  @Test
  def oneIsOne {
    assertEquals("1", calc valueOf 1)
  }

  @Test
  def twoIsTwo {
    assertEquals("2", calc valueOf 2)
  }

  @Test
  def threeIsFizz {
    assertEquals("fizz", calc valueOf 3)
  }

  @Test
  def fourIsFour {
    assertEquals("4", calc valueOf 4)
  }

  @Test
  def fiveIsBuzz {
    assertEquals("buzz", calc valueOf 5)
  }

  @Test
  def sixIsFizz {
    assertEquals("fizz", calc valueOf 6)
  }

  @Test
  def nineIsFizz {
    assertEquals("fizz", calc valueOf 9)
  }

  @Test
  def tenIsBuzz {
    assertEquals("buzz", calc valueOf 10)
  }

  @Test
  def fifteenIsFizzBuzz {
    assertEquals("fizzbuzz", calc valueOf 15)
  }
}
