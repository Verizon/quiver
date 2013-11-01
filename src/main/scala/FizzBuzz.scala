// Dummy test file -- 

class FizzBuzz {

  def valueOf(input : Int) = {
    (input % 3, input % 5) match {
      case (0,0) => "fizzbuzz"
      case (0,_) => "fizz"
      case (_,0) => "buzz"
      case _ => input toString
    }
  }
}
