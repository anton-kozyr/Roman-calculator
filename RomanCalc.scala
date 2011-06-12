object RomanCalc {
  // Mapping between Arabic numbers and Roman "digits"
  val map = List(
    1000 -> "M", 900 -> "CM", 500 -> "D", 400 -> "CD",
    100 -> "C", 90 -> "XC", 50 -> "L", 40 -> "XL",
    10 -> "X", 9 -> "IX", 5 -> "V", 4 -> "IV", 1 -> "I")
    
  def arabicToRoman(arabic:Int) = {
    /**
     * Recursive function which exits only when first parameter (Arabic number) is zero. 
     * Otherwise it calls itself reducing Arabic number and accumulating Roman number.
     * When Arabic number is zero it returns second parameter (Roman number).
     *
     * @param arabic Arabic number which is being reduced by value stored in the head of the mapping
     * @param roman Roman number which is being appended with corresponding string from the mapping
     * @param map the mapping is being reduced by one element every call. The mapping's head tuple is used as
     *   a divider for the Arabic number and as a suffix for the Roman one.
     */
    def ator(arabic:Int, roman:String, map:List[(Int, String)]):String = (arabic, map) match {
      // Nothing to add, because Arabic number is zero, return current Roman number
      case (0, _) => roman
      // Divide Arabic number by the head of the mapping to determine how much Roman "digits"
      // should be added to the Roman number.
      // Reduce Arabic number by product of Roman "digit's" value and the result of the division 
      // and add corresponding number of Roman "digits" to the Roman number.
      // Also we reduce mapping by one element and try next Roman "digit"
      case (_, h :: tail) => ator(arabic % h._1, roman + h._2 * (arabic / h._1), tail)
    }
    // Invoke the function passing input Arabic number, empty initial Roman number and full mapping
    ator(arabic, "", map)
  }
  
  def romanToArabic(roman:String) = {
    /**
     * Recursive function which exits only when first parameter (Roman number) is an empty string. 
     * Otherwise it calls itself reducing Roman number and accumulating Arabic number.
     * When Roman number is an empty string it returns second parameter (Arabic number).
     *
     * @param roman Roman number which is being reduced by the Roman "digit" string from the mapping's head
     * @param arabic Arabic number which is being  by value stored in the head of the mapping
     * @param map the mapping is being reduced by one element every call
     */
    def rtoa(roman:String, arabic:Int, map:List[(Int, String)]):Int = (map, roman) match {
      // Return current Arabic number because Roman number is empty
      case (_, "") => arabic
      // If current top Roman "digit" is equal to one in the mapping, remove it from the Roman number
      // and add its numerical value to the Arabic number
      case (h :: _, _) if roman startsWith h._2 => rtoa(roman substring h._2.length, arabic + h._1, map)
      // Otherwise throw out top element from the mapping and invoke itself again
      case (_ :: tail, _) => rtoa(roman, arabic, tail)
    }
    // Invoke the function passing input Roman number, 0 as initial Arabic number and full mapping
    rtoa(roman, 0, map);
  }

  // Pseudo test
  def main(argv:Array[String]) = 
    1.until(10000).foreach { x => if (RomanCalc.romanToArabic(RomanCalc.arabicToRoman(x))!=x) println("Fail " + x) }
}


