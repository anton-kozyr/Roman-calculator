package ant

import scala.None

object RomanCalc {
  val map = List(
    1000 -> "M", 900 -> "CM", 500 -> "D", 400 -> "CD",
    100 -> "C", 90 -> "XC", 50 -> "L", 40 -> "XL",
    10 -> "X", 9 -> "IX", 5 -> "V", 4 -> "IV", 1 -> "I")
    
  def arabicToRoman(arabic:Int) = {
    def ator(arabic:Int, roman:String, map:List[(Int, String)]):String = (arabic, map) match {
      case (0, _) => roman
      case (_, h :: tail) => 
        ator(arabic % h._1, roman + h._2 * (arabic / h._1), tail)
    }
    ator(arabic, "", map)
  }
  
  def romanToArabic(roman:String) = {
    def rtoa(roman:String, arabic:Int, map:List[(Int, String)]):Int = (map, roman) match {
      case (_, "") => arabic
      case (h :: _, _) if roman startsWith h._2 => rtoa(roman substring h._2.length, arabic + h._1, map)
      case (_ :: tail, _) => rtoa(roman, arabic, tail)
    }
    rtoa(roman, 0, map);
  }
}


