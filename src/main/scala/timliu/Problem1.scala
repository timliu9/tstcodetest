package timliu

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Map

object Problem1 {
  
  case class Rate(rateCode: String, rateGroup: String)
  
  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
  
  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    //This is a hashmap of bestGroupPrices
    //Grouped by cabinCode and rateGroup using the 2 as a composite key
    //Using a LinkedHashMap so that the BestGroupPrices will be returned in the order that they were traversed
    val bestGroupPrices = LinkedHashMap[String, BestGroupPrice]()
    
    //Create a map of the rateCodes so that we can have fast lookup for the rateGroup
    val rateCodeMap = Map[String, String]()
    rates.foreach(rate => rateCodeMap.put(rate.rateCode, rate.rateGroup))
    
    for (price <- prices) {

      val rateGroupOption = rateCodeMap.get(price.rateCode)

        //Being safe by checking that we've found the rateGroup
      if (rateGroupOption.isDefined) {
        
        val rateGroup = rateGroupOption.get
        
        //create our group pricing aggregation based on cabin code and rateGroup 
        val bestGroupPriceKey = price.cabinCode + rateGroup
        val bestGroupPrice = bestGroupPrices.get(bestGroupPriceKey)
          
        //set the new BestGroupPrice if we don't have one yet
        //or if this price is better than the existing one
        if (bestGroupPrice.isEmpty || bestGroupPrice.get.price > price.price) {
          
          bestGroupPrices.put(bestGroupPriceKey, 
            BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateGroup))
            
        } 
      }
    }

    return bestGroupPrices.values.toList;
  }
  
  def main(args: Array[String]) {
    
    val rates = List(
        Rate("M1", "Military"), 
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
        )
        
    val prices = List(
        CabinPrice("CA", "M1", 200.0),
        CabinPrice("CA", "M2", 250.0),
        CabinPrice("CA", "S1", 225.0),
        CabinPrice("CA", "S2", 260.0),
        CabinPrice("CB", "M1", 230.0),
        CabinPrice("CB", "M2", 260.0),
        CabinPrice("CB", "S1", 245.0),
        CabinPrice("CB", "S2", 270.0)
        )

    val bestGroupPrices = getBestGroupPrices(rates, prices);
    
    println("Expected Output - Best Cabin Prices:")
    for (groupPrice <- bestGroupPrices)
      println(groupPrice)
  }
}
