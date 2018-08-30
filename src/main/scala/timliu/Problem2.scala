package timliu

import scala.collection.mutable.ListBuffer
import util.control.Breaks._
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedHashMap

object Problem2 {
  
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])
  
  /**
   * Takes in a list of promotions
   * returns the unique combinations of promotions with the maximum number of promotions in each
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    
    val promotionCombos = ListBuffer[PromotionCombo]()
    
    //Loop through all the promotions and generate combinations with each individual promotion
    for ((promotion, index) <- allPromotions.zipWithIndex) {
      
      //As an optimization we can give combinablePromotions only the part of the list that hasn't been traversed
        for (newCombo <- combinablePromotions(promotion.code, allPromotions.drop(index + 1))) {
          
          //It turns out that we could potentially general subsets which are invalid because they were a part of previous combinations
          //so we need to look to not add the combination if it occured as a subset before
          //This loop adds additional traversal overhead to the algorithm
          //I created another traversal which does not need this, but left this one as the primary
          //because this implementation is more straightforward
          breakable {
            for (existingCombo <- promotionCombos) {
              
              //Helper function for is subset
              if (isSubset(newCombo.promotionCodes, existingCombo.promotionCodes)) {
                break
              }
            }
            promotionCombos += newCombo 
          }
          
        }
        
      }
    
    return promotionCombos
  }

  /**
   * Nothing fancy
   * I couldn't find a standalone subset method
   * From what I've read we could put these in sets and use the built in subset method
   * But that would mean creating 2 sets every time we compare which seems worse for what we need
   * 
   */
  def isSubset( someSeq: Seq[String], otherSeq: Seq[String] ): Boolean = {
    
    if (otherSeq.length < someSeq.length) {
      return false
    }
    
    for (element <- someSeq) {
      if (!otherSeq.contains(element))
          return false
    }
      
    return true 
  }
  
  def combinablePromotions( promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    
    //filter out all values that we know are not going to be part of any potential promotion combo
    //might be able to do this inline in the loops below and save traversal time
    //but I think the loops below are easier to understand if we do this up here
    val compatiblePromotions = allPromotions.filterNot(_.code.equals(promotionCode)).filterNot(_.notCombinableWith.contains(promotionCode));
    
    //create a list of promotions to traverse
    //using a ListBuffer because we are going remove promotions from this list that we don't need to traverse anymore
    val promotionsToTraverse = compatiblePromotions.to[ListBuffer]
    
    val promotionCombos = ListBuffer[PromotionCombo]()
    
    while (!promotionsToTraverse.isEmpty) {
      
      //This is beginning of first combination
      //now we need to check all the other promotions to see what combinations are possible
      val promotionCombo = ListBuffer[String](promotionCode)

      for (promotionToAdd <- promotionsToTraverse) {
          
        breakable {
          
          //Check to see if the promotion we're about to add conflicts
          //with any of the existing promotions in this combination
          for (promotionInCombo <- promotionCombo) {
            if (promotionToAdd.notCombinableWith.contains(promotionInCombo))
              break
          }
          
          //Add the promotion to the combination
          promotionCombo += promotionToAdd.code
          
          //We need to remove promotions that we've grouped
          //otherwise the while loop won't end
          promotionsToTraverse -= promotionToAdd
        }
      }
      
      //Only add the promotion combo if it's length is greater than 1
      //otherwise, there were no combinations found
      if (promotionCombo.length > 1) {
        promotionCombos += PromotionCombo(promotionCombo.toSeq)
      }
    }
    
    return promotionCombos.seq;
  }
  
  /**
   * Didn't know if this was relevant, but this is a quick way for printing the way the output was expected
   * could just put the objects straight into println, but wouldn't quite have the right look per the test doc
   */
  def printCombinations(combinations: Seq[PromotionCombo]) = {
    println("Seq ( ")
    for ((combo, index) <- combinations.zipWithIndex) {
      print("  PromotionCombo(Seq(")
      for ((code, codeIndex) <- combo.promotionCodes.zipWithIndex) {
        print(code)
        if (codeIndex < combo.promotionCodes.length - 1)
          print(",")
      }
      print("))");
      if (index < combinations.length - 1)
          println(",")
      else
        println()
    }
    println(")")
  }
  
  /**
   * This was an experiment with a different algorithm which creates a map of relationships
   * and traverses the map to build the unique promotions without needed to to any subset checking
   * It works too and might be more efficient, but the downside is that it's much more complicated
   */
  def allCombinablePromotionsEnhanced(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    
    val relationships = LinkedHashMap[String, ListBuffer[String]]()
    
    for ((promotion, index) <- allPromotions.zipWithIndex) {
      val promotionRelationships = ListBuffer[String]()
      for (otherPromotion <- allPromotions.drop(index + 1)) {
        if (promotion != otherPromotion && !promotion.notCombinableWith.contains(otherPromotion.code)) {
          promotionRelationships += otherPromotion.code
        }
      }
      relationships.put(promotion.code, promotionRelationships)
    }
    
    val promotionCombos = ListBuffer[PromotionCombo]()
    
    var nextTraversalIndex = 0
    
    for ((promotionCode, relations) <- relationships) {
      
      nextTraversalIndex += 1
      
      while (!relations.isEmpty) {
        val firstNeighbor = relations.remove(0);
        val promotionCombo = ListBuffer[String](promotionCode, firstNeighbor)
        
        val relationsToAdd = ListBuffer[String]()
        val firstNeighborRelations = relationships.get(firstNeighbor).get
        for (firstNeighborRelation <- firstNeighborRelations) {
          if (relations.contains(firstNeighborRelation)) {
            relationsToAdd += firstNeighborRelation
          }
        }
        
        //remove the relationship from the neighbor only if 
        //that neighbor is no longer referenced by others
        var removeRelationshipFromNeighbor = true
        for ((code, otherRelations) <- relationships.drop(nextTraversalIndex)) {
          if (otherRelations.contains(firstNeighbor)) {
            removeRelationshipFromNeighbor = false
          }
        }
        
        for (toAdd <- relationsToAdd) {
          
          if (removeRelationshipFromNeighbor) 
            firstNeighborRelations -= toAdd
            
          relations -= toAdd
          promotionCombo += toAdd
        }
        
        promotionCombos += PromotionCombo(promotionCombo)
      }
    }

    return promotionCombos
  }
  
  def main(args: Array[String]) {
      
    val originalList = List(
        Promotion("P1", Seq("P3")), // P1 is not combinable with P3  
        Promotion("P2", Seq("P4", "P5")),// P2 is not combinable with P4 and P5
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")), 
        Promotion("P5", Seq("P2"))
            )

//    val additionalTestInput = List(
//        Promotion("P1", Seq("P3", "P6")),
//        Promotion("P2", Seq("P4", "P5", "P6", "P7")),
//        Promotion("P3", Seq("P1", "P6", "P7")),
//        Promotion("P4", Seq("P2", "P6", "P7")), 
//        Promotion("P5", Seq("P2", "P6", "P7")),
//        Promotion("P6", Seq("P1", "P2", "P3", "P4", "P5")),
//        Promotion("P7", Seq("P2", "P3", "P4", "P5"))
//    )
    
    println("Expected Output for All Promotion Combinations:")
    printCombinations(allCombinablePromotions(originalList))
    
    println("Expected Output for Promotion Combinations for promotionCode=\"P1\":")
    printCombinations(combinablePromotions("P1", originalList))
    
    println("Expected Output for Promotion Combinations for promotionCode=\"P3\":")
    printCombinations(combinablePromotions("P3", originalList))
     
  }

}
