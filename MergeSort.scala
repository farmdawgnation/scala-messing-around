/*****
 * Recursive Merge Sort
 * Matt Farmer
 *
 * Simple little recursive Merge Sort I wrote to help me learn Scala.
******/

object MergeSort {
  def sort(elements: Seq[Int]): Seq[Int] = {
    var strSort = "Sorting: "
    elements.foreach(strSort += _ + " " )
    println(strSort)
    
    MergeSort.sortsub(elements)
  }
  
  /**
   * The heavy lifter of the Merge sort algorithm.
   *
   * This method splits the element sequence into two sequences, each representing
   * one half of the sequence. Then, it checks the size. If either of the resulting
   * sequences is larger than one element then MergeSort.sortsub is recursively called
   * on that sequence. When there is only one element in each sub array, the two are 
   * compared and unified into one array, then returned upstream.
   *
   * I put my right hand in, I put my right hand out... and that's how we do a merge sort.
   *
   * @author Matt Farmer
  **/
  private def sortsub(elements: Seq[Int]): Seq[Int] = {
    //Generate sub sequences of the elements sequence
    var aryA : Seq[Int] = elements.view(0, (elements.length/2)).force
    var aryB : Seq[Int] = elements.view((elements.length/2), elements.length).force
    
    //Loop to output our first sub array.
    println()
    var strA = "Array A: "
    aryA.foreach(strA += _ + " ")
    println(strA)
    
    //Loop to output our second sub array.
    var strB = "Array B: "
    aryB.foreach(strB += _ + " ")
    println(strB)
    
    //If a is more than one element, recurse on aryA
    if(aryA.length > 1) {
      aryA = MergeSort.sortsub(aryA)
    }
    
    //If b is more than one element, recurse on aryB
    if(aryB.length > 1) {
      aryB = MergeSort.sortsub(aryB)
    }
    
    // Declare our counter variables.
    var ia : Int = 0 // index A counter
    var ib : Int = 0 // index B counter
    var newAryLength : Int = aryA.length + aryB.length // size of the result array
    var aryResult: Array[Int] = new Array(newAryLength) // the result array
    
    //while...
    while( ia + ib < newAryLength ) {
      // until we've iterated over every element in aryA and aryB, compare these elements.
      // To make this happen without IndexOutOfBoundsExceptions, we say the following:
      // Add aryA[ia] as the next element in the result if:
      //   (i) ia < the length of aryA
      //   (ii) aryA[ia] is less than aryB[ib] OR aryB has been used up.
      // I believe this covers every possible scenario.
      
      if(ib < aryB.length && ia < aryA.length) println("Compare " + ia + "=" + aryA.apply(ia) + " and " + ib + "=" + aryB.apply(ib))
      
      if( ia < aryA.length && (ib == aryB.length || aryA.apply(ia) < aryB.apply(ib)) ) {
        aryResult.update(ia+ib, aryA.apply(ia))
        ia = ia + 1 // increment index for aryA
      } else {
        aryResult.update(ia+ib, aryB.apply(ib))
        ib = ib + 1 // increment index for aryB
      }
    }
    
    // Output resulting array for diagnostic purposes.
    var strAryResult = "Resulting Array: "
    aryResult.foreach(strAryResult += _ + " ")
    println(strAryResult)
    
    return aryResult
  }
}

//////
// Main Script
//////

println("My first Scala Merge Sort!")
println("WHEEEEEE")
println()

var aryToSort = Array(3, 1, 2, 6, 0, 2, 12)
var sortResult = MergeSort.sort(aryToSort)

var strResult = "Result: "
sortResult.foreach(strResult += _ + " ")
println()
println(strResult)
