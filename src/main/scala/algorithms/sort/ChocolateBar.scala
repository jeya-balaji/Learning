package algorithms.sort

object ChocolateBar {

  def getCount(arr: Array[Int], d: Int, m: Int, startIndex: Int = 0, counter: Int = 0): Int = {
    val size = arr.length

    val nextElements = startIndex+m -1 //index starts with 0
    if (startIndex < size && nextElements < size) {
      val sum = (startIndex to nextElements).map(arr(_)).sum
      //println(s"startIndex $startIndex ele ${arr(startIndex)} next ${(startIndex to nextElements).toList} sum $sum")
      if (sum == d) {
        getCount(arr, d, m, startIndex+1, counter+1)
      } else {
        getCount(arr, d, m, startIndex+1, counter)
      }
    } else if (m == 1 && arr(0) == d) { //special case with only one element
      1
    } else {
      counter
    }
  }

}
