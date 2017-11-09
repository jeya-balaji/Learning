package algorithms.sort

import scala.annotation.tailrec

object SelectionSort {
  @tailrec
  def sort(list: Array[Int], selectedIndex: Int = 0): Array[Int] = {
    val size = list.length
    if (selectedIndex < size){
      val element = list(selectedIndex)
      val minIndex = findMinIndex(list, selectedIndex, selectedIndex)
      list(selectedIndex) = list(minIndex)
      list(minIndex) = element
      println(list.toList)
      sort(list, selectedIndex+1)
    } else {
      list
    }
  }

  def findMinIndex(arr: Array[Int], startIndex: Int = 0, minIndex: Int = 0): Int = {
    val size = arr.length
    if (startIndex < size && minIndex < size) {
      val e1 = arr(startIndex)
      if(e1 < arr(minIndex)) {
        //println(s"start $startIndex  ${arr(startIndex)} min $minIndex ${arr(minIndex)}")
        findMinIndex(arr, startIndex+1, startIndex)
      } else {
        //println(s" else start $startIndex ${arr(startIndex)} min $minIndex ${arr(minIndex)}")
        findMinIndex(arr, startIndex+1, minIndex)
      }
    } else {
      minIndex
    }
  }
}
