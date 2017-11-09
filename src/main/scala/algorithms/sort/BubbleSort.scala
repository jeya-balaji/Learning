package algorithms.sort

import scala.annotation.tailrec

object BubbleSort {

  @tailrec
  def sort(list: Array[Int], position: Int = 0, iteration: Int = 1): Array[Int] = {
    val size = list.length
    val ele1 = list(position)

    if (iteration <= size) {
      if ((position + 1) < size) {
        val ele2 = list(position + 1)
        if (ele1 > ele2) {
          list(position) = ele2
          list(position+1) = ele1
        }
        println(list.toList)
        sort(list, position + 1, iteration)
      } else {
        sort(list, 0, iteration + 1)
      }
    } else {
      list
    }
  }

  def sortWithoutRec(list: Array[Int]): Array[Int] = {
    val size = list.length
    var swapped = true

    while(swapped) {
      swapped = false
      (1 until size).foreach { j =>
        if(list(j-1) > list(j)) {
          swap(list, j-1, j)
          swapped = true
        }
        println(list.toList)
      }
      size - 1
    }
    list
  }

  private def swap(array: Array[Int], i: Int, j: Int): Unit = {
    val tmp = array(i)
    array(i) = array(j)
    array(j) = tmp
  }

}
