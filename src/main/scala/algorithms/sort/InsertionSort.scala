package algorithms.sort

object InsertionSort {

  def sort(arr: Array[Int], index: Int = 0): Array[Int] = {
    if (index < arr.length) {
      if (index == 0) {
        sort(arr, index + 1)
      } else {
        (0 until index) foreach { i =>
          if (arr(i + 1) > arr(i)) {
            println(s"${arr.toList} $index")
            sort(arr, index + 1)
          } else {
            val temp = arr(i + 1)
            arr(i + 1) = arr(i)
            arr(i) = temp
            println(s"${arr.toList} $index")
            sort(arr, index)
          }
        }
        arr
      }
    } else {
      arr
    }
  }

}
