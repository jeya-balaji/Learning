package algorithms.dataStructures

case class MyString(c: Char*) {
  val size: Int = c.length
  val isEmpty: Boolean = size == 0

  val array: Array[Char] = c.toArray

  val value:String = toString

  def append(str: MyString): MyString = {
    val arr: Array[Char] = new Array[Char](size + str.size)
    array.indices foreach { i =>
      arr.update(i, c(i))
    }
    str.array.indices foreach{ i =>
      arr.update(size+i, str.c(i))
    }
    MyString(arr:_*)
  }

  def charAt(i: Int): Char = {
    if (i < size) {
      c(i)
    } else {
      throw new ArrayIndexOutOfBoundsException
    }
  }

  def indexOf(a: Char): Int = {
    def foundChar(index: Int = 0, isCharFound: Boolean = false): Int = {
      if (isCharFound) {
        index
      } else {
        if (index < size) {
          if (a == c(index)) {
            foundChar(index, isCharFound = true)
          } else {
            foundChar(index + 1)
          }
        } else {
          -1
        }
      }
    }

    foundChar()
  }

  def toUpperCase: MyString = {
    val cLower: Seq[Char] = c.map{i =>
      if (i > 90) {
        (i - 32).toChar
      } else {
        i
      }
    }
    MyString(cLower:_*)
  }

  def toLowerCase: MyString = {
    val cLower: Seq[Char] = c.map{i =>
      if (i <= 90 && i >= 65) {
        (i + 32).toChar
      } else {
        i
      }
    }
    MyString(cLower:_*)
  }

  def compareTo(str: MyString): Int = {
    val size1 = str.size
    val minSize = Math.min(size, size1)

    var res = 0
    var k = 0
    while( k < minSize) {
      val c1 = this.charAt(k)
      val c2 = str.charAt(k)
      if (c1 != c2) {
        res = c1 - c2
        k = minSize
      } else {
        k = k+1
      }
    }

    if (res == 0) {
      size - size1
    } else {
      res
    }
  }

  def contains(chars: Char*): Boolean = {
    if (isContains(MyString(chars:_*)) > 0) {
      true
    } else {
      false
    }
  }

  def isContains(str: MyString): Int = {
    val size1 = str.size
    val result = this.array.indices.map {
      case i if i+size1-1 < size =>
        println(s" $str ${i+size1-1} $size")
        val subStr = this.subString(i, i+size1-1)
        println(s"$subStr $str $i $size1 $size")
        if (subStr.equals(str)) {
          Option(1)
        } else {
          None
        }
      case _ => None
    }
    println(s"$result")
    result.map(_.getOrElse(0)).sum
  }

  def subString(startPos: Int, endPos: Int = size): MyString = {
    val size1 = (endPos - startPos) + 1
    if (endPos - startPos < size) {
      val arr = (startPos to endPos map { i =>
        this.charAt(i)
      }).toArray
      MyString(arr:_*)
    } else {
      MyString()
    }
  }

  def equals(str: MyString): Boolean = {
    var isEqual = true
    var k = 0

    if (str.size == size) {
      while (k < str.size && isEqual) {
        if (str.c(k) == c(k)) {
          k = k+1
          isEqual = true
        } else {
          isEqual = false
        }
      }
    } else {
      isEqual = false
    }

    isEqual
  }

  override def toString: String = {
    var str = ""
    c.foreach(i => str = str+i)
    str
  }

}
