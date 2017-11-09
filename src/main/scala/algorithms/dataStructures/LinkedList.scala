package algorithms.dataStructures

sealed trait Node[+A] {
  def isEmpty: Boolean
  def isEmptyLink: Boolean
  def getLink: Node[A]
  def last: Node[A]
  def next: Node[A]
  def size: Int
  def getValue: A
  def getValue(position: Int): A
  def map[B](f: Node[A] => B): Node[B]
}

case object NoValueException extends Exception

case object EmptyNode extends Node[Nothing] {
  override def isEmpty: Boolean = true

  override def isEmptyLink: Boolean = true

  override def getLink: Node[Nothing] = EmptyNode

  override def last: Node[Nothing] = EmptyNode

  override def next: Node[Nothing] = EmptyNode

  override def size:Int = 0

  override def getValue: Nothing = throw NoValueException

  override def getValue(position: Int): Nothing = throw NoValueException

  override def map[B](f: (Node[Nothing]) => B): Node[B] = EmptyNode

  def append[A](value: A): Node[A] = LinkedList(value)

}

case class LinkedList[A](value: A, link: Node[A]) extends Node[A] {

  implicit def toNodeElement(node: Node[A]): LinkedList[A] = node.asInstanceOf[LinkedList[A]]

  override def isEmpty: Boolean = false

  override def isEmptyLink: Boolean = link.isEmpty

  override def getLink: Node[A] = link

  override def last: Node[A] = {
    nextWhen(this)(_.isEmptyLink)
  }

  override def next: Node[A] = nextWhen(this.link)( _ => true)

  override def getValue: A = value

  override def getValue(position: Int): A =  {
    def increment(n: Node[A], index: Int, position: Int): Node[A] = {
      if (index > position) {
        EmptyNode
      } else if (index == position){
        n
      } else {
        n match {
          case a: LinkedList[A] if a.isEmptyLink => EmptyNode
          case a: LinkedList[A] => increment(a.getLink, index+1, position)
        }
      }
    }

    increment(this, 0, position).getValue
  }

  override def map[B](f: (Node[A]) => B): Node[B] = {
    def applyFun(n: Node[A], f: Node[A] => B): Node[B] = {
      val valueB: B = f(n)
      val link = n.getLink
      if (link.isEmpty) {
        LinkedList(valueB, EmptyNode)
      } else {
        LinkedList(valueB, applyFun(link, f))
      }
    }

    applyFun(this, f)
  }

  def append(value: A): Node[A] = {
    if (link.isEmpty) {
      this.copy(link = LinkedList(value, EmptyNode))
    } else {
      val res = this.link.append(value)
      this.copy(link = res)
    }
  }

  def prepend(value:A): Node[A] = {
    LinkedList(value).copy(link = this)
  }

  def addAt(value: A, position: Int): Node[A] = {
    def increment(n: LinkedList[A], newNode: LinkedList[A], i: Int): Node[A] = {
      if (i == position - 1) {
        val modifiedNode = newNode.copy(link = n.link)
        n.copy(link = modifiedNode)
      } else {
        val res = increment(n.getLink,newNode, i+1)
        n.copy(link = res)
      }
    }

    if (position == 0) {
      prepend(value)
    } else if (position == size) {
      append(value)
    } else if (position < size){
      increment(this,LinkedList(value, EmptyNode), 0)
    } else {
      this
    }
  }

  def removeAt(position: Int): Node[A] = {
    def increment(n: Node[A], i: Int): Node[A] = {
      n match {
        case a: LinkedList[A] if i == position - 1 =>
          val nextNode = a.getLink.getLink
          a.copy(link = nextNode)
        case a: LinkedList[A] =>
          val res = increment(a.getLink, i+1)
          a.copy(link = res)
        case _ =>
          EmptyNode
      }
    }

    if (position == 0) {
      this.getLink
    } else if (position < size){
      increment(this, 0)
    } else {
      this
    }
  }

  def nextWhen(node: Node[A])(f: Node[A] => Boolean): Node[A] = {
    if (node.isEmpty) {
      EmptyNode
    } else if (f(node)) {
      node
    } else {
      nextWhen(node.getLink)(f)
    }
  }

  def size: Int = {
    def increment(node: Node[A], i: Int): Int = {
      node match {
        case n: LinkedList[A] => increment(node.getLink, i+1)
        case EmptyNode => i
      }
    }
    increment(this, 0)
  }
}

object LinkedList {
  implicit def toNodeElement[A](node: Node[A]): LinkedList[A] = node.asInstanceOf[LinkedList[A]]

  def apply[A](values: A*): Node[A] = {
    if (values.isEmpty) {
      EmptyNode
    } else {
      var newNode: Node[A] = EmptyNode
      values foreach{ v =>
        if (newNode.isEmpty) {
          newNode = LinkedList(v, EmptyNode)
        } else if (newNode.isEmptyLink) {
          newNode = newNode.copy(link = LinkedList(v, EmptyNode))
        } else {
          newNode = newNode.append(v)
        }
      }
      newNode
    }
  }
}

