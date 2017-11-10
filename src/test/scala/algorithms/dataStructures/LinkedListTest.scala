package algorithms.dataStructures

import algorithms.UnitSpec
import algorithms.dataStructures.LinkedList._

class LinkedListTest extends UnitSpec {

  "Linked list" should "create new linked list object for a single integer" in {
    val ll = LinkedList(1)

    ll should not be EmptyNode
    ll.isEmpty shouldBe false
    ll.size shouldBe 1
    ll.getLink shouldBe EmptyNode
  }

  it should "create new linked list with set of given integers" in {
    val ll = LinkedList(1,2,3,4,5)

    ll should not be EmptyNode
    ll.isEmpty shouldBe false
    ll.size shouldBe 5
    ll.getLink should not be EmptyNode
  }

  it should "get the value of the head node if no position given" in {
    val ll = LinkedList(1,2,3,4,5)

    ll.getValue shouldBe 1
  }

  it should "get the value of the node by the given node" in {
    val ll = LinkedList(1,2,3,4,5)

    ll.getValue(3) shouldBe 4
  }

  it should "throw NoValueException for invalid position" in {
    val ll = LinkedList(1,2,3,4,5)

    the [NoValueException] thrownBy {
      ll.getValue(-1)
    }
  }

  it should "give the next item for the given node" in {
    val ll = LinkedList(1,2,3,4,5)
    val ll1 = ll.next
    val ll2 = ll1.next

    ll1.size shouldBe 4
    ll2.size shouldBe 3

    ll1.getValue shouldBe 2
    ll2.getValue shouldBe 3
  }

  it should "create empty node for empty items" in {
    val ll = LinkedList()

    ll shouldBe EmptyNode
    ll.isEmpty shouldBe true
    ll.size shouldBe 0
  }

  it should "get link for non-empty linked list" in {
    val ll = LinkedList(1,2,3,4,5)

    ll should not be EmptyNode
    ll.size shouldBe 5
    ll.getLink should not be EmptyNode
    ll.getLink.size shouldBe 4
    ll.getLink.isEmpty shouldBe false
    ll.getLink.getValue shouldBe 2
  }

  it should "give the last item of the list" in {
    val ll = LinkedList(1,2,3,4,5)

    ll.size shouldBe 5
    ll.last.getValue shouldBe 5
  }

  it should "append the given item to the existing list" in {
    val ll = LinkedList(1,2,3,4,5).append(6)
    ll.size shouldBe 6
    ll.last.getValue shouldBe 6
  }

  it should "prepend the given item to the existing list" in {
    val ll = LinkedList(1,2,3,4,5).prepend(0)
    ll.size shouldBe 6
    ll.getValue shouldBe 0
  }

  it should "add an node to the given position" in {
    val ll = LinkedList(1,2,3,4,5).addAt(6, 3)

    ll.size shouldBe 6
    ll.getValue(3) shouldBe 6
  }

  it should "prepend using addAt for 0 as an input" in {
    val ll = LinkedList(1,2,3,4,5).addAt(6, 0)
    val ll1 = LinkedList(1,2,3,4,5).prepend(6)

    ll.size shouldBe 6
    ll.getValue shouldBe 6

    ll shouldBe ll1
  }

  it should "append using addAt for list size as an input" in {
    val ll = LinkedList(1,2,3,4,5).addAt(6, 5)
    val ll1 = LinkedList(1,2,3,4,5).append(6)

    ll.size shouldBe 6
    ll.getValue shouldBe 1

    ll shouldBe ll1
  }

  it should "not add item if the position is wrong" in {
    val ll = LinkedList(1,2,3,4,5)
    val ll2 = ll.addAt(6, -1)

    ll shouldBe ll2
  }


}
