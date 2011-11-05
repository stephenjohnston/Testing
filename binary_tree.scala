abstract class Tree[+T] {
  def containsElement[U >: T <% Ordered[U]](element : U) : Boolean
  def addElement[U >: T <% Ordered[U]](element: U) : Tree[U] 
  def removeElement[U >: T <% Ordered[U]](element: U) : Tree[U] 
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  
  def containsElement[U >: T <% Ordered[U]](element : U) : Boolean  = {
      if (element == value) 
          true
      else if (element < value) 
          left.containsElement(element)
      else
          right.containsElement(element)
  }

  def addElement[U >: T <% Ordered[U]](element: U) : Tree[U] = {
      if (element == value)
           this
      else if (element < value)
	   Node(value, left.addElement(element), right)
      else
	   Node(value, left, right.addElement(element))
  }

  def removeElement[U >: T <% Ordered[U]](element: U) : Tree[U] = {
      if (element == value) {
	if (left.isInstanceOf[Tree[Nothing]] && right.isInstanceOf[Tree[Nothing]]) { 
	    println("made it here")
	    left
        }
        else if (left.isInstanceOf[Tree[Nothing]]) 
	    right
        else if (right.isInstanceOf[Tree[Nothing]])
	    left
	else {
	    println("failed to delete node with value: " + value)
	    this
        }
      }
      else if (element < value)
          Node(value, left.removeElement(element), right)
      else
          Node(value, left, right.removeElement(element))
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def containsElement[U <% Ordered[U]](element : U) : Boolean  = {
      false
  }
  def addElement[U <% Ordered[U]](element: U) : Tree[U] = {
      Node(element, this, this)
  }
  def removeElement[U <% Ordered[U]](element: U) : Tree[U] = {
      this
  }
}

object LeafNode {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
