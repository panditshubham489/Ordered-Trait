
trait Set[A <: Ordered[A]]{
  def contains(value: A): Boolean
  def include(element: A): Set[A]
}

class EmptySet[A <: Ordered[A]] extends Set[A]{
  override def contains(value: A): Boolean = false
  override def include(element: A): Set[A] = new NonEmptySet(element, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](element: A, leftChild: Set[A], rightChild: Set[A]) extends Set[A]{
  override def contains(element: A): Boolean = {
    if (this.element > element) rightChild contains this.element
    else if (this.element < element) leftChild contains this.element
    else true
  }

  override def include(element: A): Set[A] = {
    if (this.element < element) new NonEmptySet[A](element, leftChild include this.element, rightChild)
    else if (this.element > element)  new NonEmptySet[A](element, leftChild, rightChild include this.element)
    else this
  }

}

case class number(element: Int) extends Ordered[number] {
  override def compare(that: number): Int = {
    if (that.element > this.element) 1
    else if (that.element < this.element) -1
    else 0
  }
}

