import org.scalatest.funsuite.AnyFunSuite

class OrderedTraitTest extends AnyFunSuite {
  val emptySetObj = new EmptySet[number]

  test("To check an EmptySet is including a number"){
    val obj = emptySetObj.include(number(8))
    assert(obj.contains(number(8)))
  }

  val nonEmptySetObj = new NonEmptySet[number](
    number(4),
    emptySetObj.include(element = number(3)),
    emptySetObj.include(element = number(2))
  )

  test("To check an NonEmptySet is contains a number"){
    assert(nonEmptySetObj.contains(number(4)))
  }

  test("To check an NonEmptySet does not contains a number"){
    assert(!nonEmptySetObj.contains(number(7)))
  }

}