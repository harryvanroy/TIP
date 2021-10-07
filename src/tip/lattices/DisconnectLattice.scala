package tip.lattices

object DisconnectElement extends Enumeration {
  val True, False = Value
}

object DisconnectLattice extends FlatLattice[DisconnectElement.Value] with LatticeWithOps {

  import DisconnectElement._

  def isDisconnected: Element = True
  def notDisconnected: Element = False

  def num(i: Int): Element = ???

  def plus(a: Element, b: Element): Element = ???

  def minus(a: Element, b: Element): Element = ???

  def times(a: Element, b: Element): Element = ???

  def div(a: Element, b: Element): Element = ???

  def eqq(a: Element, b: Element): Element = ???

  def gt(a: Element, b: Element): Element = ???
}

