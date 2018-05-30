package fpinscala.monoids

/**
  * @author caleb
  */
sealed trait WC {
  def +(that: WC): WC
}
case class Stub(chars: String) extends WC {
  override def +(that: WC): WC = that match {
    case Stub(str) => Stub(chars + str)
    case p: Part => p.copy(lStub = chars + p.lStub)
  }
}
case class Part(lStub: String, words: Int, rStub: String) extends WC {
  override def +(that: WC): WC = that match {
    case Stub(str) => copy(rStub = rStub + str)
    case p: Part => copy(words = words + p.words + (if (rStub.nonEmpty || p.lStub.nonEmpty ) 1 else 0), rStub = p.rStub)
  }
}

object WC {
  def apply(s: String): WC = Monoid.foldMapV(s, WCMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString))

  def countWords(s: String): Int = WC(s) match {
    case Stub(chars) => if (chars.isEmpty) 0 else 1
    case Part(left, n, right) => n + (if (left.isEmpty) 0 else 1) + (if (right.isEmpty) 0 else 1)
  }

  case object WCMonoid extends Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 + a2
    override val zero: WC = Stub("")
  }
}
