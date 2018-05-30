package vp.interview

import scala.collection.immutable.List
import List._

/**
  * An attempt to answer the Shopping Cart interview question with more FP and
  * less OO. The top-level ShoppingCart object should be thought of as a module.
  *
  * @author Caleb Harris
  */
object ShoppingCart {

  // Define a Discount type, which is a function that takes a Double and returns
  // a Double, and two implementations.  One captures discounting a price by
  // a percentage, and the other by a dollar amount.
  type Discount = Double => Double
  def percent(pct: Double): Discount = price => price * (1.0 - pct)
  def amount(amt: Double): Discount = price => price - amt

  // A marker trait meaning "something that the Cart can hold"
  trait CartItem

  // We could create a whole Cart class and put the the totalPrice method on it,
  // but it's simpler to just use a list and define a function that can deal
  // with it.  This seems to be more in line with Scala idioms.
  type Cart = List[CartItem]

  // One kind of thing the Cart can hold is a Product
  case class Product(name: String, price: Double) extends CartItem {
    // A product knows how to copy itself with a discounted price, but expects
    // the discount calculation logic to be provided. Using immutable objects
    // and copying them is more "functional" than modifying object in place.
    def discountBy(d: Discount): Product = copy(price = d(price))
  }

  // Another kind of thing the Cart can hold is a Coupon, but there's more than one
  // type of Coupon.
  trait Coupon {
    // A coupon isn't much more than a function that transforms a Cart into...
    // another Cart
    def applyDiscounts(items: Cart): Cart
  }

  // The 'extends Coupon with CartItem' syntax makes PctOffEach a subtype of
  // both Coupon and CartItem
  case class PctOffEach(pct: Double) extends Coupon with CartItem {
    // PctOffEach is a straightforward mapping: every Product in the input
    // produces a discounted Product in the output. So we can literally just
    // map over the Cart with a discounting function
    override def applyDiscounts(cart: Cart): Cart = cart map {
      // this is the same as p.discountBy(percent(pct)). Any method in Scala
      // can be called with infix syntax like this. Also, if a function body
      // consists solely of a match statement, you can omit the match part
      // of the statement and skip straight into the cases.
      case p: Product => p discountBy percent(pct)
      case c => c
    }
  }

  case class PctOffNext(pct: Double) extends Coupon with CartItem {
    override def applyDiscounts(cart: Cart): Cart = {
      // PctOffNext is more than just a mapping. It requires context with each
      // item in the cart to decide what to do. We can accomplish providing that
      // context by folding over the Cart with a tuple for the "zero" type. In
      // this case, the context contains a Boolean that's true if the next
      // Product should be discounted. Because we're folding instead of mapping,
      // we need to build up the output Cart manually. The accumulating output
      // Cart (which is just a List) is the other half of the context.
      cart.foldLeft[(Boolean, Cart)]((false, Nil)) {
        case ((_, is), c: PctOffNext) if c == this => (true, c :: is)
        case ((true, is), p: Product) => (false, (p discountBy percent(pct)) :: is)
        case ((b, is), i) => (b, i :: is)
      }._2.reverse
    }
  }

  case class AmtOffNth(amt: Double, name: String, index: Int) extends Coupon with CartItem {
    override def applyDiscounts(cart: Cart): Cart =
      // Here, we're using the same idea as in PctOffNext, but using an Int in
      // our context tuple, which represents the number of matching Products
      // encountered in the Cart so far.
      cart.foldLeft[(Int, Cart)]((1, Nil)) {
        case ((n, is), p: Product) if p.name == name =>
          if (n == index)
            (n + 1, (p discountBy amount(amt)) :: is)
          else
            (n + 1, p :: is)

        case ((n, is), i) =>
          (n, i :: is)
      }._2.reverse
  }

  def totalPrice(cart: Cart): Double = {
    val discounted = cart.foldLeft(cart) {
      case (is, c: Coupon) => c applyDiscounts is
      case (is, _) => is
    }
    println(s"             discounted: $discounted")
    discounted.foldLeft(0.0) {
      case (b, p: Product) => b + p.price
      case (b, _) => b
    }
  }

  // Everything below is just glue to set up and run the examples

  def runExample(n: Int, cart: Cart): Unit = {
    println(s"example $n --   original: $cart")
    println(s"                  price: ${totalPrice(cart)}\n")
  }

  // This isn't strictly necessary, but allows us to create a Cart by calling
  // Cart(...), instead of List(...)
  object Cart {
    def apply(items: CartItem*): Cart = List(items: _*)
  }

  def main(args: Array[String]): Unit = {
    runExample(1, Cart(
      PctOffNext(0.1),
      Product("postcard sorter", 10),
      Product("stationery organizer", 20)
    ))

    runExample(2, Cart(
      Product("postcard sorter", 10),
      PctOffNext(0.1),
      Product("stationery organizer", 20)
    ))

    runExample(3, Cart(
      Product("postcard sorter", 10),
      AmtOffNth(2.0, "postcard sorter", 2),
      PctOffEach(0.25),
      PctOffNext(0.1),
      Product("postcard sorter", 10)
    ))
  }

}
