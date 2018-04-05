object CartExperiments {

  import cats.free.Free

  // Experimental ADT - representing Cart as Free Monad
  sealed trait CartOperationA[A]

  case class AddCartItem[ST <: Store, C <: CartItemWithQuantity[ST]](item: C) extends CartOperationA[C] // add a particular quantity of items to the cart
  case class RemoveCartItem[ST <: Store, C <: CartItemWithQuantity[ST]](itemToRemove: C) extends CartOperationA[C] // removes a particular quantity from the cart. Might remove the whole position
  case class AddCouponCode(coupon: CouponCodeString) extends CartOperationA[CouponCodeString]

  case object RemoveCouponCode extends CartOperationA[Unit]

  case class SetShippingCountry(country: CountryCode) extends CartOperationA[CountryCode] // store default initially

  type CartOperation[A] = Free[CartOperationA, A]


  object Ops {

    import cats.free.Free.liftF

    def addCartItem[ST <: Store, C <: CartItemWithQuantity[ST]](item: C): CartOperation[C] =
      liftF[CartOperationA, C](AddCartItem[ST, C](item))

    def removeCartItem[ST <: Store, C <: CartItemWithQuantity[ST]](item: C): CartOperation[C] =
      liftF[CartOperationA, C](RemoveCartItem[ST, C](item))

    def addCouponCode(couponCode: CouponCodeString): CartOperation[CouponCodeString] =
      liftF[CartOperationA, CouponCodeString](AddCouponCode(couponCode))

    def removeCouponCode: CartOperation[Unit] =
      liftF(RemoveCouponCode)

    def setShippingCountry(countryCode: CountryCode): CartOperation[CountryCode] =
      liftF(SetShippingCountry(countryCode))

    // sample

    import com.juniqe.model.dto.GermanStore

    def cartItem[ST <: Store](id: Int): CartItemWithQuantity[ST] = new CartItemWithQuantity[ST] {
      override def quantity: Quantity = Quantity(2)

      override def product: CartProductItemId = CartProductItemId(ConfigurableProductId(id), Set(SimpleProductId(id + 1)))
    }

    def sampleProgram[ST <: Store]: CartOperation[(Seq[CartItemWithQuantity[ST]], CouponCodeString, CountryCode)] = {
      for {
        item1 <- addCartItem(cartItem(1))
        item2 <- addCartItem(cartItem(10))
        item3 <- addCartItem(cartItem(100))
        coupon <- addCouponCode(CouponCodeString("test"))
        shipping <- setShippingCountry(CountryCode.DE)
      } yield (Seq(item1, item2, item3), coupon, shipping)
    }


    type CartOperationState[A] = State[Cart[ST], A] forSome {type ST <: Store}

    val pureCompiler: CartOperationA ~> CartOperationState = new (CartOperationA ~> CartOperationState) {
      def apply[A](fa: CartOperationA[A]): CartOperationState[A] =
        fa match {
          case AddCartItem(item) =>
            ???
          case RemoveCartItem(item) =>
            ???
          case AddCouponCode(code) =>
            ???
          case RemoveCouponCode =>
            ???
          case SetShippingCountry(country) =>
            ???

        }
    }

  }


}
