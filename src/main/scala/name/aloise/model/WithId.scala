package name.aloise.model

/**
  * User: aloise
  * Date: 15.10.16
  * Time: 0:47
  */
abstract class Row extends Product

class WithId[T <: Row ]( val id: Int, val value: T ) extends Row {

  override def productElement(n: Int): Any = n match {
    case 0 => id
    case _ => value.productElement( n - 1 )
  }

  override def productArity: Int = 1 + value.productArity

  override def canEqual(that: Any): Boolean = {
    that match {
      case value1: this.type =>
        value1.canEqual(this) && ( value1.id == this.id )
      case _ =>
        false
    }
  }

}

object WithId {

  def unapply[T <: Row](arg: WithId[T]): Option[(Int,T)] =
    Some( (arg.id, arg.value) )
}

object WithIdHelper {


  class WithIdRichHelper[T<:Row](value:T) {

    def withId( id: Int ): WithId[T] = {
      new WithId[T](id, value)
    }
  }

  implicit def toProduct[T<:Row]( withIdClass:WithId[T] ):T = {
    withIdClass.value
  }

  implicit def liftToWithId[T<:Row](value:T):WithIdRichHelper[T] = {
    new WithIdRichHelper[T](value)
  }

}