package name.aloise

import java.time.Instant

import scala.language.{higherKinds, implicitConversions}
import name.aloise.model._

/**
  * Created by aloise on 01.09.16.
  */


case class User( name:String, email:String, password:String, createdAt:Instant ) extends Row

object CaseClassTests {
  def main(args: Array[String]): Unit = {

    import WithIdHelper._

    val user = User("aloise","test@test.com", "password", Instant.now())

    val userWithId = user withId 5

    userWithId match {
      case WithId( id, User(name, email, password, _) ) =>
        println("unapplied successfully " + name +" " + email)
    }

  }
}