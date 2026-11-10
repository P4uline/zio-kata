package fr.p4uline.kata.zio.errors

import zio.{Cause, ZIO}

case class User(id: Long, name: String)

sealed trait UserError {
  def getMessage: String
}

case class UserNotFoundException() extends UserError {
  def getMessage: String = "User not found"
}

case class DatabaseUnavailable() extends UserError {
  def getMessage: String = "Database unavailable"
}

case class UserDisabled() extends UserError {
  def getMessage: String = "User disabled"
}

case class Interrupted() extends UserError {
  def getMessage: String = "An interruption occurred"
}

class UserRepository {
  def getUser(id: Long): ZIO[UserRepository, UserError, User] = {
    id match {
      case 1 => ZIO.fail(UserDisabled())
      case 2 => ZIO.fail(UserNotFoundException())
      case 3 => ZIO.fail(DatabaseUnavailable())
      case 4 => ZIO.interrupt
      case 5 => ZIO.die(throw new Exception("some connection exception"))
      case _ => ZIO.succeed(User(id = 1000L, name = "Dadou"))
    }
  }
}

class UserService(userRepository: UserRepository) {

  def getUser(id: Long): ZIO[UserRepository, String, User] = {
    for {
      user <- userRepository.getUser(id)
        .mapError(e => s"${e.getMessage}")
        .onInterrupt(ZIO.logError(s"An interruption occurred getting the user ${id}"))
        .catchAllCause { (cause: Cause[String]) =>
          if (cause.isInterrupted) {
            ZIO.fail("User request interrupted")
          } else if (cause.isDie) {
            ZIO.fail("User request died")
          } else {
            ZIO.failCause(cause)
          }
        }
    } yield {
      user
    }
  }

}
