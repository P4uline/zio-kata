package fr.p4uline.kata.zio.errors

import zio.test.*
import zio.{Scope, ZLayer}

object ZIOErrorManagementKataSpec extends ZIOSpecDefault {
  val userRepository = new UserRepository()
  val userService = new UserService(userRepository = userRepository)
  def spec: Spec[TestEnvironment & Scope, Any] = suite("UserService")(
    test("should get successfully the user") {
      for {
        result <- userService.getUser(1000L)
      } yield assertTrue(
        result == User(id = 1000L, name = "Dadou")
      )
    },
    test(
      "should get a 'user disabled' message when trying to a disabled user"
    ) {
      for {
        result <- userService.getUser(1L).either
      } yield assertTrue(
        result == Left("User disabled")
      )
    },
    test(
      "should get a 'user not found' message when trying to get a non existing user"
    ) {
      for {
        result <- userService.getUser(2L).either
      } yield assertTrue(
        result == Left("User not found")
      )
    },
    test(
      "should get a 'database unavailable' message when trying to get a user"
    ) {
      for {
        result <- userService.getUser(3L).either
      } yield assertTrue(
        result == Left("Database unavailable")
      )
    },
    test(
      "should get an 'interrupt' message when trying to get a user when an interruption occurs"
    ) {
      for {
        result <- userService.getUser(4L).either
      } yield assertTrue(
        result == Left("User request interrupted")
      )
    },
    test(
      "should get a 'request died' message when trying to get a user when the request died"
    ) {
      for {
        result <- userService.getUser(5L).either
      } yield assertTrue(
        result == Left("User request died")
      )
    }
  ).provide(
    ZLayer.succeed(new UserRepository())
  )
}
