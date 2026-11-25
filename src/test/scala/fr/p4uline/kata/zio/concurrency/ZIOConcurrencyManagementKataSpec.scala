package fr.p4uline.kata.zio.concurrency

import zio.{IO, Promise, Scope, ZIO, durationInt}
import zio.test.{Spec, TestClock, TestEnvironment, ZIOSpecDefault, assertTrue}

object ZIOConcurrencyManagementKataSpec extends ZIOSpecDefault {
  val userRepository = new UserRepository {
    override def findUser(id: Long): IO[DashboardError, User] =
      ZIO.succeed(User(123L, "toto"))
  }

  def spec: Spec[TestEnvironment & Scope, Any] =
    suite("UserDashboardService")(test("should get successfully the user") {
      for {
        orderPromise <- Promise.make[Nothing, Unit]
        notificationPromise <- Promise.make[Nothing, Unit]
        orderRepository = new OrderRepository {
          override def findOrders(
              userId: Long
          ): IO[DashboardError, List[Order]] = {
            for {
              _ <- orderPromise.succeed(())
              _ <- notificationPromise.await
            } yield List(Order(userId, 5.0))
          }
        }
        notificationRepository = new NotificationRepository {
          override def findNotifications(
              userId: Long
          ): zio.IO[DashboardError, List[Notification]] = {
            for {
              _ <- notificationPromise.succeed(())
              _ <- orderPromise.await
            } yield List(Notification(id = 123L, message = "message pour 123"))
          }
        }
        dashboardService = new UserDashboardServiceImpl(
          userRepository,
          orderRepository,
          notificationRepository
        )

        fiber <- dashboardService.getDashboard(123L).timeout(2.seconds).fork
        _ <- TestClock.adjust(2.seconds)
        result <- fiber.join
      } yield assertTrue(
        result.exists(_.user == User(id = 123L, name = "toto"))
      )
    })
}
