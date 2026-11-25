package fr.p4uline.kata.zio.concurrency

import zio._

case class User(id: Long, name: String)

case class Order(id: Long, amount: Double)

case class Notification(id: Long, message: String)

case class UserDashboard(
    user: User,
    orders: List[Order],
    notifications: List[Notification]
)

sealed trait DashboardError {
  def message: String
}

case class UserNotFound() extends DashboardError {
  val message = "User not found"
}

case class OrdersUnavailable() extends DashboardError {
  val message = "Orders unavailable"
}

case class NotificationsUnavailable() extends DashboardError {
  val message = "Notifications unavailable"
}
trait UserRepository {
  def findUser(id: Long): IO[DashboardError, User]
}
class UserRepositoryImpl extends UserRepository {
  override def findUser(id: Long): IO[DashboardError, User] =
    ZIO.succeed(User(id, "Pauline"))
}

trait OrderRepository {
  def findOrders(userId: Long): IO[DashboardError, List[Order]]
}
class OrderRepositoryImpl extends OrderRepository {
  override def findOrders(userId: Long): IO[DashboardError, List[Order]] =
    ZIO.succeed(List(Order(1, 42.0), Order(2, 89.0)))
}

trait NotificationRepository {
  def findNotifications(userId: Long): IO[DashboardError, List[Notification]]
}
class NotificationRepositoryImpl extends NotificationRepository {
  override def findNotifications(
      userId: Long
  ): IO[DashboardError, List[Notification]] =
    ZIO.succeed(List(Notification(1, "Welcome!")))
}

trait UserDashboardService {
  def getDashboard(userId: Long): IO[DashboardError, UserDashboard]
}
class UserDashboardServiceImpl(
    userRepository: UserRepository,
    orderRepository: OrderRepository,
    notificationRepository: NotificationRepository
) extends UserDashboardService {

  override def getDashboard(
      userId: Long
  ): zio.IO[DashboardError, UserDashboard] = {
    // FIXME do it parallel not sequential
    for {
      user <- userRepository.findUser(123L)
      orders <- orderRepository.findOrders(user.id)
      notifications <- notificationRepository.findNotifications(user.id)
    } yield {
      UserDashboard(user = user, orders = orders, notifications = notifications)
    }
  }
}
