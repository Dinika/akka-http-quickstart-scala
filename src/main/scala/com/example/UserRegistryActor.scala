package com.example

//#user-registry-actor
import akka.actor.{ Actor, ActorLogging, Props }
import java.text.SimpleDateFormat
import java.util.Calendar
//#user-case-classes
final case class User(name: String, age: Int, countryOfResidence: String)
final case class Users(users: Seq[User])
final case class Time(time: String)
//#user-case-classes

object UserRegistryActor {
  final case class ActionPerformed(description: String)
  final case object GetUsers
  final case class CreateUser(user: User)
  final case class GetUser(name: String)
  final case class DeleteUser(name: String)
  final case object GetCurrentTime

  def props: Props = Props[UserRegistryActor]
}

class UserRegistryActor extends Actor with ActorLogging {
  import UserRegistryActor._

  var users = Set.empty[User]
  var time = ""
  def receive: Receive = {
    case GetUsers =>
      User
      sender() ! Users(users.toSeq)
    case CreateUser(user) =>
      users += user
      sender() ! ActionPerformed(s"User ${user.name} created.")
    case GetUser(name) =>
      sender() ! users.find(_.name == name)
    case DeleteUser(name) =>
      users.find(_.name == name) foreach { user => users -= user }
      sender() ! ActionPerformed(s"User ${name} deleted.")
    case GetCurrentTime =>
      val today = Calendar.getInstance().getTime()
      // create the date/time formatters
      val minuteFormat = new SimpleDateFormat("mm")
      val hourFormat = new SimpleDateFormat("hh")
      val amPmFormat = new SimpleDateFormat("a")

      val currentHour = hourFormat.format(today) // 12
      val currentMinute = minuteFormat.format(today) // 29
      val amPm = amPmFormat.format(today)
      time = time.concat(currentHour)
      time = time.concat(currentMinute)
      time = time.concat(amPm)
      sender() ! Time(time)
  }
}
//#user-registry-actor