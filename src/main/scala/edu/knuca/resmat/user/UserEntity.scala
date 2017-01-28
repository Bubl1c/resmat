package edu.knuca.resmat.user

case class UserTypeData(id: Int, name: String)

object UserType extends Enumeration {
  type UserType = Value
  val Student = Value(1, "Student")
  val Instructor = Value(2, "Instructor")
  val Admin = Value(3, "Admin")

  def valuesSet: Set[UserTypeData] = values.map(v => UserTypeData(v.id, v.toString))
}

case class AuthenticatedUser(id: Long, username: String, email: String, userType: UserType.UserType, userGroupId: Option[Long]) {
  val isStudent = userType == UserType.Student
  val isInstructor = userType == UserType.Instructor
  val isAdmin = userType == UserType.Admin
}

case class UserEntity(id: Option[Long] = None,
                      username: String,
                      password: String,
                      firstName: String,
                      lastName: String,
                      email: String,
                      userType: UserType.UserType,
                      userGroupId: Option[Long]) {
  require(!username.isEmpty, "username.empty")
  require(!password.isEmpty, "password.empty")
  require(!firstName.isEmpty, "firstName.empty")
  require(!lastName.isEmpty, "lastName.empty")
  require(!email.isEmpty, "email.empty")
}

case class UserGroupEntity(id: Option[Long] = None, name: String)

case class UserEntityUpdate(username: Option[String] = None,
                            password: Option[String] = None,
                            firstName: Option[String],
                            lastName: Option[String],
                            email: Option[String]) {
  def merge(user: UserEntity): UserEntity = {
    UserEntity(
      user.id,
      username.getOrElse(user.username),
      password.getOrElse(user.password),
      firstName.getOrElse(user.firstName),
      lastName.getOrElse(user.lastName),
      email.getOrElse(user.email),
      user.userType,
      user.userGroupId)
  }
}