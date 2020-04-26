package edu.knuca.resmat.user

case class UserTypeData(id: Int, name: String)

/**
  * Enumeration for user types
  * usages:
  * id                - to store in DB
  * name = toString   - to send to UI
  * Value = UserType  - to use in business logic
  */
object UserType extends Enumeration {
  type UserType = Value
  val Student = Value(100, "student")
  val Assistant = Value(150, "assistant")
  val Instructor = Value(200, "instructor")
  val Admin = Value(300, "admin")

  def valuesSet: Set[UserTypeData] = values.map(v => UserTypeData(v.id, v.toString))
}

case class AuthenticatedUser(id: Long, username: String, email: String, userType: UserType.UserType, userGroupId: Option[Long]) {
  val isStudent = userType == UserType.Student
  val isInstructor = userType == UserType.Instructor
  val isInstructorOrHigher = userType >= UserType.Instructor
  val isAssistantOrHigher = userType >= UserType.Assistant
  val isAdmin = userType == UserType.Admin
  val notStudent = userType != UserType.Student
}

case class UserEntity(id: Option[Long] = None,
                      username: String,
                      password: String,
                      firstName: String,
                      lastName: String,
                      email: String,
                      userType: UserType.UserType,
                      accessKey: String,
                      studentGroupId: Option[Long]) {
  require(!username.isEmpty, "username.empty")
  require(!firstName.isEmpty, "firstName.empty")
  require(!accessKey.isEmpty, "accessKey.empty")
}

case class StudentGroupEntityUpdate(name: String, isArchived: Boolean)

case class StudentGroupEntity(id: Option[Long] = None, name: String, isArchived: Boolean)

case class UserEntityUpdate(username: Option[String] = None,
                            password: Option[String] = None,
                            firstName: Option[String] = None,
                            lastName: Option[String] = None,
                            email: Option[String] = None,
                            accessKey: Option[String] = None) {
  def merge(user: UserEntity): UserEntity = {
    UserEntity(
      user.id,
      username.getOrElse(user.username),
      password.getOrElse(user.password),
      firstName.getOrElse(user.firstName),
      lastName.getOrElse(user.lastName),
      email.getOrElse(user.email),
      user.userType,
      accessKey.getOrElse(user.accessKey),
      user.studentGroupId)
  }
}