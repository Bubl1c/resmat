case class UserTypeData(id: Int, name: String)
object UserType extends Enumeration {
  type UserType = Value
  val Student = Value(1, "Student")
  val Instructor = Value(2, "Instructor")
  val Admin = Value(3, "Admin")

  def valuesSet: Set[UserTypeData] = values.map(v => UserTypeData(v.id, v.toString))
}

UserType.Admin.toString

UserType(1)

UserType.valuesSet

def g(ut: UserType.UserType) = ut + ""

g(UserType.Admin)

val i = Option("str")

i.map(str => null)