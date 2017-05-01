package edu.knuca.resmat.data

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.auth.{AuthService, TokenEntity, TokensQueries}
import edu.knuca.resmat.core.RingPlateSolver
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.exam.{ProblemInputVariableConf => VarConf, ProblemInputVariableValue => VarVal}
import edu.knuca.resmat.user.{StudentGroupEntity, UserEntity, UserType, UsersService}
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object Data {
  val group1 = StudentGroupEntity(None, "ІП-41М")
  val group2 = StudentGroupEntity(None, "ІО-41")

  val userAdmin = UserEntity(None, "admin", "root", "Андрій", "Можаровський", "admin@email.com", UserType.Admin, "admin", None)
  val userInstructor = UserEntity(None, "instructor", "root", "Дмитро", "Левківський", "instructor@email.com", UserType.Instructor, "instructor", None)

  def userToken(userId: Long) =
    TokenEntity(None, userId, "3702dd845e4642659a3e7c930bc0fd37", DateTime.parse("2017-01-27T18:37:24.000+02:00").plusMinutes(userId.toInt), Some(DateTime.now().plusYears(100)))

  def student(goupId: Long, username: String, name: String, accessKey: String) =
    UserEntity(None, username, "root", name.split(" ")(0), name.split(" ")(1), s"$username@email.com", UserType.Student, accessKey, Some(goupId))

  val examConfs: Seq[(ExamConf, Seq[ExamStepConf])] = Seq(
    (ExamConf(1, "Exam1", "Exam1 description", 100), Seq(
      ExamStepConf(-1, -1, 1, "Exam1 Step1 Test Set", ExamStepType.TestSet, 200, 1, 3, 5, 20, ExamStepTestSetDataSet(1)),
      ExamStepConf(-1, -1, 2, "Exam1 Step2 Task Flow", ExamStepType.TaskFlow, -1, 1, -1, 0, 80, ExamStepTaskFlowDataSet(1, 1)),
      ExamStepConf(-1, -1, 3, "Exam1 Step3 Results", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
    ))
  )

  private val problemVariableConfs = Seq(
    VarConf(2, "Fa", "кН/м", "a.f"),
    VarConf(3, "Ma", "кНм/м", "a.m"),
    VarConf(4, "wa", "м", "a.w"),
    VarConf(5, "{phi}a", "рад", "a.fi"),

    VarConf(6, "E", "МПа", "moduleE"),
    VarConf(7, "{mu}", "", "poissonRatio"),
    VarConf(8, "q", "кН/м^2", "q"),

    VarConf(9, "Fb", "кН/м", "b.f"),
    VarConf(10, "Mb", "кНм/м", "b.m"),
    VarConf(11, "wb", "м", "b.w"),
    VarConf(12, "{phi}b", "рад", "b.fi"),

    VarConf(13, "a", "м", "a.length"),
    VarConf(14, "b", "м", "b.length"),
    VarConf(15, "t", "м", "height"),

    VarConf(16, "an", "", "a.n", false),
    VarConf(17, "bn", "", "b.n", false),
    VarConf(18, "sigmaAdm", "", "sigmaAdm", false)
  )

  private val problemVarValues: List[ProblemInputVariableValue] = List(
    VarVal(2, 0),
    VarVal(3, 0),
    VarVal(4, -0.01),
    VarVal(5, 0),

    VarVal(6, 200000000.0),
    VarVal(7, 0.3),
    VarVal(8, 0d),

    VarVal(9, 0),
    VarVal(10, 0),
    VarVal(11, 0),
    VarVal(12, 0),

    VarVal(13, 0.1),
    VarVal(14, 1.1),
    VarVal(15, 0.02),

    VarVal(16, 1),
    VarVal(17, 2),
    VarVal(18, 160)
  )

  val problemConfs: List[(ProblemConf, Seq[ProblemVariantConf])] = List(
    (ProblemConf(1, "Кільцева пластина", ProblemType.RingPlate, problemVariableConfs), Seq(
      ProblemVariantConf(1, 1, "img/tasks/9.png",
        problemVarValues,
        new RingPlateSolver(problemVariableConfs, problemVarValues).solve()
      )
    ))
  )

  def userExam(examConfId: Long, userId: Long, status: ExamStatus = ExamStatus.Initial) =
    UserExam(-1, userId, examConfId, 1, status, None, started = Some(DateTime.now), None)
}

class InitialDataGenerator(db: DatabaseService,
                           usersService: UsersService,
                           authService: AuthService,
                           examService: ExamService,
                           problemService: ProblemService,
                           userExamService: UserExamService,
                           testSetExamService: TestSetExamService,
                           taskFlowExamService: TaskFlowExamService) extends LazyLogging {

  def generate()(implicit executionContext: ExecutionContext) = {

    val testSet = new TestSetDataGenerator(testSetExamService)

    val group = await(usersService.createStudentGroup(Data.group1))
    val group2 = await(usersService.createStudentGroup(Data.group2))

    val student1 = await(
      usersService.createUser(Data.student(group.id.get, "student1", "Іван Іванов", "1"))
    )
    val student2 = await(
      usersService.createUser(Data.student(group2.id.get, "student2", "Петро Петренко", "2"))
    )
    val student3 = await(
      usersService.createUser(Data.student(group2.id.get, "student3", "Максим Максименко", "3"))
    )
    val admin = await(usersService.createUser(Data.userAdmin))
    val instructor = await(usersService.createUser(Data.userInstructor))

    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYxJjE0ODU1MzUxMDQwMDA
    val studentToken = insertToken(Data.userToken(student1.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYyJjE0ODU1MzUxNjQwMDA
    val adminToken = insertToken(Data.userToken(admin.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYzJjE0ODU1MzUyMjQwMDA
    val instructorToken = insertToken(Data.userToken(instructor.id.get))

    val examConfs = Data.examConfs.map{ case((ec: ExamConf, steps: Seq[ExamStepConf])) =>
      generateExamConf(ec, steps)
    }

    Data.problemConfs.foreach{ case((pc: ProblemConf, pvcs: Seq[ProblemVariantConf])) =>
      generateProblemConf(pc, pvcs)
    }

    val taskFlow = new TaskFlowDataGenerator(taskFlowExamService)

    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get, ExamStatus.InProgress))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get, ExamStatus.Success))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get, ExamStatus.Failed))
  }

  def insertToken(token: TokenEntity): TokenEntity = {
    db.run{ implicit c =>
      val createdId: Option[Long] = TokensQueries.insert(token.userId, token.token, token.created, token.expires).executeInsert()
      token.copy(id = createdId)
    }
  }

  def generateExamConf(ec: ExamConf, esc: Seq[ExamStepConf]): ExamConf = {
    val newEC = examService.createExamConf(ec)
    esc.foreach{case (esc: ExamStepConf) => {
      val newESC = examService.createExamStepConf(esc.copy(examConfId = newEC.id))
    }}
    newEC
  }

  def generateProblemConf(pc: ProblemConf, pvcs: Seq[ProblemVariantConf]) = {
    val newPC = problemService.createProblemConf(pc)
    pvcs.foreach(pvc =>
      problemService.createProblemVariantConf(pvc.copy(problemConfId = newPC.id))
    )
  }

  def await[T](awaitable: Awaitable[T]): T = Await.result(awaitable, 5 seconds)

}
