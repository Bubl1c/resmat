package edu.knuca.resmat.data

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.articles.{ArticleDto, ArticleQueries, ArticleService}
import edu.knuca.resmat.auth.{AuthService, TokenEntity, TokensQueries}
import edu.knuca.resmat.core.RingPlateSolver
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.exam.{ProblemInputVariableConf => VarConf, ProblemInputVariableValue => VarVal}
import edu.knuca.resmat.tests.TestConfsService
import edu.knuca.resmat.user.{StudentGroupEntity, UserEntity, UserType, UsersService}
import org.joda.time.DateTime
import io.circe.parser.parse

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object Data {
  val group1 = StudentGroupEntity(None, "Назва твоєї групи в університеті")
  val group2 = StudentGroupEntity(None, "ІО-41")

  val userAdmin = UserEntity(None, "admin", "root", "Андрій", "Можаровський", "admin@email.com", UserType.Admin, "admin", None)
  val userAssistant = UserEntity(None, "assistant", "root", "Лаборант", "Лаборант", "assistant@email.com", UserType.Assistant, "assistant", None)
  val userInstructor = UserEntity(None, "instructor", "root", "Дмитро", "Левківський", "instructor@email.com", UserType.Instructor, "instructor", None)

  def userToken(userId: Long) =
    TokenEntity(None, userId, "3702dd845e4642659a3e7c930bc0fd37", DateTime.parse("2017-01-27T18:37:24.000+02:00").plusMinutes(userId.toInt), Some(DateTime.now().plusYears(100)))

  def student(goupId: Long, username: String, name: String, accessKey: String) =
    UserEntity(None, username, "root", name.split(" ")(0), name.split(" ")(1), s"$username@email.com", UserType.Student, accessKey, Some(goupId))

  val examConfs: Seq[(ExamConf, Seq[ExamStepConf])] = Seq(
    (ExamConf(1, "Назва залікової роботи", "Тет має бути детальний опис роботи та інструкції для студентів", 100), Seq(
      ExamStepConf(-1, -1, 1, "Тестування", ExamStepType.TestSet, 5, 1, 3, 5, 20, ExamStepTestSetDataSet(1)),
      ExamStepConf(-1, -1, 2, "Розв'язання задачі", ExamStepType.TaskFlow, -1, 1, -1, 0, 80, ExamStepTaskFlowDataSet(1, 1)),
      ExamStepConf(-1, -1, 3, "Результати", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
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
    VarConf(18, "{sigma}адм", "", "sigmaAdm")
  )

  private val problemVarValues: List[ProblemInputVariableValue] = List(
    VarVal(2, 0),
    VarVal(3, 0),
    VarVal(4, -0.01),
    VarVal(5, 0),

    VarVal(6, 200000.0),
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

  private val problemVarValues2: List[ProblemInputVariableValue] = List(
    VarVal(2, 0),
    VarVal(3, 0),
    VarVal(4, -0.01),
    VarVal(5, 0),

    VarVal(6, 200000.0),
    VarVal(7, 0.3),
    VarVal(8, 0d),

    VarVal(9, 0),
    VarVal(10, 0),
    VarVal(11, 0),
    VarVal(12, 0),

    VarVal(13, 0.1),
    VarVal(14, 1.1),
    VarVal(15, 0.02),

    VarVal(16, 2),
    VarVal(17, 2),
    VarVal(18, 160)
  )

  val problemConfs: List[(ProblemConf, Seq[ProblemVariantConf])] = List(
    (ProblemConf(1, "Кільцева пластина", ProblemType.RingPlate, problemVariableConfs), Seq(
      ProblemVariantConf(1, 1, "img/tasks/variants/sc1.png",
        problemVarValues,
        new RingPlateSolver(problemVariableConfs, problemVarValues).solve()
      ),
      ProblemVariantConf(1, 1, "img/tasks/variants/sc2.png",
        problemVarValues2,
        new RingPlateSolver(problemVariableConfs, problemVarValues2).solve()
      )
    ))
  )

  def userExam(examConfId: Long, userId: Long, status: ExamStatus = ExamStatus.Initial) =
    UserExam(-1, userId, examConfId, 1, status, None, started = Some(DateTime.now), None)

  val article = ArticleDto(
    -1,
    "Lorem Ipsum",
    "<h2>Что такое Lorem Ipsum?</h2>\n<p style=\"text-align: center;\"><strong><img style=\"float: left;\" " +
      "src=\"http://www.jqueryscript.net/images/Universal-Placeholder-Text-Lorem-Ipsum-Generator-getlorem.jpg\" " +
      "width=\"443\" height=\"323\" /></strong></p>\n<p style=\"text-align: center;\">&nbsp;</p>\n<p style=\"text-align: " +
      "center;\">&nbsp;</p><p style=\"text-align: center;\"><strong>Lorem Ipsum</strong>&nbsp;- это текст-\"рыба\", " +
      "часто используемый в печати и вэб-дизайне. Lorem Ipsum является стандартной \"рыбой\" для текстов на латинице с начала XVI века. " +
      "В то время некий безымянный печатник создал большую коллекцию размеров и форм шрифтов, используя Lorem Ipsum для распечатки образцов. " +
      "Lorem Ipsum не только успешно пережил без заметных изменений пять веков, но и перешагнул в электронный дизайн. " +
      "Его популяризации в новое время послужили публикация листов Letraset с образцами Lorem Ipsum в 60-х годах и, " +
      "в более недавнее время, программы электронной вёрстки типа Aldus PageMaker, в шаблонах которых используется " +
      "Lorem Ipsum.</p>",
    "<div><h2><img style=\"display: block; margin-left: auto; margin-right: auto;\" src=\"https://www.sessions.edu/wp-content/uploads/1-PJgt-UmbqBXXGiDBL1KG0A.jpeg\" width=\"700\" height=\"399\" /></h2><h2>Почему он используется?</h2><p>Давно выяснено, что при оценке дизайна и композиции читаемый текст мешает сосредоточиться. Lorem Ipsum используют потому, что тот обеспечивает более или менее стандартное заполнение шаблона, а также реальное распределение букв и пробелов в абзацах, которое не получается при простой дубликации \"Здесь ваш текст.. Здесь ваш текст.. Здесь ваш текст..\" Многие программы электронной вёрстки и редакторы HTML используют Lorem Ipsum в качестве текста по умолчанию, так что поиск по ключевым словам \"lorem ipsum\" сразу показывает, как много веб-страниц всё ещё дожидаются своего настоящего рождения. За прошедшие годы текст Lorem Ipsum получил много версий. Некоторые версии появились по ошибке, некоторые - намеренно (например, юмористические варианты).</p></div><p style=\"text-align: center;\">&nbsp;<iframe src=\"//www.youtube.com/embed/9t3kU77C7H4\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></p><div><h2>Откуда он появился?</h2><p>Многие думают, что Lorem Ipsum - взятый с потолка псевдо-латинский набор слов, но это не совсем так. Его корни уходят в один фрагмент классической латыни 45 года н.э., то есть более двух тысячелетий назад. Ричард МакКлинток, профессор латыни из колледжа Hampden-Sydney, штат Вирджиния, взял одно из самых странных слов в Lorem Ipsum, \"consectetur\", и занялся его поисками в классической латинской литературе. В результате он нашёл неоспоримый первоисточник Lorem Ipsum в разделах 1.10.32 и 1.10.33 книги \"de Finibus Bonorum et Malorum\" (\"О пределах добра и зла\"), написанной Цицероном в 45 году н.э. Этот трактат по теории этики был очень популярен в эпоху Возрождения. Первая строка Lorem Ipsum, \"Lorem ipsum dolor sit amet..\", происходит от одной из строк в разделе 1.10.32</p></div>",
    true,
    ArticleQueries.parseMeta("{ \"uploadedFileUrls\": [] }"))
  val article2 = ArticleDto(
    -1,
    "Lorem Ipsum 2",
    "<h4>Что такое Lorem Ipsum?</h4><p style=\"text-align: center;\"><iframe src=\"//www.youtube.com/embed/9t3kU77C7H4\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></p>",
    "<div><h2><img style=\"display: block; margin-left: auto; margin-right: auto;\" src=\"https://www.sessions.edu/wp-content/uploads/1-PJgt-UmbqBXXGiDBL1KG0A.jpeg\" width=\"700\" height=\"399\" /></h2><h2>Почему он используется?</h2><p>Давно выяснено, что при оценке дизайна и композиции читаемый текст мешает сосредоточиться. Lorem Ipsum используют потому, что тот обеспечивает более или менее стандартное заполнение шаблона, а также реальное распределение букв и пробелов в абзацах, которое не получается при простой дубликации \"Здесь ваш текст.. Здесь ваш текст.. Здесь ваш текст..\" Многие программы электронной вёрстки и редакторы HTML используют Lorem Ipsum в качестве текста по умолчанию, так что поиск по ключевым словам \"lorem ipsum\" сразу показывает, как много веб-страниц всё ещё дожидаются своего настоящего рождения. За прошедшие годы текст Lorem Ipsum получил много версий. Некоторые версии появились по ошибке, некоторые - намеренно (например, юмористические варианты).</p></div><p style=\"text-align: center;\">&nbsp;<iframe src=\"//www.youtube.com/embed/9t3kU77C7H4\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></p><div><h2>Откуда он появился?</h2><p>Многие думают, что Lorem Ipsum - взятый с потолка псевдо-латинский набор слов, но это не совсем так. Его корни уходят в один фрагмент классической латыни 45 года н.э., то есть более двух тысячелетий назад. Ричард МакКлинток, профессор латыни из колледжа Hampden-Sydney, штат Вирджиния, взял одно из самых странных слов в Lorem Ipsum, \"consectetur\", и занялся его поисками в классической латинской литературе. В результате он нашёл неоспоримый первоисточник Lorem Ipsum в разделах 1.10.32 и 1.10.33 книги \"de Finibus Bonorum et Malorum\" (\"О пределах добра и зла\"), написанной Цицероном в 45 году н.э. Этот трактат по теории этики был очень популярен в эпоху Возрождения. Первая строка Lorem Ipsum, \"Lorem ipsum dolor sit amet..\", происходит от одной из строк в разделе 1.10.32</p></div>",
    true,
    ArticleQueries.parseMeta("{ \"uploadedFileUrls\": [] }"))
}

class InitialDataGenerator(db: DatabaseService,
                           usersService: UsersService,
                           authService: AuthService,
                           examService: ExamService,
                           problemService: ProblemService,
                           userExamService: UserExamService,
                           testSetExamService: TestSetExamService,
                           taskFlowExamService: TaskFlowExamService,
                           testConfsService: TestConfsService,
                           articleService: ArticleService) extends LazyLogging {

  def generate()(implicit executionContext: ExecutionContext) = {

    val article = articleService.create(Data.article)
    val article2 = articleService.create(Data.article2)

    val testSet = new TestSetDataGenerator(testConfsService)

    val group = await(usersService.createStudentGroup(Data.group1))
    usersService.setArticlesToGroup(group.id.get, Seq(article.id))

    val group2 = await(usersService.createStudentGroup(Data.group2))

    val student1 = await(
      usersService.createUser(Data.student(group.id.get, "lev", "Дмитро Левківський", "1"))
    )
    val student2 = await(
      usersService.createUser(Data.student(group2.id.get, "student2", "Петро Петренко", "2"))
    )
    val student3 = await(
      usersService.createUser(Data.student(group2.id.get, "student3", "Максим Максименко", "3"))
    )
    val admin = await(usersService.createUser(Data.userAdmin))
    val assistant = await(usersService.createUser(Data.userAssistant))
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
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
    userExamService.createUserExam(Data.userExam(examConfs.head.id, student1.id.get))
  }

  def insertToken(token: TokenEntity): TokenEntity = {
    db.run{ implicit c =>
      val createdId: Option[Long] = TokensQueries.insert(token.userId, token.token, token.created, token.expires).executeInsert()
      token.copy(id = createdId)
    }
  }

  def generateExamConf(ec: ExamConf, esc: Seq[ExamStepConf]): ExamConf = {
    val newEC = examService.createExamConfWithSteps(ExamConfDto(ec, esc))
    newEC.examConf
  }

  def generateProblemConf(pc: ProblemConf, pvcs: Seq[ProblemVariantConf]) = {
    val newPC = problemService.createProblemConf(pc)
    pvcs.foreach(pvc =>
      problemService.createProblemVariantConf(pvc.copy(problemConfId = newPC.id))
    )
  }

  def await[T](awaitable: Awaitable[T]): T = Await.result(awaitable, 5 seconds)

}
