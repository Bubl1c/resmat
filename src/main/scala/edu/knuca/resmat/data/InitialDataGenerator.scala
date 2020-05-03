package edu.knuca.resmat.data

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.articles.{ArticleDto, ArticleQueries, ArticleService}
import edu.knuca.resmat.auth.{AuthService, TokenEntity, TokensQueries}
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowConfAndExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.tests.TestConfService
import edu.knuca.resmat.user.{StudentGroupEntity, StudentGroupEntityUpdate, UserEntity, UserStudentGroupAccessDto, UserType, UsersService}
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object Data {
  val group1 = StudentGroupEntity(None, "Назва твоєї групи в університеті", false)
  val group2 = StudentGroupEntity(None, "ІО-41", false)
  val archivedGroup = StudentGroupEntity(None, "Група студентів заархівована", true)

  val userAdmin = UserEntity(None, "admin", "root", "Андрій", "Можаровський", "admin@email.com", UserType.Admin, "admin", None)
  val userAssistant = UserEntity(None, "assistant", "root", "Лаборант", "Лаборант", "assistant@email.com", UserType.Assistant, "assistant", None)
  val userInstructor = UserEntity(None, "instructor", "root", "Дмитро", "Левківський", "instructor@email.com", UserType.Instructor, "instructor", None)

  def userToken(userId: Long) =
    TokenEntity(None, userId, "3702dd845e4642659a3e7c930bc0fd37", DateTime.parse("2017-01-27T18:37:24.000+02:00").plusMinutes(userId.toInt), Some(DateTime.now().plusYears(100)))

  def student(goupId: Long, username: String, name: String, accessKey: String) =
    UserEntity(None, username, "root", name.split(" ")(0), name.split(" ")(1), s"$username@email.com", UserType.Student, accessKey, Some(goupId))

  object RingPlateExamConf {
    val ec: ExamConf = ExamConf(1, "Кільцева пластина - тести і задача", "Тут має бути детальний опис роботи та інструкції для студентів", 100)
    val testSetStep: ExamStepConf = ExamStepConf(-1, -1, 1, "Тестування", ExamStepType.TestSet, 5, 1, 3, 5, 20, ExamStepTestSetDataSet(1))
    val taskFlowStep: ExamStepConf = ExamStepConf(-1, -1, 2, "Розв'язання задачі", ExamStepType.TaskFlow, -1, 1, -1, 0, 80, ExamStepTaskFlowDataSet(1, 1))
    val resultsStep: ExamStepConf = ExamStepConf(-1, -1, 3, "Результати", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
  }

  object SimpleTestExamConf {
    val ec: ExamConf = ExamConf(2, "Тести питання", "Тут має бути детальний опис", 100)
    val testSetStep: ExamStepConf = ExamStepConf(-1, -1, 1, "Тестування", ExamStepType.TestSet, 5, 1, 3, 5, 20, ExamStepTestSetDataSet(2))
    val resultsStep: ExamStepConf = ExamStepConf(-1, -1, 2, "Результати", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
  }

  object OnlyTaskExamConf {
    val ec: ExamConf = ExamConf(1, "Кільцева пластина - задача", "Тут має бути детальний опис роботи та інструкції для студентів", 100)
    val taskFlowStep: ExamStepConf = ExamStepConf(-1, -1, 1, "Розв'язання задачі", ExamStepType.TaskFlow, -1, 1, -1, 0, 100, ExamStepTaskFlowDataSet(1, 1))
    val resultsStep: ExamStepConf = ExamStepConf(-1, -1, 2, "Результати", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
  }

  object CrossSectionExamConf {
    val ec: ExamConf = ExamConf(-1, "Геометрія - задача", "Зовсім нова задача", 100)
    val taskFlowStep: ExamStepConf = ExamStepConf(-1, -1, 1, "Розв'язання задачі", ExamStepType.TaskFlow, -1, 1, -1, 0, 100, ExamStepTaskFlowDataSet(1, 1))
    val resultsStep: ExamStepConf = ExamStepConf(-1, -1, 2, "Результати", ExamStepType.Results, -1, 0, -1, 0, 0, ExamStepResultsDataSet, false)
  }

  val problemConfs: List[(ProblemConf, Seq[ProblemVariantConf])] = List(
    (RingPlateData.Problem.conf, RingPlateData.Problem.variants),
    (CrossSectionData.Problem.conf, CrossSectionData.Problem.variants)
  )

  def userExam(examConfId: Long, userId: Long, curStepConfId: Long, status: ExamStatus = ExamStatus.Initial) =
    UserExam(-1, userId, examConfId, curStepConfId, status, None, started = Some(DateTime.now), None)

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
    ArticleQueries.parseMeta("{ \"uploadedFileUrls\": [] }")
  )
  val article2 = ArticleDto(
    -1,
    "Lorem Ipsum 2",
    "<h4>Что такое Lorem Ipsum?</h4><p style=\"text-align: center;\"><iframe src=\"//www.youtube.com/embed/9t3kU77C7H4\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></p>",
    "<div><h2><img style=\"display: block; margin-left: auto; margin-right: auto;\" src=\"https://www.sessions.edu/wp-content/uploads/1-PJgt-UmbqBXXGiDBL1KG0A.jpeg\" width=\"700\" height=\"399\" /></h2><h2>Почему он используется?</h2><p>Давно выяснено, что при оценке дизайна и композиции читаемый текст мешает сосредоточиться. Lorem Ipsum используют потому, что тот обеспечивает более или менее стандартное заполнение шаблона, а также реальное распределение букв и пробелов в абзацах, которое не получается при простой дубликации \"Здесь ваш текст.. Здесь ваш текст.. Здесь ваш текст..\" Многие программы электронной вёрстки и редакторы HTML используют Lorem Ipsum в качестве текста по умолчанию, так что поиск по ключевым словам \"lorem ipsum\" сразу показывает, как много веб-страниц всё ещё дожидаются своего настоящего рождения. За прошедшие годы текст Lorem Ipsum получил много версий. Некоторые версии появились по ошибке, некоторые - намеренно (например, юмористические варианты).</p></div><p style=\"text-align: center;\">&nbsp;<iframe src=\"//www.youtube.com/embed/9t3kU77C7H4\" width=\"560\" height=\"314\" allowfullscreen=\"allowfullscreen\"></iframe></p><div><h2>Откуда он появился?</h2><p>Многие думают, что Lorem Ipsum - взятый с потолка псевдо-латинский набор слов, но это не совсем так. Его корни уходят в один фрагмент классической латыни 45 года н.э., то есть более двух тысячелетий назад. Ричард МакКлинток, профессор латыни из колледжа Hampden-Sydney, штат Вирджиния, взял одно из самых странных слов в Lorem Ipsum, \"consectetur\", и занялся его поисками в классической латинской литературе. В результате он нашёл неоспоримый первоисточник Lorem Ipsum в разделах 1.10.32 и 1.10.33 книги \"de Finibus Bonorum et Malorum\" (\"О пределах добра и зла\"), написанной Цицероном в 45 году н.э. Этот трактат по теории этики был очень популярен в эпоху Возрождения. Первая строка Lorem Ipsum, \"Lorem ipsum dolor sit amet..\", происходит от одной из строк в разделе 1.10.32</p></div>",
    true,
    ArticleQueries.parseMeta("{ \"uploadedFileUrls\": [] }")
  )
}

class InitialDataGenerator(
  db: DatabaseService,
  usersService: UsersService,
  authService: AuthService,
  examConfService: ExamConfService,
  problemService: ProblemConfService,
  userExamService: UserExamService,
  testSetExamService: TestSetExamService,
  taskFlowExamService: TaskFlowConfAndExamService,
  testConfsService: TestConfService,
  articleService: ArticleService
) extends LazyLogging {

  def generate()(implicit executionContext: ExecutionContext) = {

    val article = articleService.create(Data.article)
    val article2 = articleService.create(Data.article2)

    Data.problemConfs.foreach { case ((pc: ProblemConf, pvcs: Seq[ProblemVariantConf])) =>
      generateProblemConf(pc, pvcs)
    }

    val testSet = new TestSetDataGenerator(testConfsService)
    val taskFlow = new TaskFlowDataGenerator(taskFlowExamService)

    val group = await(usersService.createStudentGroup(Data.group1))
    usersService.setArticlesToGroup(group.id.get, Seq(article.id))

    val group2 = await(usersService.createStudentGroup(Data.group2))

    val archivedGroup = await(usersService.createStudentGroup(Data.archivedGroup))
    await(usersService.updateStudentGroup(archivedGroup.id.get, StudentGroupEntityUpdate(archivedGroup.name, true)))

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
    
    usersService.setUserStudentGroupAccess(UserStudentGroupAccessDto(
      admin.id.get, Set(group.id.get, group2.id.get, archivedGroup.id.get)
    ))
    usersService.setUserStudentGroupAccess(UserStudentGroupAccessDto(
      assistant.id.get, Set(group.id.get)
    ))
    usersService.setUserStudentGroupAccess(UserStudentGroupAccessDto(
      instructor.id.get, Set(group.id.get, group2.id.get, archivedGroup.id.get)
    ))
    
    testConfsService.setUserTestGroupAccess(UserTestGroupAccessDto(
      admin.id.get, (testSet.archivedTestGroupConfs.map(_.id) ++ testSet.defaultTestSetGroupConfs.map(_.id) ++ testSet.simpleTestSetGroupConfs.map(_.id)).toSet
    ))
    testConfsService.setUserTestGroupAccess(UserTestGroupAccessDto(
      assistant.id.get, testSet.defaultTestSetGroupConfs.map(_.id).toSet
    ))
    testConfsService.setUserTestGroupAccess(UserTestGroupAccessDto(
      instructor.id.get, (testSet.archivedTestGroupConfs.map(_.id) ++ testSet.defaultTestSetGroupConfs.map(_.id) ++ testSet.simpleTestSetGroupConfs.map(_.id)).toSet
    ))

    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYxJjE0ODU1MzUxMDQwMDA
    val studentToken = insertToken(Data.userToken(student1.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYyJjE0ODU1MzUxNjQwMDA
    val adminToken = insertToken(Data.userToken(admin.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYzJjE0ODU1MzUyMjQwMDA
    val instructorToken = insertToken(Data.userToken(instructor.id.get))

    val simpleTestExamConf: ExamConfWithStepsDto = {
      val steps = Seq(
        ExamStepConfCreateDto(Data.SimpleTestExamConf.testSetStep, testSet.simpleNotInsertedTestSetConfDto),
        ExamStepConfCreateDto(Data.SimpleTestExamConf.resultsStep, ResultsConf)
      )
      val newEC = examConfService.createExamConfWithSteps(ExamConfCreateDto(Data.SimpleTestExamConf.ec, steps))
      newEC
    }

    val simpleArchivedTestExamConf: ExamConfWithStepsDto = {
      val steps = Seq(
        ExamStepConfCreateDto(Data.SimpleTestExamConf.testSetStep, testSet.simpleNotInsertedTestSetConfDto),
        ExamStepConfCreateDto(Data.SimpleTestExamConf.resultsStep, ResultsConf)
      )
      val newEC = examConfService.createExamConfWithSteps(ExamConfCreateDto(Data.SimpleTestExamConf.ec, steps))
      examConfService.setArchivedForExamConf(newEC.examConf.id, isArchived = true)
      newEC
    }

    val defaultExamConf: ExamConfWithStepsDto = {
      val steps = Seq(
        ExamStepConfCreateDto(Data.RingPlateExamConf.testSetStep, testSet.defaultNotInsertedTestSetConfDto),
        ExamStepConfCreateDto(Data.RingPlateExamConf.taskFlowStep, taskFlow.ringPlateNotInsertedTaskFlowConfDto),
        ExamStepConfCreateDto(Data.RingPlateExamConf.resultsStep, ResultsConf)
      )
      val newEC = examConfService.createExamConfWithSteps(ExamConfCreateDto(Data.RingPlateExamConf.ec, steps))
      newEC
    }

    val onlyTaskExamConf: ExamConfWithStepsDto = {
      val steps = Seq(
        ExamStepConfCreateDto(Data.OnlyTaskExamConf.taskFlowStep, taskFlow.ringPlateNotInsertedTaskFlowConfDto),
        ExamStepConfCreateDto(Data.OnlyTaskExamConf.resultsStep, ResultsConf)
      )
      val newEC = examConfService.createExamConfWithSteps(ExamConfCreateDto(Data.OnlyTaskExamConf.ec, steps))
      newEC
    }

    val crossSectionExamConf: ExamConfWithStepsDto = {
      val steps = Seq(
        ExamStepConfCreateDto(Data.CrossSectionExamConf.taskFlowStep, taskFlow.crossSectionNotInsertedTaskFlowConfDto),
        ExamStepConfCreateDto(Data.CrossSectionExamConf.resultsStep, ResultsConf)
      )
      val newEC = examConfService.createExamConfWithSteps(ExamConfCreateDto(Data.CrossSectionExamConf.ec, steps))
      newEC
    }

    examConfService.setUserExamConfAccess(UserExamConfAccessDto(admin.id.get,
      Set(simpleTestExamConf.examConf.id, simpleArchivedTestExamConf.examConf.id, defaultExamConf.examConf.id, onlyTaskExamConf.examConf.id, crossSectionExamConf.examConf.id)
    ))
    examConfService.setUserExamConfAccess(UserExamConfAccessDto(instructor.id.get,
      Set(simpleTestExamConf.examConf.id, simpleArchivedTestExamConf.examConf.id, defaultExamConf.examConf.id, onlyTaskExamConf.examConf.id, crossSectionExamConf.examConf.id)
    ))
    examConfService.setUserExamConfAccess(UserExamConfAccessDto(assistant.id.get,
      Set(simpleTestExamConf.examConf.id)
    ))

    userExamService.createUserExam(Data.userExam(crossSectionExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(crossSectionExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(onlyTaskExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(onlyTaskExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(simpleTestExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(simpleTestExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(defaultExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
    userExamService.createUserExam(Data.userExam(defaultExamConf.examConf.id, student1.id.get, crossSectionExamConf.firstStepId))
  }

  def insertToken(token: TokenEntity): TokenEntity = {
    db.run { implicit c =>
      val createdId: Option[Long] = TokensQueries.insert(token.userId, token.token, token.created, token.expires).executeInsert()
      token.copy(id = createdId)
    }
  }

  def generateProblemConf(pc: ProblemConf, pvcs: Seq[ProblemVariantConf]) = {
    val newPC = problemService.createProblemConf(pc)
    pvcs.foreach(pvc =>
      problemService.createProblemVariantConf(pvc.copy(problemConfId = newPC.id))
    )
  }

  def await[T](awaitable: Awaitable[T]): T = Await.result(awaitable, 5 seconds)

}
