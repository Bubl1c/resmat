package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.testset.TestSetExamService

object TestSetData {
  val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, "Набір тестів для крутих студентів", 50)
  )

  val testGroupConfs: List[(TestGroupConf, Seq[TestConf])] = List(
    (TestGroupConf(-1, "Знання змінних та коефіцієнтів, гіпотез та об’єктів теорії пружності"), Seq(
      test("Коефіцієнт Пуассона – це", Seq(
        opt("Міра зміни поперечних розмірів ізотропного тіла при деформації розтягу", true),
        opt("Міра зміни відносної деформації по відношенню до нормального напруження"),
        opt("Міра зміни видовження ізотропного тіла при деформації розтягу"),
        opt("Відношення нормальних напружень при розтягу до поперечної деформації")
      ))/*,
      test("Модуль пружності при осьовому розтягу-стиску - це", Seq(
        opt("Модуль зсуву"),
        opt("Модуль Юнга", true),
        opt("Модуль об’ємної пружності"),
        opt("Модуль деформації")
      )),
      test("Закон Гука описує", Seq(
        opt("Зв'язок між напруженнями і переміщеннями в пружній стадії"),
        opt("Залежність між зусиллями і напруженнями"),
        opt("Зв'язок між деформаціями і напруженнями в пружній стадії", true),
        opt("Взаємозв’язок між деформаціями пружного тіла до його переміщень")
      )),
      test("Рівняння Коші описують", Seq(
        opt("Зв'язок між напруженнями і переміщеннями"),
        opt("Зв'язок між переміщеннями та деформаціями", true),
        opt("Зв'язок між деформаціями і напруженнями"),
        opt("Взаємозв’язок між зусиллями та напруженнями")
      )),
      test("Рівняння Нав’є – це", Seq(
        opt("Рівняння рівноваги тіла", true),
        opt("Рівняння, що описують зміну напружень"),
        opt("Рівняння пружної взаємодії між тілами"),
        opt("Рівняння, що описують деформацію Нав’є")
      )),
      test("Яка з гіпотез класичної теорії тонких пластин сформульована правильно?", Seq(
        opt("При деформації пластина не змінює свою товщину, це означає, що деформація $\\sigma_z = 0$ і нормальні напруження $\\varepsilon_z$ залежать від осі $z$"),
        opt("При деформації пластина змінює свою товщину, це означає, що деформація $\\varepsilon_z = 0$ і нормальні напруження $\\sigma_z$ не залежать від осі $z$"),
        opt("При деформації пластина не змінює своєї товщини, це означає, що деформація $\\varepsilon_z \\neq 0$ і нормальні напруження $\\sigma_x$ не залежать від осі $z$"),
        opt("При деформації пластина не змінює своєї товщини, це означає, що деформація $\\varepsilon_z = 0$ і вертикальні переміщення w не залежать від осі $z$", true)
      )),
      test("Яка з гіпотез класичної теорії тонких пластин сформульована правильно?", Seq(
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації, залишаються після деформації прямолінійними і перпендикулярними до зігнутої поверхні, зберігаючи при цьому свою довжину", true),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації, залишаються під час деформації прямолінійними і перпендикулярними до зігнутої поверхні, та змінюють при цьому свою довжину"),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації, залишаються після деформації прямолінійними, повертаючись до зігнутої поверхні на певний кут"),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації залишаються перпендикулярними до зігнутої поверхні та набувають криволінійну форму")
      )),
      test("Яка гіпотеза відноситься до гіпотез Кірхгофа", Seq(
        opt("Якщо в будь-якій малій частині тіла прикладена зрівноважена система сила, то вона викликає в тілі напруження, які швидко зменшуються при віддаленні від даної частини"),
        opt("Прямолінійні волокна, які до деформації були нормальними до серединної поверхні, зберігають після деформації свою довжину та прямолінійність, але не залишаються перпендикулярними до деформованої серединної поверхні, при цьому повертаються на певний кут"),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації, залишаються після деформації прямолінійними і перпендикулярними до зігнутої поверхні, зберігаючи при цьому свою довжину"),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації залишаються перпендикулярними до зігнутої поверхні та набувають криволінійну форму")
      )),
      test("Яке з припущень відноситься до некласичної теорії пластин (припущення Тимошенко)?", Seq(
        opt("Якщо в будь-якій малій частині тіла прикладена зрівноважена система сила, то вона викликає в тілі напруження, які швидко зменшуються при віддаленні від даної частини"),
        opt("Прямолінійні волокна, які до деформації були нормальними до серединної поверхні, зберігають після деформації свою довжину та прямолінійність, але не залишаються перпендикулярними до деформованої серединної поверхні, при цьому повертаючись на певний кут", true),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації, залишаються після деформації прямолінійними і перпендикулярними до зігнутої поверхні, зберігаючи при цьому свою довжину"),
        opt("Прямолінійні волокна, перпендикулярні до серединної поверхні пластини до деформації залишаються перпендикулярними до зігнутої поверхні та набувають криволінійну форму")
      )),
      test("Модуль пружності при розтягу-стиску – це", Seq(
        opt("тангенс кута нахилу прямолінійної ділянки діаграми розтягу-стиску зразка до горизонтальної осі діаграми", true),
        opt("фізична характеристика теплопровідності матеріалу"),
        opt("відношення напружень до переміщень"),
        opt("відношення деформацій до напружень при розтягу-стиску")
      ))*/
    )),
    (TestGroupConf(-1, "Формули"), Seq(
      test("У чому вимірюється модуль Юнга?", Seq(
        opt("$[кН*м]$"),
        opt("$[кН*м^2]$"),
        opt("$[кН/м]$"),
        opt("$[кН/м^2]$", true)
      ))/*,
      test("У чому вимірюється коефіцієнт Пуассона?", Seq(
        opt("$кН*м$"),
        opt("$м^2$"),
        opt("$1/м$"),
        opt("безрозмірна величина", true)
      )),
      test("У чому вимірюється коловий момент кільцевої пластини?", Seq(
        opt("$кН*м/м$", true),
        opt("безрозмірна величина"),
        opt("$кН*м$"),
        opt("$кН*м^2$")
      )),
      test("У чому вимірюється радіальний момент кільцевої пластини?", Seq(
        opt("$кН*м/м$", true),
        opt("безрозмірна величина"),
        opt("$кН*м$"),
        opt("$кН*м^2$")
      )),
      test("У чому вимірюється поперечна сила кільцевої пластини?", Seq(
        opt("$кН*м/м$"),
        opt("безрозмірна величина"),
        opt("$кН/м$", true),
        opt("$кН*м$")
      )),
      test("Формула для визначення циліндричної жорсткості", Seq(
        opt("$D=\\frac{E \\cdot 1 \\cdot h^3}{12 \\cdot (1 - \\mu^2)}кНм$", true),
        opt("$D=\\frac{E \\cdot 1 \\cdot h^3}{12 \\cdot (1 - \\mu)}кНм^2$"),
        opt("$D=\\frac{E \\cdot b \\cdot h^2}{12 \\cdot (1 + \\mu^2)}кНм$"),
        opt("$D=\\frac{E \\cdot 1 \\cdot h^3}{12 \\cdot (1 + \\mu)}кНм^2$")
      )),
      test("Формула для визначення коефіцієнта Пуассона", Seq(
        opt("$\\mu = |\\frac{\\varepsilon_{розтягу}}{\\varepsilon_{стиску}}|$"),
        opt("$\\mu = |\\frac{\\varepsilon_{поздовжня}}{\\varepsilon_{поперечна}}|$"),
        opt("$\\mu = |\\frac{\\varepsilon_{поперечна}}{\\varepsilon_{поздовжня}}|$", true),
        opt("$\\mu = |\\frac{\\sigma_x}{\\varepsilon_x}|$")
      )),
      test("Формула для визначення максимальних дотичних напружень в кільцевій пластині", Seq(
        opt("$\\tau_{max} = \\frac{E \\cdot 1 \\cdot h^3}{12 \\cdot (1 - \\mu^2)}$"),
        opt("$\\tau_{max} = -\\frac{3Q_r}{2h}$", true),
        opt("$\\tau_{max} = -\\frac{3M_r}{2h}$"),
        opt("$\\tau_{max} = -\\frac{3Q_r}{4h}$")
      )),
      test("Формула для визначення максимального радіального нормального напруження", Seq(
        opt("$\\sigma_r = \\frac{6M_r}{12 \\cdot (1 - \\mu^2)}$"),
        opt("$\\sigma_r = \\frac{M_r}{12 \\cdot h^2}$"),
        opt("$\\sigma_r = \\frac{6M_r}{h^2}$", true),
        opt("$\\sigma_r = \\frac{12M_r}{bh^2}$")
      )),
      test("Формула для визначення максимального колового нормального напруження", Seq(
        opt("$\\sigma_\\theta = \\frac{6M_\\theta}{12 \\cdot (1 - \\mu^2)}$"),
        opt("$\\sigma_\\theta = \\frac{M_\\theta}{12h2}$"),
        opt("$\\sigma_\\theta = \\frac{12M_\\theta}{bh^2}$"),
        opt("$\\sigma_\\theta = \\frac{6M_\\theta}{h^2}$", true)
      ))*/
    )),
    (TestGroupConf(3, "Перевірка граничних умов"), Seq(
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(null, null, 0d, 0d,   0d, null, 0d, null, true),
        cond(null, 0d, 0d, 0d,   0d, null, 0d, null),
        cond(null, null, 0d, 0d,   0d, 0d, 0d, 0d)
      ), "img/tests/extreme-conditions/ec1.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0d, null, 0d, null,   0d, null, 0d, null, true),
        cond(0d, 0d, null, 0d,   0d, null, 0d, null),
        cond(null, null, 0d, 0d,   0d, null, 0d, 0d)
      ), "img/tests/extreme-conditions/ec2.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0d, null, 0d, null,   null, null, 0d, 0d, true),
        cond(0d, null, 0d, null,   0d, null, 0d, null),
        cond(null, null, 0d, 0d,   0d, null, 0d, null)
      ), "img/tests/extreme-conditions/ec3.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0d, 0d, null, null,   null, null, 0d, 0d, true),
        cond(null, null, 0d, 0d,   0d, 0d, 0d, 0d),
        cond(null, null, 0d, 0d,   0d, null, 0d, null)
      ), "img/tests/extreme-conditions/ec4.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0d, null, 0d, null,   0d, 0d, null, null, true),
        cond(null, null, 0d, 0d,   0d, null, 0d, 0d),
        cond(0d, 0d, null, 0d,   0d, null, 0d, null)
      ), "img/tests/extreme-conditions/ec5.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(null, null, 0d, 0d,   0d, 0d, null, null, true),
        cond(null, 0d, 0d, 0d,   0d, null, 0d, 0d),
        cond(0d, 0d, null, 0d,   null, 0d, 0d, null)
      ), "img/tests/extreme-conditions/ec6.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(null, null, 0d, 0d,   0.005, null, 0d, null, true),
        cond(null, 0d, 0d, 0d,   0d, 0.005, 0d, 0d),
        cond(0d, null, null, 0d,   0d, 0d, 0d, null)
      ), "img/tests/extreme-conditions/ec7.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(-0.007, null, 0d, null,   0d, null, 0d, null, true),
        cond(null, null, 0d, 0d,   -0.007, null, 0d, 0d),
        cond(0d, null, 0d, 0d,   null, 0d, 0d, null)
      ), "img/tests/extreme-conditions/ec8.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(-0.003, null, 0d, null,   null, null, 0d, 0d, true),
        cond(0.003, null, 0d, null,   null, null, 0d, 0d),
        cond(0d, null, 0d, 0d,   null, 0d, 0d, null)
      ), "img/tests/extreme-conditions/ec9.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0d, -0.004, null, null,   null, null, 0d, 0d, true),
        cond(null, 0.004, null, null,   null, null, 0d, 0d),
        cond(-0.004, null, 0d, 0d,   0d, 0d, 0d, null)
      ), "img/tests/extreme-conditions/ec10.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(0.005, null, 0d, null,   0d, 0d, null, null, true),
        cond(-0.005, null, 0d, null,   0d, 0d, null, null),
        cond(0d, null, -0.005, 0d,   0d, null, 0d, null)
      ), "img/tests/extreme-conditions/ec11.png"),
      test("Визначити граничні умови кільцевої пластини вказаної на рисунку, невідомі умови позначити знаком «?»", Seq(
        cond(null, null, 0d, 0d,   0d, -0.002, null, null, true),
        cond(-0.002, null, 0d, null,   0d, 0d, null, null),
        cond(0d, null, -0.002, 0d,   0d, null, 0d, null)
      ), "img/tests/extreme-conditions/ec12.png")
    ))
  )

  type D = java.lang.Double

  def cond(wa: D, phia: D, mra: D, qra: D, wb: D, phib: D, mrb: D, qrb: D, correct: Boolean = false): TestOptionConf = {
    def parse(value: D): String = (if(value == null) "\\;?" else value.toString) + "\\;"
    val template = s"""${"$"}
      |w(a) = ${parse(wa)} м \\\\
      |\\phi(a) = ${parse(phia)} рад \\\\
      |M_r(a) = ${parse(mra)} кНм/м \\\\
      |Q_r(a) = ${parse(qra)} кН/м \\\\
      |w(b) = ${parse(wb)} м \\\\
      |\\phi(b) = ${parse(phib)} рад \\\\
      |M_r(b) = ${parse(mrb)} кНм/м \\\\
      |Q_r(b) = ${parse(qrb)} кН/м \\\\
      |${"$"}
    """.stripMargin
    opt(template, correct)
  }

//  val tests =

  def test(question: String, options: Seq[TestOptionConf], imageUrl: String = null): TestConf = {
    TestConf(-1, -1, question, Option(imageUrl), options).normalised
  }

  def opt(value: String, correct: Boolean = false): TestOptionConf = TestOptionConf(-1, value, correct)
}

class TestSetDataGenerator(testSetExamService: TestSetExamService) {

  val testSetConfs: Seq[TestSetConf] = TestSetData.testSetConfs.map(testSetExamService.createTestSetConf)

  private val groupsWithTests = generateGroupsWithTests

  val groupConfs: Seq[TestGroupConf] = groupsWithTests.map(_._1)
  val testConfs: Seq[TestConf] = groupsWithTests.flatMap(_._2)

  val testSetConfGroups: Seq[TestSetConfTestGroup] = addGroupsToTestSet(testSetConfs.head, groupConfs)

  def generateGroupsWithTests: Seq[(TestGroupConf, Seq[TestConf])] = {
    TestSetData.testGroupConfs.map{ case(tgConf, tConfs) =>
      val tgc = testSetExamService.createTestGroupConf(tgConf)
      val testConfs = tConfs.map(tc =>
        testSetExamService.createTestConf(tc.copy(groupId = tgc.id))
      )
      (tgc, testConfs)
    }
  }

  def addGroupsToTestSet(testSetConf: TestSetConf, groups: Seq[TestGroupConf]): Seq[TestSetConfTestGroup] = {
    val balancedProportion = 100 / groups.size
    groups.map(gc =>
      testSetExamService.createTestSetConfTestGroup(TestSetConfTestGroup(-1, testSetConf.id, gc.id, balancedProportion))
    )
  }
}
