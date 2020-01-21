import edu.knuca.resmat.data.TaskFlowData
import edu.knuca.resmat.data.Utils._
import edu.knuca.resmat.exam._
import org.scalatest.{FlatSpec, FunSpec}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

class TestJsonProtocol extends FunSpec {

  import edu.knuca.resmat.core.ringplate.RingPlateProblemAnswer.{Mapping => M}

  describe("eqset") {
    val json = InputSetEquationSystem("InputSetEquationSystem", List(
      InputSetEquation(1, List[EquationItem](
        ei(11, M.g1_00, "X1"), es("+"), ei(12, M.g1_01, "X2"), es("+"), ei(13, M.g1_02, "X3"), es("+"), ei(14, M.g1_03, "X4"), es("="), ed(M.g1_04)
      )),
      InputSetEquation(2, List[EquationItem](
        ei(21, M.g1_10, "X1"), es("+"), ei(22, M.g1_11, "X2"), es("+"), ei(23, M.g1_12, "X3"), es("+"), ei(24, M.g1_13, "X4"), es("="), ed(M.g1_14)
      )),
      InputSetEquation(3, List[EquationItem](
        ei(31, M.g1_20, "X1"), es("+"), ei(32, M.g1_21, "X2"), es("+"), ei(33, M.g1_22, "X3"), es("+"), ei(34, M.g1_23, "X4"), es("="), ed(M.g1_24)
      )),
      InputSetEquation(4, List[EquationItem](
        ei(41, M.g1_30, "X1"), es("+"), ei(42, M.g1_31, "X2"), es("+"), ei(43, M.g1_32, "X3"), es("+"), ei(44, M.g1_33, "X4"), es("="), ed(M.g1_34)
      ))
    )).asJson.toString()

    println(json)
  }

  describe("sdrsg") {
    import edu.knuca.resmat.http.JsonProtocol._
    val json =
      """
        |{
        |  "examConf": {
        |    "id": 1,
        |    "name": "Назва залікової роботи",
        |    "description": "Тет має бути детальний опис роботи та інструкції для студентів",
        |    "maxScore": 100
        |  },
        |  "stepConfs": [
        |    {
        |      "examStepConf": {
        |        "id": 1,
        |        "examConfId": 1,
        |        "sequence": 1,
        |        "name": "Тестування",
        |        "stepType": "test-set",
        |        "mistakesPerAttemptLimit": 5,
        |        "mistakeValuePercents": 1,
        |        "attemptsLimit": 3,
        |        "attemptValuePercents": 5,
        |        "maxScore": 20,
        |        "dataSet": {
        |          "ExamStepTestSetDataSet": {
        |            "testSetConfId": 1
        |          }
        |        },
        |        "hasToBeSubmitted": true
        |      },
        |      "stepDataConf": {
        |        "TestSetConfDto" : {
        |          "testSetConf": {
        |          "id": 1,
        |          "name": "Набір тестів для крутих студентів",
        |          "maxTestsAmount": 9
        |        },
        |        "testGroups": [
        |          {
        |            "id": 1,
        |            "testSetConfId": 1,
        |            "testGroupConfId": 1,
        |            "proportionPercents": 20
        |          },
        |          {
        |            "id": 2,
        |            "testSetConfId": 1,
        |            "testGroupConfId": 2,
        |            "proportionPercents": 20
        |          },
        |          {
        |            "id": 3,
        |            "testSetConfId": 1,
        |            "testGroupConfId": 3,
        |            "proportionPercents": 20
        |          },
        |          {
        |            "id": 4,
        |            "testSetConfId": 1,
        |            "testGroupConfId": 4,
        |            "proportionPercents": 20
        |          },
        |          {
        |            "id": 5,
        |            "testSetConfId": 1,
        |            "testGroupConfId": 5,
        |            "proportionPercents": 20
        |          }
        |        ]
        |        }
        |      }
        |    },
        |    {
        |      "examStepConf": {
        |        "id": 2,
        |        "examConfId": 1,
        |        "sequence": 2,
        |        "name": "Розв'язання задачі",
        |        "stepType": "task-flow",
        |        "mistakesPerAttemptLimit": -1,
        |        "mistakeValuePercents": 1,
        |        "attemptsLimit": -1,
        |        "attemptValuePercents": 0,
        |        "maxScore": 80,
        |        "dataSet": {
        |          "ExamStepTaskFlowDataSet": {
        |            "taskFlowConfId": 1,
        |            "problemConfId": 1
        |          }
        |        },
        |        "hasToBeSubmitted": true
        |      }
        |    },
        |    {
        |      "examStepConf": {
        |        "id": 3,
        |        "examConfId": 1,
        |        "sequence": 3,
        |        "name": "Результати",
        |        "stepType": "results",
        |        "mistakesPerAttemptLimit": -1,
        |        "mistakeValuePercents": 0,
        |        "attemptsLimit": -1,
        |        "attemptValuePercents": 0,
        |        "maxScore": 0,
        |        "dataSet": {
        |          "ExamStepResultsDataSet": {}
        |        },
        |        "hasToBeSubmitted": false
        |      }
        |    }
        |  ]
        |}
      """.stripMargin
    val res = decode[ExamConfUpdateDto](json).fold(e => {
      None
    }, res => {
      res
    })
  }

  describe("interfaces") {
    val dataSet1: ExamStepConfDataSet = ExamStepTestSetDataSet(1)
    val dataSet2: ExamStepConfDataSet = ExamStepTaskFlowDataSet(2, 3)
    val dataSet3: ExamStepConfDataSet = ExamStepResultsDataSet

    val jsons = Vector(dataSet1.asJson.toString(), dataSet2.asJson.toString(), dataSet3.asJson.toString(), "{}")

    println(jsons)

    println(jsons.map(json => {
      decode[ExamStepConfDataSet](json).fold(e => None, Some(_))
    }))
  }

}
