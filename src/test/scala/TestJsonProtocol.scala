import edu.knuca.resmat.exam.{ExamStepConfDataSet, ExamStepResultsDataSet, ExamStepTaskFlowDataSet, ExamStepTestSetDataSet}
import org.scalatest.{FlatSpec, FunSpec}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

class TestJsonProtocol extends FunSpec {

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
