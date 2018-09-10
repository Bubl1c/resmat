package edu.knuca.resmat.exam.testset

import edu.knuca.resmat.exam.{TestOptionConf, TestOptionValueType}
import edu.knuca.resmat.utils.NumberUtils

import scala.util.Try

object TestUtils {

  def verifyTraditionalTest(testAnswer: TestSubmittedAnswerDto, correctOptionIds: Seq[Long]): VerifiedTestAnswerDto = {
    //For every correct option, submitted option exists
    var isCorrectAnswer = correctOptionIds.forall(testAnswer.submittedOptions.contains(_))
    var mistakesAmount = 0
    //For every submitted option, correct option exists
    val verifiedOptions = testAnswer.submittedOptions.map { soId: Long =>
      val correct = correctOptionIds.contains(soId)
      if(!correct) {
        isCorrectAnswer = false
        mistakesAmount = mistakesAmount + 1
      }
      (soId, correct)
    }

    VerifiedTestAnswerDto(testAnswer.testConfId, isCorrectAnswer, mistakesAmount, verifiedOptions.toMap)
  }

  def verifySingleInputTest(testConfId: Long,
                            submitted: String,
                            correctOption: TestOptionConf): VerifiedTestAnswerDto = {
    val isCorrectAnswer: Boolean = correctOption.valueType match {
      case TestOptionValueType.Number =>
        val submittedNumber = Try(submitted.toDouble).getOrElse{
          throw new IllegalArgumentException(s"Submitted data for test conf with id $testConfId has to be of type Double")
        }
        val correctNumber = Try(correctOption.value.toDouble).getOrElse{
          throw new IllegalStateException(s"Correct data for test conf with id $testConfId has to be of type Double")
        }
        NumberUtils.areAlmostEqual(correctNumber, submittedNumber, Some(0.0001))
      case TestOptionValueType.Text =>
        submitted == correctOption.value
      case _ =>
        throw new IllegalStateException(s"ValueType ${correctOption.valueType} is not supported for test conf with id $testConfId")
    }
    val mistakesAmount = if (isCorrectAnswer) 0 else 1

    VerifiedTestAnswerDto(testConfId, isCorrectAnswer, mistakesAmount, Map(correctOption.id -> isCorrectAnswer))
  }

}
