package edu.knuca.resmat.exam

import edu.knuca.resmat.exam.taskflow.{InputSetAnswerDto, InputSetInputAnswer, VerifiedInputSetAnswer}
import edu.knuca.resmat.utils.NumberUtils

object InputSetUtils {

  def verify(submittedAnswer: InputSetAnswerDto, correctAnswer: Seq[InputSetInputAnswer], precision: Option[Double] = None): VerifiedInputSetAnswer = {
    var isCorrectAnswer = true
    var mistakesAmount = 0
    val verifiedAnswers: Map[Int, Boolean] = correctAnswer.map{ correctAnswer =>
      submittedAnswer.inputAnswers.find(_.id == correctAnswer.id) match {
        case Some(submittedInputAnswer) =>
          val areEqual = submittedInputAnswer.value match {
            case Some(sia) => correctAnswer.value match {
              case Some(ca) => NumberUtils.areAlmostEqual(ca, sia, precision)
              case None => false
            }
            case None => correctAnswer.value match {
              case Some(ca) => false
              case None => true
            }
          }
          if(areEqual) {
            (correctAnswer.id, true)
          } else {
            isCorrectAnswer = false
            mistakesAmount = mistakesAmount + 1
            (correctAnswer.id, false)
          }
        case None =>
          isCorrectAnswer = false
          mistakesAmount = mistakesAmount + 1
          (correctAnswer.id, false)
      }
    }.toMap
    VerifiedInputSetAnswer(submittedAnswer.inputSetId, isCorrectAnswer, mistakesAmount, verifiedAnswers)
  }

}
