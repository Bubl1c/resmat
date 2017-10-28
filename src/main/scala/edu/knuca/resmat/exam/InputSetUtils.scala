package edu.knuca.resmat.exam

import edu.knuca.resmat.exam.taskflow.{InputSetAnswerDto, InputSetInputAnswer, VerifiedInputSetAnswer}

object InputSetUtils {

  def verify(submittedAnswer: InputSetAnswerDto, correctAnswer: Seq[InputSetInputAnswer], precision: Option[Double] = None): VerifiedInputSetAnswer = {
    var isCorrectAnswer = true
    var mistakesAmount = 0
    val verifiedAnswers: Map[Int, Boolean] = correctAnswer.map{ correctAnswer =>
      submittedAnswer.inputAnswers.find(_.id == correctAnswer.id) match {
        case Some(submittedInputAnswer) =>
          val areEqual = submittedInputAnswer.value match {
            case Some(sia) => correctAnswer.value match {
              case Some(ca) => areAlmostEqual(ca, sia, precision)
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

  def areAlmostEqual(ethalon: Double, toVerify: Double, precision: Option[Double] = None): Boolean = {
    val diff = (ethalon - toVerify).abs
    if(ethalon == 0.0 || ethalon.abs < 0.00000001) {
      toVerify == 0.0
    } else {
      (diff / ethalon).abs <= precision.getOrElse(0.01)
    }
  }

}
