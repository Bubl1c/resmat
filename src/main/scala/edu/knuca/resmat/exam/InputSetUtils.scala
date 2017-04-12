package edu.knuca.resmat.exam

object InputSetUtils {

  def verify(submittedAnswer: InputSetAnswerDto, correctAnswer: Seq[InputSetInputAnswer]): VerifiedInputSetAnswer = {
    var isCorrectAnswer = true
    var mistakesAmount = 0
    val verifiedAnswers: Map[Int, Boolean] = correctAnswer.map{ correctAnswer =>
      submittedAnswer.inputAnswers.find(_.id == correctAnswer.id) match {
        case Some(submittedInputAnswer) =>
          val areEqual = submittedInputAnswer.value match {
            case Some(sia) => correctAnswer.value match {
              case Some(ca) => areAlmostEqual(ca, sia)
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

  def areAlmostEqual(ethalon: Double, d2: Double, precision: Double = 0.05): Boolean = {
    val diff = (ethalon - d2).abs
    if(ethalon == 0.0) {
      ethalon == d2
    } else {
      (diff / ethalon).abs <= precision
    }
  }

}
