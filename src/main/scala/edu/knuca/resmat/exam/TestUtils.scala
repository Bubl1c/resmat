package edu.knuca.resmat.exam

object TestUtils {

  def verify(testAnswer: TestAnswerDto, correctOptionIds: Seq[Long]): VerifiedTestAnswerDto = {
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

    VerifiedTestAnswerDto(testAnswer.testId, isCorrectAnswer, mistakesAmount, verifiedOptions.toMap)
  }

}
