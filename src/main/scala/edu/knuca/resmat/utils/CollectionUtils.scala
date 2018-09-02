package edu.knuca.resmat.utils

case class DiffResult[AT, BT](added: Seq[BT], same: Seq[BT], removed: Seq[AT])

object CollectionUtils {

  def diff[AT, BT](a: Seq[AT], b: Seq[BT], areTheSame: (AT, BT) => Boolean): DiffResult[AT, BT] = {
    val r = a.foldLeft(DiffResult[AT, BT](Seq(), Seq(), Seq()))((diff, ai) => {
      val biOpt = b.find(areTheSame(ai, _))
      biOpt.fold(
        diff.copy(removed = diff.removed :+ ai)
      )(bi =>
        diff.copy(same = diff.same :+ bi)
      )
    })
    val r2 = b.foldLeft(r)((diff, bi) => {
      val aiOpt = a.find(areTheSame(_, bi))
      aiOpt.fold(
        diff.copy(added = diff.added :+ bi)
      )(_ => diff) // already added all the same on 1st iteration
    })
    r2
  }

}
