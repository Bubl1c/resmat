val i = 10;

val j = i match {
  case 1 | 2 => 12
  case 10 | 100 => 100
  case _ => 200
}