import scala.io.Source

@main
def zadanie_01(): Unit = {
  val src = Source.fromFile("nazwiska.txt")
  val lines = src.getLines.toList

  val najwiecejRoznychLiter = lines
    .map(v => (v, v.toLowerCase.replace(" ", "").distinct.length))
    .maxBy(_._2)._1
  println(najwiecejRoznychLiter)

  val najkrotszeNazwisko = lines
    .map(_.split(" "))
    .minBy(_.last.length)
    .mkString(" ")
  println(najkrotszeNazwisko)

  src.close
}

def histogram(max: Int): String = {
  val src = Source.fromFile("ogniem_i_mieczem.txt")
  val chars = src.toList

  val freq = chars
    .filter(_.isLetter)
    .groupBy(v => v.toLower)
    .toList
    .map(v => (v._1, v._2.length))
    .sortBy(_._2)
    .reverse
  val scale = (freq.head._2 / max.toDouble) max 1
  val stars = freq
    .map(v => (v._1, "*" * (v._2 / scale).ceil.toInt))

  stars.map(v => s"${v._1}:${v._2}").mkString("\n")
}

@main
def zadanie_02(): Unit = {
  println(histogram(50))
}