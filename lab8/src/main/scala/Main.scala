def mastermind(guess: List[Int], secret: List[Int]): (Int, Int) = {
  val intersection = secret.intersect(guess)
  val black = guess.zip(secret).count((a, b) => a == b)
  val white = intersection.length - black

  (white, black)
}

@main
def zadanie_01(): Unit = {
  val secret = List(1, 3, 2, 2, 4, 5)
  val guess = List(2, 1, 2, 4, 7, 2)

  val (white, black) = mastermind(guess, secret)

  println(white)
  println(black)
}

def oblicz_wyniki(dane: List[(String, String, Int, Int)]): Map[Int, List[(String, String, Int, Int)]] = {
  val placesValues = dane
    .groupBy(data => (data._1, data._2))
    .map(
      (k, v) => k -> (
        k._1,
        k._2,
        v.map(_._3).sum / v.length,
        v.map(_._4).sum / v.length
      )
    )
    .values
    .groupBy(data => data._3 + data._4)
    .values
    .flatMap(_.toList.sortBy(data => data._3).reverse)
    .groupBy(data => (data._3, data._4))
    .toList
    .sortBy(_._1._1).reverse
    .map(_._2.toList)
  val placesNumbers = (1 to placesValues.length).toList
  (placesNumbers zip placesValues).toMap
}

@main
def zadanie_02(): Unit = {
  val dane = List(
    ("a", "b",  1, 19),
    ("a", "b", 19,  1),
    ("c", "d", 19,  1),
    ("e", "f",  1,  1),
    ("g", "h",  1,  1),
    ("i", "j", 10, 10)
  )

  val result = oblicz_wyniki(dane)

  println(result)
}

def threeNumbers(n: Int): List[(Int, Int, Int)] = {
  (
    for {
      a <- 1 to n
      b <- a + 1 to n
      c <- b + 1 to n
      if a*a + b*b == c*c
    } yield (a, b, c)
  ).toList
}

@main
def zadanie_03(): Unit = {
  println(threeNumbers(19))
}
