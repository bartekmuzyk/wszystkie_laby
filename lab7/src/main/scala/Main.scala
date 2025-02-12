def sumOpts(l: List[Option[Double]]): Option[Double] = {
  l.foldLeft(None: Option[Double])(
    (acc, v) => (acc, v) match
      case (None, Some(value)) => Some(value)
      case (Some(acc), Some(value)) => Some(acc + value)
      case (Some(acc), None) => Some(acc)
      case (None, None) => None
  )
}

@main
def zadanie_01(): Unit = {
  val lista = List(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  assert(sumOpts(lista).contains(7.0)) // ==> true
  assert(sumOpts(List()).isEmpty) // ==> true
  assert(sumOpts(List(None, None)).isEmpty) // ==> true
}

def position[A](l: List[A], el: A): Option[Int] = {
  val result = l.indexOf(el)

  if result == -1 then None else Some(result)
}

@main
def zadanie_02(): Unit = {
  val lista = List(2, 1, 1, 5)
  println(position(lista, 1)) // ==> Some(1)
  println(position(lista, 3)) // ==> None
}

def indices[A](l: List[A], el: A): Set[Int] = {
  l.zipWithIndex.filter(_._1 == el).map(_._2).toSet
}

@main
def zadanie_03(): Unit = {
  val lista = List(1, 2, 1, 1, 5)
  println(indices(lista, 1)) // ==> Set(0, 2, 3).
  println(indices(lista, 7)) // ==> Set()
}

def swap[A](l: List[A]): List[A] = {
  l
    .grouped(2)
    .flatMap(_ match
      case List(a, b) => List(b, a)
      case a => a
    )
    .toList
}

@main
def zadanie_04(): Unit = {
  val lista = List(1, 2, 3, 4, 5)
  println(swap(lista))
}

@main
def zadanie_05(): Unit = {
  val strefy: List[String] = java.util.TimeZone.getAvailableIDs.toList
  println(
    strefy
      .filter(_.startsWith("Europe"))
      .groupBy(_.length)
      .toList
      .sortBy(_._1)
      .flatMap(_._2.sortWith((a, b) => a < b))
  )
}

def freq[A](l: List[A]): List[(A, Int)] = {
  l
    .groupBy(v => v)
    .toList
    .map(kv => (kv._1, kv._2.length))
}

@main
def zadanie_06(): Unit = {
  val lista = List('a', 'b', 'a', 'c', 'c', 'a')
  println(freq(lista))
}
