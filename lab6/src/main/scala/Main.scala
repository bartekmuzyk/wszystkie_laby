//noinspection DropTakeToSlice
def subseq[A](list: List[A], begIdx: Int, endIdx: Int): List[A] = {
  list.drop(begIdx).take(endIdx - begIdx + 1)
}

@main
def zadanie_01(): Unit = {
  val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  println(subseq(list, 2, 5))
}

def pairPosNeg(list: List[Double]): (List[Double], List[Double]) = {
  list.filter(_ != 0).partition(_ < 0)
}

@main
def zadanie_02(): Unit = {
  val list = List(1.0, 2.0, 3.0, 0.0, -1.0, -2.0, -3.0)
  println(pairPosNeg(list))
}

def deStutter[A](list: List[A]): List[A] = {
  list.foldLeft(List.empty[A])(
    (acc, el) => acc match
      case head :: rest if head == el => acc
      case head :: rest => el :: acc
      case Nil => List(el)
  ).reverse
}

@main
def zadanie_03(): Unit = {
  val list = List(1, 1, 2, 4, 4, 4, 1, 3)
  println(deStutter(list))  // => 1, 2, 4, 1, 3
}

def remElems[A](list: List[A], k: Int): List[A] = {
  list.zipWithIndex.filter((_, i) => i != k).map((e, _) => e)
}

@main
def zadanie_04(): Unit = {
  val list = List(1, 2, 3, 4, 5, 6, 7)
  println(remElems(list, 2))  // => List(1, 2, 4, 5, 6, 7)
}

def freqMax[A](list: List[A]): (Set[A], Int) = {
  val freq = list.foldLeft(Map.empty[A, Int])(
    (acc, el) => {
      acc + (el -> (acc.getOrElse(el, 0) + 1))
    }
  )
  val highest = freq.filter((_, v) => v == freq.values.max)

  (highest.keys.toSet, highest.values.head)
}

@main
def zadanie_05(): Unit = {
  val l = List(1, 1, 2, 4, 4, 3, 4, 1, 3)
  println(freqMax(l))  // => Set(1, 4), 3
}
