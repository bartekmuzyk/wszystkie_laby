import scala.annotation.tailrec
import scala.math.sqrt

def reverse(str: String): String = {
  @tailrec
  def rep(src: String, acc: String): String = {
    if src.isEmpty then acc
    else rep(src.dropRight(1), acc.concat(src.takeRight(1)))
  }

  rep(str, "")
}


@main
def zadanie_01(tekst: String): Unit = {
  println(reverse(tekst))
}

def pierwsza(n: Int): Boolean = {
  @tailrec
  def rep(n: Int, n_sqrt: Int, current: Int): Boolean = {
    if current > n_sqrt then true
    else if n % current == 0 then false
    else rep(n, n_sqrt, current + 1)
  }

  if n == 2 then true else rep(n, sqrt(n).ceil.toInt, 2)
}

@main
def zadanie_02(n: Int): Unit = {
  println(pierwsza(n))
}

//noinspection NonAsciiCharacters
def ciąg(n: Int): Int = {
  val first = 2
  val second = 1

  @tailrec
  def rep(current: Int, left: Int, right: Int): Int = {
    if current == -1 then right
    else rep(current - 1, right, left + right)
  }

  if n == 0 then first
  else if n == 1 then second
  else rep(n - 2, first, second)
}

@main
def zadanie_03(n: Int): Unit = {
  //noinspection NonAsciiCharacters
  println(ciąg(n))
}

def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
  @tailrec
  def rep(acc: List[Int], l1_inner: List[Int], l2_inner: List[Int]): List[Int] = {
    if l1_inner.isEmpty && l2_inner.isEmpty then acc
    else {
      if !(l1_inner.isEmpty || l2_inner.head < l1_inner.head) && (l2_inner.isEmpty || l1_inner.head <= l2_inner.head) then
        if acc.isEmpty || l1_inner.head != acc.last then
          rep(acc.appended(l1_inner.head), l1_inner.tail, l2_inner)
        else
          rep(acc, l1_inner.tail, l2_inner)
      else
        if acc.isEmpty || l2_inner.head != acc.last then
          rep(acc.appended(l2_inner.head), l1_inner, l2_inner.tail)
        else
          rep(acc, l1_inner, l2_inner.tail)
    }
  }

  rep(List(), l1, l2)
}

@main
def zadanie_04(): Unit = {
  val lista1 = List(2, 4, 3, 5)
  val lista2 = List(1, 2, 2, 3, 1, 5)

  println(tasuj(lista1, lista2))
  println(tasuj(lista1, lista2) == List(1, 2, 3, 1, 4, 3, 5))
}
