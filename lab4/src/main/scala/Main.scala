import scala.annotation.tailrec

def sumuj(l: List[Option[Double]]): Option[Double] = {
  @tailrec
  def rep(list: List[Option[Double]], acc: Option[Double]): Option[Double] = list match
    case None :: rest => rep(rest, acc)
    case Some(head) :: rest => rep(
      rest,
      Some(acc.getOrElse(0d) + (head max 0))
    )
    case Nil => acc

  rep(l, None)
}

@main
def zadanie_01(): Unit = {
  val lista = List(Some(4.0), Some(-3.0), None, Some(1.0), Some(0.0))

  println(sumuj(lista))
}

def maksimum(l1: List[Double], l2: List[Double]): List[Double] = {
  @tailrec
  def rep(a: List[Double], b: List[Double], acc: List[Double]): List[Double] = (a, b) match
    case (Nil, Nil) => acc.reverse
    case (Nil, b_value :: b_rest) => rep(Nil, b_rest, b_value :: acc)
    case (a_value :: a_rest, Nil) => rep(a_rest, Nil, a_value :: acc)
    case (a_value :: a_rest, b_value :: b_rest) => rep(a_rest, b_rest, (a_value max b_value) :: acc)

  rep(l1, l2, Nil)
}

@main
def zadanie_02(): Unit = {
  val lista1 = List(2.0, -1.6, 3.2, 5.4, -8.4)
  val lista2 = List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5)

  println(maksimum(lista1, lista2))
}

def usun[A](l: List[A], el: A): List[A] = {
  @tailrec
  def rep(source: List[A], result: List[A]): List[A] = source match
    case head :: rest => rep(rest, if head == el then result else head :: result)
    case Nil => result.reverse

  rep(l, Nil)
}

@main
def zadanie_03(): Unit = {
  val lista = List(2, 1, 4, 1, 3, 3, 1, 2)

  println(usun(lista, 1))
}

def divide[A](l: List[A]): (List[A], List[A]) = {
  @tailrec
  def rep(source: List[A], left: List[A], right: List[A], flag: Boolean): (List[A], List[A]) = source match
    case head :: rest => if flag then rep(rest, head :: left, right, false) else rep(rest, left, head :: right, true)
    case Nil => (left.reverse, right.reverse)

  rep(l, Nil, Nil, true)
}

@main
def zadanie_04(): Unit = {
  val lista = List(1, 3, 5, 6, 7)

  println(divide(lista))
}

// Zadanie 5

type Pred[A] = A => Boolean

def and[A](p: Pred[A], q: Pred[A]): Pred[A] = {
  a => p(a) && q(a)
}

def or[A](p: Pred[A], q: Pred[A]): Pred[A] = {
  a => p(a) || q(a)
}

def not[A](p: Pred[A]): Pred[A] = {
  a => !p(a)
}

def imp[A](p: Pred[A], q: Pred[A]): Pred[A] = {
  a => !(p(a) && !q(a))
}
