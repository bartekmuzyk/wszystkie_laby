import scala.annotation.tailrec

def oczyść[A](l: List[A]): List[A] = {
  @tailrec
  def rep(source: List[A], target: List[A]): List[A] = (source, target) match
    case (head :: rest, target_head :: _) if target_head == head => rep(rest, target)
    case (head :: rest, _) => rep(rest, head :: target)
    case (Nil, result) => result.reverse

  rep(l, Nil)
}


@main
def zadanie_01(): Unit = {
  val lista = List(1, 1, 2, 4, 4, 4, 1, 3)
  println(oczyść(lista)) // ==> List(1, 2, 4, 1, 3)
}


def skompresuj[A](l: List[A]): List[(A, Int)] = {
  @tailrec
  def rep(source: List[A], target: List[(A, Int)]): List[(A, Int)] = (source, target) match
    case (head :: rest, (last_element, last_count) :: result_rest) if head == last_element =>
      rep(rest, (last_element, last_count + 1) :: result_rest)
    case (head :: rest, result_rest) =>
      rep(rest, (head, 1) :: result_rest)
    case (Nil, result) => result.reverse

  rep(l, Nil)
}

@main
def zadanie_02(): Unit = {
  val lista = List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd')
  println(skompresuj(lista)) // ==> List(('a', 2), ('b', 1), ('c', 3), ('a', 2), ('b', 1), ('d', 1))
}

@tailrec
def isOrdered[A](leq: (A, A) => Boolean)(l: List[A]): Boolean = l match
  case Nil => true
  case _ :: Nil => true
  case x :: y :: rest => leq(x, y) && isOrdered(leq)(y :: rest)

@main
def zadanie_03(): Unit = {
  val lt = (m: Int, n: Int) => m < n
  val lte = (m: Int, n: Int) => m <= n
  val lista = List(1, 2, 2, 5)
  println(isOrdered(lt)(lista)) // ==> false
  println(isOrdered(lte)(lista)) // ==> true
}

def applyForAll[A, B](f: A => B)(l: List[A]): List[B] = {
  @tailrec
  def rep(source: List[A], target: List[B]): List[B] = source match
    case head :: rest => rep(rest, f(head) :: target)
    case Nil => target.reverse
  
  rep(l, Nil)
}

@main
def zadanie_04(): Unit = {
  val lista = List(1, 3, 5)
  val f = (n: Int) => n + 3
  println(applyForAll(f)(lista)) // ==> List(4, 6, 8)
}
