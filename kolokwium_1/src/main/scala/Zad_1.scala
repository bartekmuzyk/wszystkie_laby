//======================================================================================
// Definicje aliasu „Lokata” oraz funkcji „dane” i „częstość” znajdują się w plikach
// „Aliasy.scala” oraz „Funkcje.scala”. Zawartości tych plików nie wolno modyfikować.
//======================================================================================

import scala.annotation.tailrec

@tailrec
def myLength[A](list: List[A], acc: Int = 0): Int = list match {
  case _ :: tail => myLength(tail, acc + 1)
  case Nil => acc
}

def myDistinct[A](list: List[A]): List[A] = list.toSet.toList

def punktacja(slowo: String): Double = {
  @tailrec
  def helper(l: List[Char], acc: Double): Double = l match {
    case head :: tail => helper(tail, acc + częstość(head))
    case Nil => acc
  }

  val sumaCzestosci = helper(slowo.toList, 0)
  val iloscRoznychLiter = myLength(myDistinct(slowo.toList))

  sumaCzestosci + iloscRoznychLiter
}

def myHead[A](list: List[A]): Option[A] = list match {
  case head :: tail => Some(head)
  case Nil => None
}

def myTail[A](list: List[A]): List[A] = list match {
  case head :: tail => tail
  case Nil => Nil
}

def myMinByDouble[A](l: List[A])(func: A => Double): Option[A] = {
  @tailrec
  def helper(list: List[A], acc: A): A = list match {
    case head :: tail if func(head) < func(acc) => helper(tail, head)
    case head :: tail => helper(tail, acc)
    case Nil => acc
  }

  val head = myHead(l)

  head match {
    case Some(value) => Some(helper(myTail(l), value))
    case None => None
  }
}

def myAddToListMaybe[A](list: List[A], elem: Option[A]): List[A] = elem match {
  case Some(value) => value :: list
  case None => list
}

@tailrec
def myFilter[A](list: List[A], check: A => Boolean, acc: List[A] = Nil): List[A] = list match {
  case head :: tail if check(head) => myFilter(tail, check, head :: acc)
  case head :: tail => myFilter(tail, check, acc)
  case Nil => myReverse(acc)
}

@tailrec
def myReverse[A](list: List[A], acc: List[A] = Nil): List[A] = list match {
  case head :: tail => myReverse(tail, head :: acc)
  case Nil => acc
}

def mySortByDouble[A](list: List[A])(func: A => Double): List[A] = {
  @tailrec
  def helper(l: List[A], acc: List[A]): List[A] = l match {
    case Nil => acc
    case _ => myMinByDouble(l)(func) match {
      case Some(value) => helper(
        myFilter(l, elem => elem != value),
        value :: acc
      )
      case None => acc
    }
  }

  helper(list, Nil)
}

// Jedyna rzecz do zaimplementowania, czyli funkcja „ranking”:
def ranking: List[Lokata] = {
  @tailrec
  def helper1(l: List[String], acc: List[Lokata]): List[Lokata] = l match {
    case head :: tail => helper1(
      tail,
      (0, head, punktacja(head)) :: acc
    )
    case Nil => acc
  }

  val czesc1 = helper1(dane, Nil)
  val czesc2 = mySortByDouble(czesc1)(lokata => lokata._3)

//  println(czesc2)

  @tailrec
  def helper2(l: List[Lokata], accWyn: List[Lokata], accPoz: Int, accPrawPoz: Int): List[Lokata] = (l, accWyn) match {
    case (headL :: tailL, Nil) => helper2(
      tailL,
      (accPoz, headL._2, headL._3) :: accWyn,
      accPoz,
      accPrawPoz + 1
    )
    case (headL :: tailL, headAccWyn :: tailAccWyn) if headAccWyn._3 == headL._3 => helper2(
      tailL,
      (accPoz, headL._2, headL._3) :: accWyn,
      accPoz,
      accPrawPoz + 1
    )
    case (headL :: tailL, headAccWyn :: tailAccWyn) => helper2(
      tailL,
      (accPrawPoz, headL._2, headL._3) :: accWyn,
      accPrawPoz,
      accPrawPoz + 1
    )
    case (Nil, _) => myReverse(accWyn)
  }

  // Poniżej, zamiast Nil oczywiście należy umieścić rozwiązanie zadania
  helper2(czesc2, Nil, 1, 1)
}

//======================================================================================
//  UWAGA! Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
//
//    testOnly Test1
//
//======================================================================================

@main
def zad_1: Unit = {
  println(ranking)
}
