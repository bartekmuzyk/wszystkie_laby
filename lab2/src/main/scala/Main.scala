import scala.annotation.tailrec
import scala.math.sqrt

def parzysta(n: Int): Boolean = n % 2 == 0

@main
def zadanie_01: Unit = {
  assert(parzysta(2))
  assert(!parzysta(3))
  assert(parzysta(4))
  assert(!parzysta(5))
}

@tailrec
def nwd(a: Int, b: Int): Int = {
  if (a == b) {
    a
  } else if (a > b) {
    nwd(a - b, b)
  } else {
    nwd(a, b - a)
  }
}

@main
def zadanie_02(a: Int, b: Int): Unit = {
  println(a)
  println(b)
  println(nwd(a, b))
}

def pierwsza(n: Int): Boolean = {
  assert(n >= 2)
  !(2 until sqrt(n).ceil.toInt).exists(d => n % d == 0)
}

@main
def zadanie_03(liczba: Int): Unit = {
  println(liczba)
  println(pierwsza(liczba))
}

def roundUp(x: Double): Int = x.ceil.toInt

def hipoteza(n: Int): Unit = {
  @tailrec
  def rep(acc: Int): Unit = {
    val m = n - acc
    val polowa = roundUp(n / 2)

    if acc > polowa then println(s"Nie ma takich dwóch liczb pierwszych, które po zsumowaniu dają $n")
    else if pierwsza(acc) && pierwsza(m) then println(s"$acc + $m = $n")
    else rep(acc + 1)
  }
  
  rep(2)
}

@main
def zadanie_04(n: Int): Unit = {
  if !parzysta(n) then println("n nie jest parzyste")
  else if n <= 2 then println("n nie jest wieksze od 2")
  else hipoteza(n)
}
