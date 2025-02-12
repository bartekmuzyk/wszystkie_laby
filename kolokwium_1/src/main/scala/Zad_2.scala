import scala.annotation.tailrec
//======================================================================================
// Definicje aliasów „Lokata” i „Filtr” oraz funkcji „dane” znajdują się w plikach
// „Aliasy.scala” oraz „Funkcje.scala”. Zawartości tych plików nie wolno modyfikować.
//======================================================================================

def spelniaFiltr(wartoscFiltra: Int, numerFiltra: Int, slowo: String, wzorzec: String): Boolean = {
  require(wartoscFiltra < 3)
  val indeks = numerFiltra - 1
  wartoscFiltra match {
    case 0 => slowo.contains(wzorzec(indeks))
    case 1 => slowo(indeks) == wzorzec(indeks) || !slowo.contains(wzorzec(indeks))
    case 2 => !(slowo(indeks) == wzorzec(indeks))
  }
}

// Jedyna rzecz do zaimplementowania, czyli funkcja „oczyść”:
def oczyść(lista: List[Lokata], wzorzec: String, filtr: Filtr): List[Lokata] = {
  require(wzorzec.length == 5)
  require(filtr.toList.forall(el => Set(0, 1, 2).contains(el)))
  // Poniżej, zamiast Nil oczywiście należy umieścić rozwiązanie zadania
  val (f1, f2, f3, f4, f5) = filtr

  lista
    .filter(
      lokata => !(
        spelniaFiltr(f1, 1, lokata._2, wzorzec) ||
        spelniaFiltr(f2, 2, lokata._2, wzorzec) ||
        spelniaFiltr(f3, 3, lokata._2, wzorzec) ||
        spelniaFiltr(f4, 4, lokata._2, wzorzec) ||
        spelniaFiltr(f5, 5, lokata._2, wzorzec)
      )
    )
}

//======================================================================================
//  UWAGA! Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
//
//    testOnly Test2
//
//======================================================================================

@main def zad_2: Unit = {
  // „program główny” ma znaczenie czysto pomocnicze
  // do przetestowania działania funkcji można wykorzystać dane poniżej
  val lista = dane.map(s => (0, s, 0.0))
  // dane z zadania Zad_1 pozwalają na ciekawsze zastosowania
  //val lista = sortuj
  println(oczyść(lista, "great", (2, 2, 2, 2, 2)))
}
