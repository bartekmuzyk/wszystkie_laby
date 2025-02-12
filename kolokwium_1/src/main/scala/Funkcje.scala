//======================================================================================
// UWAGA! Zawartości tego pliku NIE WOLNO modyfikować
//======================================================================================

// Funkcja wczytuje dane z pliku „words.txt” i zwraca listę linii (słów)
def dane: List[String] = {
  io.Source.fromResource("words.txt").getLines.toList
}

// Funkcja zwraca częstości występowania poszczególnych liter w słowach z listy „dane”
def częstość: Map[Char, Double] = {
  Map(
    'a' -> 0.078, 'b' -> 0.02, 'c' -> 0.04, 'd' -> 0.038, 'e' -> 0.11, 'f' -> 0.014,
    'g' -> 0.03, 'h' -> 0.023, 'i' -> 0.086, 'j' -> 0.0021, 'k' -> 0.0097, 'l' -> 0.053,
    'm' -> 0.027, 'n' -> 0.072, 'o' -> 0.061, 'p' -> 0.028, 'q' -> 0.0019, 'r' -> 0.073,
    's' -> 0.087, 't' -> 0.067, 'u' -> 0.033, 'v' -> 0.01, 'w' -> 0.0091, 'x' -> 0.0027,
    'y' -> 0.016, 'z' -> 0.0044
  )
}
