def obramuj(napis: String): String = {
  val linie = napis.split("\n")
  val najdluzsza = linie.maxBy(s => s.length())
  val szerokosc = najdluzsza.length()
  val ramka = "**" + ("*" * szerokosc) + "**"
  ramka + "\n" + linie.map(l => "* " + l + (" " * (szerokosc - l.length())) + " *").mkString("\n") + "\n" + ramka
}

@main
def mainProg(napis: String): Unit = {
  println(obramuj(napis))
}