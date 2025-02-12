import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

case class Init(liczbaPracownikow: Int)

case class Zlecenie(tekst: List[String])

case class Wykonaj(napis: String)

case class Wynik(wartosc: Int)

class Szef extends Actor with ActorLogging {
  def czekaNaWyniki(pracownicy: List[ActorRef], ileWynikowOczekuje: Int, wyn: Int): Receive = {
    case Wynik(wartosc) => {
      val nowyWyn = wyn + wartosc
      log.info(s"nowyWyn = $nowyWyn, ileWynikowOczekuje = $ileWynikowOczekuje")

      if (ileWynikowOczekuje == 1) {
        log.info(nowyWyn.toString)
        context.become(gotowy(pracownicy))
      } else {
        context.become(czekaNaWyniki(pracownicy, ileWynikowOczekuje - 1, nowyWyn))
      }
    }
  }

  def gotowy(pracownicy: List[ActorRef]): Receive = {
    case Zlecenie(tekst) => {
      val iloscNapisow = tekst.length
      log.info(s"Zlecenie: $iloscNapisow napisów.")
      context.become(czekaNaWyniki(pracownicy, iloscNapisow, 0))
      val kolejkaPracownikow = Iterator.continually(pracownicy).flatten.take(iloscNapisow)
      (kolejkaPracownikow zip tekst).foreach(para => {
        para._1 ! Wykonaj(para._2)
      })
    }
  }

  def receive: Receive = {
    case Init(liczbaPracownikow) => {
      log.info(s"liczbaPracownikow = $liczbaPracownikow")
      val pracownicy = (1 to liczbaPracownikow)
        .map(num => context.system.actorOf(Props[Pracownik](), s"pracownik$num"))
        .toList
      context.become(gotowy(pracownicy))
    }
  }
}

class Pracownik extends Actor with ActorLogging {
  def receive: Receive = {
    case Wykonaj(napis) => {
      log.info(napis)
      sender() ! Wynik(napis.split(' ').length)
    }
  }
}

@main 
def mainProg(): Unit = {
  val system = ActorSystem("licznik")
  val szef = system.actorOf(Props[Szef](), "szef")

  szef ! Init(3)
  szef ! Zlecenie(List("Ala ma kota", "Turbosprężarka", "Rewolucja przemysłowa i jej wpływ na społeczeństwo", "Lubię placki", "Oto przykładowe zdanie"))
}
