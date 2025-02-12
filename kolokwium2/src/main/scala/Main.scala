import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

case object Init

case class Zlecenie(liczba: Int)

case class Oblicz(liczba: Int)

case class Wynik(liczba: Int)

class Serwer extends Actor with ActorLogging {
  def gotowy(pracownicy: List[ActorRef], iloscPracownikowOgolnie: Int, kolejkaZlecen: List[Int]): Receive = {
    case Zlecenie(liczba) => {
//      log.info(s"Zlecenie($liczba) | iloscPracownikowOgolnie: $iloscPracownikowOgolnie/5 | kolejkaZlecen: $kolejkaZlecen")

      if (iloscPracownikowOgolnie < 5) {
        val nowyPracownik = context.system.actorOf(Props[Pracownik](), s"pracownik${iloscPracownikowOgolnie + 1}")

        context.become(gotowy(pracownicy, iloscPracownikowOgolnie + 1, kolejkaZlecen))

        nowyPracownik ! Oblicz(liczba)
        log.info(s"Oblicz($liczba) -> ${nowyPracownik.path}")
      } else {
        val pierwszy = pracownicy.headOption

        pierwszy match {
          case Some(pracownik) => {
            pracownik ! Oblicz(liczba)
            log.info(s"Oblicz($liczba) -> ${pracownik.path}")

            context.become(gotowy(pracownicy.tail, iloscPracownikowOgolnie, kolejkaZlecen))
          }
          case None => {
//            log.info(s"Dodaje $liczba do kolejki")
            context.become(gotowy(pracownicy, iloscPracownikowOgolnie, liczba :: kolejkaZlecen))
          }
        }
      }
    }
    case Wynik(liczba) => {
      log.info(s"Wynik($liczba) <- ${sender().path}")

      if (kolejkaZlecen.nonEmpty) {
        val nastepnaLiczba = kolejkaZlecen.head

        sender() ! Oblicz(nastepnaLiczba)
        log.info(s"Oblicz($nastepnaLiczba) -> ${sender().path}")

        context.become(gotowy(pracownicy, iloscPracownikowOgolnie, kolejkaZlecen.tail))
      } else {
        context.become(gotowy(sender() :: pracownicy, iloscPracownikowOgolnie, kolejkaZlecen))
      }
    }
  }

  def receive: Receive = {
    case Init => {
      context.become(gotowy(Nil, 0, Nil))
    }
  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Oblicz(liczba) => sender() ! Wynik(liczba * liczba)
  }
}

@main 
def mainProg(): Unit = {
  val system = ActorSystem("serwis")
  val serwer = system.actorOf(Props[Serwer](), "serwer")

  // testy
  serwer ! Init

  serwer ! Zlecenie(1)
  serwer ! Zlecenie(2)
  serwer ! Zlecenie(3)
  serwer ! Zlecenie(4)
  serwer ! Zlecenie(5)
  serwer ! Zlecenie(6)
  serwer ! Zlecenie(7)
}
