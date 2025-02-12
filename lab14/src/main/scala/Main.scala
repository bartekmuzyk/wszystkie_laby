import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

case class Init(liczbaZleceń: Int)

case object Zlecenie

case object Wykonaj

class Szef extends Actor with ActorLogging {
  def gotowy(liczbaZleceń: Int, pracownikA: ActorRef, pracownikB: ActorRef): Receive = {
    case Zlecenie => {
      log.info("Zlecenie")
      pracownikA ! Wykonaj
      pracownikB ! Wykonaj
    }
  }

  def receive: Receive = {
    case Init(liczbaZleceń) => {
      val pracownikA = context.system.actorOf(Props[PracownikA](), "pracownikA")
      val pracownikB = context.system.actorOf(Props[PracownikB](), "pracownikB")
      val monter = context.system.actorOf(Props[Monter](), "monter")
      val magazyn = context.system.actorOf(Props[Magazyn](), "magazyn")

      monter ! InitMonter(magazyn)
      magazyn ! InitMagazyn(liczbaZleceń)
      pracownikA ! InitA(monter)
      pracownikB ! InitB(monter)

      context.become(gotowy(liczbaZleceń, pracownikA, pracownikB))
    }
  }
}

case class InitA(monter: ActorRef)

case class InitB(monter: ActorRef)

case class InitMonter(magazyn: ActorRef)

case class InitMagazyn(maxLiczbaProduktow: Int)

case object ProduktA

case object ProduktB

case object Produkt

class PracownikA extends Actor with ActorLogging {
  def gotowy(monter: ActorRef): Receive = {
    case Wykonaj => {
      log.info("Wykonuję ProduktA")
      monter ! ProduktA
    }
  }

  def receive: Receive = {
    case InitA(monter) => {
      context.become(gotowy(monter))
    }
  }
}

class PracownikB extends Actor with ActorLogging {
  def gotowy(monter: ActorRef): Receive = {
    case Wykonaj => {
      log.info("Wykonuję ProduktB")
      monter ! ProduktB
    }
  }

  def receive: Receive = {
    case InitB(monter) => {
      context.become(gotowy(monter))
    }
  }
}

class Magazyn extends Actor with ActorLogging {
  def gotowy(maxLiczbaProduktow: Int, liczbaProduktow: Int): Receive = {
    case Produkt => {
      val nowaLiczbaProduktow = liczbaProduktow + 1

      if (nowaLiczbaProduktow >= maxLiczbaProduktow) {
        log.info(s"$nowaLiczbaProduktow == $maxLiczbaProduktow KOŃCZĘ")
        context.system.terminate()
      } else {
        log.info(s"$nowaLiczbaProduktow/$maxLiczbaProduktow")
        context.become(gotowy(maxLiczbaProduktow, nowaLiczbaProduktow))
      }
    }
  }

  def receive: Receive = {
    case InitMagazyn(maxLiczbaProduktow) => {
      log.info(s"InitMagazyn($maxLiczbaProduktow)")
      context.become(gotowy(maxLiczbaProduktow, 0))
    }
  }
}

class Monter extends Actor with ActorLogging {
  def gotowy(magazyn: ActorRef, iloscA: Int, iloscB: Int): Receive = {
    case ProduktA => {
      val noweA = iloscA + 1
      log.info("Dostałem A")

      if (iloscB > 0) {
        magazyn ! Produkt
        context.become(gotowy(magazyn, iloscA, iloscB - 1))
        log.info(s"A: $iloscA, B: ${iloscB - 1}")
      } else {
        context.become(gotowy(magazyn, noweA, iloscB))
        log.info(s"A: $noweA, B: $iloscB")
      }
    }
    case ProduktB => {
      val noweB = iloscB + 1
      log.info("Dostałem B")

      if (iloscA > 0) {
        magazyn ! Produkt
        context.become(gotowy(magazyn, iloscA - 1, iloscB))
        log.info(s"A: ${iloscA - 1}, B: $iloscB")
      } else {
        context.become(gotowy(magazyn, iloscA, noweB))
        log.info(s"A: $iloscA, B: $noweB")
      }
    }
  }

  def receive: Receive = {
    case InitMonter(magazyn) => {
      context.become(gotowy(magazyn, 0, 0))
    }
  }
}

@main 
def mainProg(): Unit = {
  val system = ActorSystem("manufaktura")
  val szef = system.actorOf(Props[Szef](), "szef")
  szef ! Init(5)
  szef ! Zlecenie
  szef ! Zlecenie
  szef ! Zlecenie
  szef ! Zlecenie
  szef ! Zlecenie
  szef ! Zlecenie
  szef ! Zlecenie
}
