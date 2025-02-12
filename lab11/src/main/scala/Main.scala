import org.apache.pekko
import pekko.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

case object Piłeczka

case class Graj01(przeciwnik: ActorRef)

case class Graj02(przeciwnik: ActorRef, maks: Int)

case class Graj03(przeciwnik1: ActorRef, przeciwnik2: ActorRef, zacznij: Boolean)

case class Graj04(kolejny: ActorRef, zacznij: Boolean)

class Gracz01 extends Actor with ActorLogging {
  def grajacy(iloscOdbic: Int): Receive = {
    case Piłeczka if iloscOdbic <= 1 => {
      log.info(s"Dostałem: Piłeczka | iloscOdbic = $iloscOdbic")
      log.info("context.system.terminate()")
      context.system.terminate()
    }
    case Piłeczka => {
      log.info(s"Dostałem: Piłeczka | iloscOdbic = $iloscOdbic")
      context.become(grajacy(iloscOdbic - 1))
      sender() ! Piłeczka
    }
  }

  def receive: Receive = {
    case Graj02(przeciwnik, maks) => {
      log.info(s"Dostałem: Graj02 | maks = $maks")
      context.become(grajacy(maks))
      przeciwnik ! Piłeczka
    }
  }
}

class Gracz02 extends Actor with ActorLogging {
  def receive: Receive = {
    case Piłeczka => {
      log.info("Piłeczka")
      sender() ! Piłeczka
    }
  }
}

class Gracz03 extends Actor with ActorLogging {
  def grajacy(graczPoLewej: ActorRef, graczPoPrawej: ActorRef): Receive = {
    case Piłeczka => {
      log.info(s"Gram! Podaję do ${graczPoLewej.path}")
      graczPoLewej ! Piłeczka
    }
    case Graj03(_, _, _) => ()
  }

  def receive: Receive = {
    case Graj03(przeciwnikPoLewej, przeciwnikPoPrawej, zacznij) => {
      log.info(s"Graj03 | ${przeciwnikPoLewej.path} <-> ${przeciwnikPoPrawej.path} | $zacznij")

      context.become(grajacy(przeciwnikPoLewej, przeciwnikPoPrawej))
      przeciwnikPoLewej ! Graj03(przeciwnikPoPrawej, self, false)
      przeciwnikPoPrawej ! Graj03(self, przeciwnikPoLewej, false)

      if (zacznij) {
        log.info(s"Podaję do: ${przeciwnikPoLewej.path}")
        przeciwnikPoLewej ! Piłeczka
      }
    }
  }
}

class Gracz04 extends Actor with ActorLogging {
  def gotowyDoGry(kolejny: ActorRef): Receive = {
    case Piłeczka => {
      log.info(s"Podaję do: ${kolejny.path}")
      kolejny ! Piłeczka
    }
  }

  def receive: Receive = {
    case Graj04(kolejny, zacznij) => {
      context.become(gotowyDoGry(kolejny))

      if (zacznij) {
        log.info(s"Zaczynam! Podaję do: ${kolejny.path}")
        kolejny ! Piłeczka
      }
    }
  }
}

@main
def mainProg(): Unit = {
  val system = ActorSystem("stol")
  val gracz1 = system.actorOf(Props[Gracz04](), "gracz1")
  val gracz2 = system.actorOf(Props[Gracz04](), "gracz2")
  val gracz3 = system.actorOf(Props[Gracz04](), "gracz3")
  val gracz4 = system.actorOf(Props[Gracz04](), "gracz4")
  val gracz5 = system.actorOf(Props[Gracz04](), "gracz5")

  gracz1 ! Graj04(gracz2, false)
  gracz2 ! Graj04(gracz3, false)
  gracz3 ! Graj04(gracz4, false)
  gracz4 ! Graj04(gracz5, false)
  gracz5 ! Graj04(gracz1, true)
}
