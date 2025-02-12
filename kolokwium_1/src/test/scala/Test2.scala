
import org.scalatest.funsuite.AnyFunSuite

class Test2 extends AnyFunSuite {
  val lista = dane.map(e=>(0,e,0.0))
  test("Result for guess 'goods' (1,1,2,0,0)") {
    assert(oczyść(lista,"goods",(1,1,2,0,0)).length == 9)
  }

  test("Result for guess 'smart' (0,0,0,0,0)") {
    assert(oczyść(lista,"smart",(0,0,0,0,0)).length == 691)
  }

  test("Result for guess 'great' (2,2,2,2,2)") {
    assert(oczyść(lista,"great",(2,2,2,2,2)).length == 1)
  }
}
