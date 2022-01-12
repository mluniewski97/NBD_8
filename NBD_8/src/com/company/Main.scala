import scala.annotation.tailrec
object Main extends App {

  def Zad1(dzień: String) =
    dzień match {
      case "Poniedziałek" => "Praca"
      case "Wtorek" => "Praca"
      case "Środa" => "Praca"
      case "Czwartek" => "Praca"
      case "Piatek" => "Praca"
      case "Sobota" => "Weekend"
      case "Niedziela" => "Weekend"
      case _ => "Nie ma takiego dnia"
    }

  class KontoBankowe() {
    private var _stanKonta = 0

    def stanKonta = _stanKonta

    def this(stanPoczatkowy : Int) = {
      this()
      this._stanKonta = stanPoczatkowy
    }

    def wplata(wartosc : Int) = {
      this._stanKonta += wartosc
    }
    def wyplata(wartosc : Int) = {
      this._stanKonta -= wartosc
    }
  }

  def noweOsoby(osoba: Osoba) = osoba match {
    case Osoba("Mateusz", "Łuniewski") => "Cześć, Miło was widzieć"
    case Osoba("Brajan", "Tutak") => "Hej, Fajnie was widzieć"
    case Osoba("Michał", "Łuniewski") => "Siemka, To co robimy?"
    case _ => "Cześć, mnie jeszcze nie znacie"
  }

  def zwroc(num : Int, func : Int => Int) = {
    func(func(func(num)))
  }


    println("P1")
    println("Poniedziałek - " + Zad1("Poniedziałek"))
    println("Wtorek - " + Zad1("Wtorek"))
    println("Sobota - " + Zad1("Sobota"))
    println("Styczeń - " + Zad1("Styczeń"))


    println("P2")
    val konto = new KontoBankowe()
    println("Balans konta -  " + konto.stanKonta)
    println("Wpłata 50zł na konto")
    konto.wplata(50)
    println("Balans konta -  " + konto.stanKonta)
    println("Wypłata 50zł z konta")
    konto.wyplata(35)
    println("Balans konta -  " + konto.stanKonta)


    val mateusz = new Osoba("Mateusz", "Łuniewski")
    val marta = new Osoba("Marta", "Wądałowska")
    val brajan = new Osoba("Brajan", "Tutak")

    println("P3")
    println("Mateusz " + noweOsoby(mateusz))
    println("Marta " + noweOsoby(marta))
    println("Brajan " + noweOsoby(brajan))


    println("P4")
    println("Wartosc " + zwroc(5, (num : Int) =>  num * 2 ))


    println("P5")
    val osoba = new Osoba2("Andrzej","Kowalski")

    val student = new Osoba2("Konrad","Paluch") with Student

    val pracownik = new Osoba2("Maurycy","Kolwaczyk") with Pracownik
    pracownik.pensja = 200

    val nauczyciel = new Osoba2("Michał","Nowak") with Nauczyciel
    nauczyciel.pensja = 200

    val student_pracownik = new Osoba2("Konrad","Nowakowski") with Student with Pracownik
    student_pracownik.pensja = 200

    val pracownik_student = new Osoba2("Marcin","Andrzejewski") with Pracownik with Student
    pracownik_student.pensja = 200


    println("Stan początkowy konta: 200")
    println("osoba.podatek = " + osoba.podatek)
    println("student.podatek = " + student.podatek)
    println("pracownik.podatek = " + pracownik.podatek)
    println("nauczyciel.podatek = " + nauczyciel.podatek)
    println("student_pracownik.podatek = " + student_pracownik.podatek)
    println("pracownik_student.podatek = " + pracownik_student.podatek)

}


case class Osoba(imie: String, nazwisko: String)

case class Osoba2(private var _imie: String, private var _nazwisko: String) {
  def ime = _imie
  def nazwisko = _nazwisko
  def podatek = _podatek
  private var _podatek = 0.0

  def this(imie: String) {
    this(imie, "")
  }

  def this(imie: String, nazwisko: String, podatek: Double){
    this(imie, nazwisko)
    this._podatek = podatek
  }
}


trait Student extends Osoba2{
  override def podatek = {
    0.0
  }
}


trait Nauczyciel extends Pracownik {
  override def podatek = {
    0.1 * pensja
  }
}


trait Pracownik extends Osoba2 {
  var pensja = 0

  override def podatek = {
    0.2 * pensja
  }
}
