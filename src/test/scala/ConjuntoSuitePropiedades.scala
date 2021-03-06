
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators, all}
import org.scalacheck.Gen._

object ConjuntoSuitePropiedades extends Properties("Test sobre conjunto") {
  val valor = choose(0, 10)

  /**
    * Generacion de secuencia de tamaño
    *
    * @param tam
    * @return
    */
  def secuencia(tam: Int): Range = {
    val inicio = valor.sample.getOrElse(0)
    inicio to (inicio + tam)
  }

  /**
    * Propiedad para probar el metodo de obtencion de la longitud
    */
  property("Conjunto de tamaño uno") =
    forAll(valor) {
      valor => {
        // Se crea el conjunto de un elemento
        val conjunto = Conjunto.conjuntoUnElemento(valor)

        // Se comprueba que el conjunto contiene el valor
        conjunto(valor) == true
      }
    }

  property("Conjunto unión") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)

        // Se generan los conjuntos a unir
        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

        // Se produce la union
        val union = Conjunto.union(conjunto1, conjunto2)

        // Se itera sobre la union de ambos rangos y se comprueba la
        // pertenencia al conjunto
        val rangoUnion = secuencia1.toList ::: secuencia2.toList

        // De cumplirse que cada elemento esta en el conjunto union
        val resultado = rangoUnion.map(valor => {
          union(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("Conjunto intersección") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)

        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

        val interseccion = Conjunto.interseccion(conjunto1, conjunto2)

        val rangoInterseccion = secuencia1.toList.intersect(secuencia2.toList)

        val resultado = rangoInterseccion.map(valor => {
          interseccion(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("Conjunto diferencia") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)

        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

        val diferencia = Conjunto.diferencia(conjunto1, conjunto2)

        val rangoDiferencia = secuencia1.toList.filterNot(secuencia2.toList.contains(_))

        val resultado = rangoDiferencia.map(valor => {
          diferencia(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("Conjunto filter") =
    forAll(valor) {
      valor => {
        val secuencia1 = secuencia(10)
        val secuencia2 = secuencia(10)

        val conjunto1 = Conjunto(x => x >= secuencia1.min && x <= secuencia1.max)
        val conjunto2 = Conjunto(x => x >= secuencia2.min && x <= secuencia2.max)

        val filter = Conjunto.filtrar(conjunto1, conjunto2.funcionCaracteristica)

        val rangoFilter = secuencia1.toList.intersect(secuencia2.toList)

        val resultado = rangoFilter.map(valor => {
          filter(valor) == true
        })

        val global: Boolean = resultado.forall(res => res == true)
        global == true
      }
    }

  property("Conjunto para todo") =
    forAll(valor) {
      valor => {

        val conjunto = Conjunto(x => x <= 10)

        val condicion1 = Conjunto.paraTodo(conjunto,x=> x <= 10)
        val condicion2 = !Conjunto.paraTodo(conjunto,x => x >= 5)

        all(condicion1,condicion2)
      }
    }

  property("Conjunto existe") =
    forAll(valor) {
      valor => {

        val conjunto = Conjunto(x => x <= 10)

        val condicion1 = Conjunto.paraTodo(conjunto,x=> x <= 10)
        val condicion2 = !Conjunto.paraTodo(conjunto,x => x >= 2)

        all(condicion1,condicion2)
      }
    }

  property("Conjunto map") =
    forAll(valor) {
      valor => {

        val conjunto = Conjunto(x => x <= 10)
        val conjuntoMap = Conjunto.map(conjunto, x => x+25)

        val condicion1 = conjuntoMap(30)
        val condicion2 = conjuntoMap(31)
        val condicion3 = conjuntoMap(150)

        all(condicion1 && condicion2 && !condicion3)
      }
    }
}