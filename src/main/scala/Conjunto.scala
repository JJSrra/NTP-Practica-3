
/**
  * Clase para representar conjuntos definidos mediante una funcion
  * caracteristica (un predicado). De esta forma, se declara el tipo
  * conjunto como un predicado que recibe un entero (elemento) como
  * argumento y dvuelve un valor booleano que indica si pertenece o no
  * al conjunto
  *
  * @param funcionCaracteristica
  */
class Conjunto(val funcionCaracteristica: Int => Boolean) {
/**
    * Crea una cadena con el contenido completo del conjunto
    *
    * @return
    */
  override def toString(): String = {
    // El uso de this(i) implica la el uso del metodo apply
    val elementos = for (i <- -Conjunto.LIMITE to Conjunto.LIMITE 
                         if this(i)) yield i
    elementos.mkString("{", ",", "}")
  }
  
  /**
    * Metodo para determinar la pertenencia de un elemento al
    * conjunto
    * @param elemento
    * @return valor booleano indicando si elemento cumple
    *         la funcion caracteristica o no
    */
  def apply(elemento: Int): Boolean = {
    return funcionCaracteristica(elemento)
  }
}



/**
  * Objeto companion que ofrece metodos para trabajar con
  * conjuntos
  */
object Conjunto {
   /**
    * Limite para la iteracion necesaria algunas operaciones,
    * entre -1000 y 1000
    */
  private final val LIMITE = 1000
  
  /**
    * Metodo que permite crear objetos de la clase Conjunto
    * de forma sencilla
    * @param f
    * @return
    */
  def apply(f: Int => Boolean): Conjunto = {
    new Conjunto(f)
  }

  def conjuntoUnElemento(elemento : Int) : Conjunto = {
    new Conjunto((x:Int) => x == elemento)
  }

  def union(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => c1.apply(x) || c2.apply(x))
  }

  def interseccion(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => c1.apply(x) && c2.apply(x))
  }

  def diferencia(c1 : Conjunto, c2 : Conjunto) : Conjunto = {
    new Conjunto((x:Int) => c1.apply(x) && !c2.apply(x))
  }

  def filtrar(c : Conjunto, predicado : Int => Boolean) : Conjunto = {
    new Conjunto((x:Int) => c.apply(x) && predicado(x))
  }

  def paraTodo(c : Conjunto, predicado: Int => Boolean) : Boolean = {
    def iterar (elemento:Int) : Boolean = {
      if(elemento == LIMITE+1) true
      else if (!c.apply(elemento)) iterar(elemento+1)
      else{
        predicado(elemento) && iterar(elemento+1)
      }

    }

    iterar(-LIMITE)
  }

  def existe(c : Conjunto, predicado : Int => Boolean) : Boolean = {
    !paraTodo(c, !predicado(_))
  }

  def map(c : Conjunto, funcion : Int => Int) : Conjunto = {
    new Conjunto((x:Int) => existe(c, valor => x == funcion(valor)))
  }
}