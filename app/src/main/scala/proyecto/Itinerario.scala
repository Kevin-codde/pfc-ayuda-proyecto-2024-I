package proyecto

/*
	KEVIN ANDRES BEJARANO - 2067678

	JOHAN SEBASTIAN ACOSTA -2380393

	JUAN DAVID GUTIERREZ- 2060104
*/

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

def buscarVuelos(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo], visitados: mutable.Set[String]): List[List[Vuelo]] = {
  if (origen == destino) List(itinerarioActual)
  else if (visitados.contains(origen)) Nil
  else {
    visitados += origen
    val vuelosPosibles = vuelosDisponibles.filter(_.Org == origen)
    val resultados = for {
      vuelo <- vuelosPosibles
      nuevosItinerarios <- buscarVuelos(vuelo.Dst, destino, vuelosDisponibles.filterNot(_ == vuelo), itinerarioActual :+ vuelo, visitados)
    } yield nuevosItinerarios
    visitados -= origen
    resultados
  }
}

def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
  def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
    buscarVuelos(cod1, cod2, vuelos, List(), mutable.Set())
  }
  encontrarItinerarios
}

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def tiempoTotal(itinerario: List[Vuelo]): Int = {
      (for {
        vuelo_indice <-  0 until itinerario.length
      } yield {
        val origen = aeropuertos.find(_.Cod == itinerario(vuelo_indice).Org).get
        val destino = aeropuertos.find(_.Cod == itinerario(vuelo_indice).Dst).get
        val diferenciaHoraria = (destino.GMT - origen.GMT) / 100

        val salida = itinerario(vuelo_indice).HS * 60 + itinerario(vuelo_indice).MS
        val llegada = itinerario(vuelo_indice).HL * 60 + itinerario(vuelo_indice).ML + (diferenciaHoraria * 60).toInt

        if (vuelo_indice != 0){
          val tiempo_tierra = salida - (itinerario(vuelo_indice-1).HL * 60 + itinerario(vuelo_indice-1).ML)
          if (llegada >= salida) (llegada + tiempo_tierra.abs) - salida  else ((llegada + tiempo_tierra.abs) + (24 * 60)) - salida
        }else {
          if (llegada >= salida) llegada  - salida  else (llegada  + (24 * 60)) - salida
        }
      }
        ).sum
    }

    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(tiempoTotal).take(3)
    }
    encontrarItinerarios
  }



  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def escalas(itinerario: List[Vuelo]): Int = {
      itinerario.map { vuelo =>
        1 + vuelo.Esc
      }.sum
    }
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(escalas).take(3)
    }
    encontrarItinerarios
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def tiempoTotalVuelo(itinerario: List[Vuelo]): Int = {
      itinerario.map { vuelo =>
        val origen = aeropuertos.find(_.Cod == vuelo.Org).get
        val destino = aeropuertos.find(_.Cod == vuelo.Dst).get
        val diferenciaHoraria = (destino.GMT - origen.GMT) / 100

        val salida = (vuelo.HS * 60) + vuelo.MS
        val llegada = (vuelo.HL * 60) + vuelo.ML + (diferenciaHoraria * 60).toInt
        if (llegada >= salida) llegada - salida else (llegada + (24 * 60)) - salida
      }.sum
    }
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(tiempoTotalVuelo).take(3)
    }
    encontrarItinerarios
  }

  //Recibe una lista de vuelos y aeropuertos
  //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
  //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
  //que permiten llegar a una hora de la cita
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]]  = {
    def atiempo(itinerario: List[Vuelo],hora:Int, min:Int):Boolean = {
      val llegada = (itinerario.last.HL * 60) + itinerario.last.ML
      val cita = (hora * 60) + min
      if (llegada > cita) false else true
    }

    def el_mas_tarde(itinerario: List[Vuelo]):Int = {
      (itinerario.head.HS * 60) + itinerario.head.MS
    }

    def encontrarItinerarios(cod1: String, cod2: String, hora:Int, min:Int):List[List[Vuelo]]  = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      println(itinerariosPosibles.filter(atiempo(_,hora,min)))
      itinerariosPosibles.filter(atiempo(_,hora,min)).sortBy(el_mas_tarde).take(1)
    }
    encontrarItinerarios
  }
}
