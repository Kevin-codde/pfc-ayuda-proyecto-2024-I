package proyecto

/*
	KEVIN ANDRES BEJARANO - 2067678

	JOHAN SEBASTIAN ACOSTA -2380393

	JUAN DAVID GUTIERREZ- 2060104
*/



class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  //case class Vuelo(Org: String, Dst: String)

  def buscarVuelos(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo], visitados: Set[String] = Set()): List[List[Vuelo]] = {
    if (origen == destino) {
      List(itinerarioActual)
    } else {
      vuelosDisponibles.filter(vuelo => vuelo.Org == origen && !visitados.contains(vuelo.Dst)).flatMap { vuelo =>
        val nuevosVuelosDisponibles = vuelosDisponibles.filterNot(_ == vuelo)
        buscarVuelos(vuelo.Dst, destino, nuevosVuelosDisponibles, itinerarioActual :+ vuelo, visitados + vuelo.Org)
      }
    }
  }


  //visitados algoritmo distra
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      buscarVuelos(cod1, cod2, vuelos, List())
    }
    encontrarItinerarios
  }

   //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje
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
            if ((itinerario(vuelo_indice-1).HL * 60 + itinerario(vuelo_indice-1).ML) > salida ){
              val tiempo_tierra =  ((itinerario(vuelo_indice-1).HL * 60 + itinerario(vuelo_indice-1).ML) + (24*60)) - salida 
              if ((llegada + tiempo_tierra.abs) >= salida) (llegada - salida) + tiempo_tierra.abs  else ((llegada  + (24 * 60)) - salida) + tiempo_tierra.abs
            } else{
              val tiempo_tierra = salida - (itinerario(vuelo_indice-1).HL * 60 + itinerario(vuelo_indice-1).ML) 
              if ((llegada + tiempo_tierra.abs) >= salida) (llegada - salida)  + tiempo_tierra.abs  else ((llegada  + (24 * 60)) - salida) + tiempo_tierra.abs
            }
          }else {
            if (llegada >= salida) llegada  - salida  else (llegada  + (24 * 60)) - salida
          }
        }
      ).sum
    }
    //16 35 - 19 48 + (19 40 + 24 08) - 10 55
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(tiempoTotal).take(3)
    }

    encontrarItinerarios
  }

   //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas
   //3
   // restar 1
  

   def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def escalas(itinerario: List[Vuelo]): Int = {
      itinerario.map { vuelo =>
        vuelo.Esc
      }.sum + itinerario.length
    } 
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(escalas).take(3)
    }
    encontrarItinerarios
   }

   //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
  //4
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
    def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo]  = {
      def atiempo(itinerario: List[Vuelo],hora:Int, min:Int):Boolean = {
        val llegada = (itinerario.last.HL * 60) + itinerario.last.ML
        val cita = (hora * 60) + min
        if (llegada > cita) false else true
      }

      def el_mas_tarde(itinerario: List[Vuelo]):Int = {
        (itinerario.head.HS * 60) + itinerario.head.MS
      }

      def encontrarItinerarios(cod1: String, cod2: String, hora:Int, min:Int):List[Vuelo]  = {
        val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
        itinerariosPosibles.filter(atiempo(_,hora,min)).sortBy(el_mas_tarde).take(1).head
      }
      encontrarItinerarios
    }

}
