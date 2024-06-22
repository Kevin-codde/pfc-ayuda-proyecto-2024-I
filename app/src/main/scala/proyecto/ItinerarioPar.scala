package proyecto

import common._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

class ItinerariosPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]


  def buscarVuelos(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo], visitados: Set[String] = Set()): List[List[Vuelo]] = {
    if (origen == destino) {
      List(itinerarioActual)
    } else {
      vuelosDisponibles.par.flatMap { vuelo =>
        if (vuelo.Org == origen && !visitados.contains(vuelo.Dst)) {
          val nuevosVuelosDisponibles = vuelosDisponibles.filterNot(_ == vuelo)
          buscarVuelos(vuelo.Dst, destino, nuevosVuelosDisponibles, itinerarioActual :+ vuelo, visitados + vuelo.Org)
        } else {
          Nil
        }
      }.toList
    }
  }

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      buscarVuelos(cod1, cod2, vuelos, List())
    }
    encontrarItinerarios
  }

  
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def tiempoTotal(itinerario: List[Vuelo]): Int = {
      val (tiempoEnAire, tiempoEnTierra) = (
        itinerario.par.map { vuelo =>
          val origen = aeropuertos.find(_.Cod == vuelo.Org).get
          val destino = aeropuertos.find(_.Cod == vuelo.Dst).get
          val diferenciaHoraria = (destino.GMT - origen.GMT) / 100

          val salida = (vuelo.HS * 60) + vuelo.MS
          val llegada = (vuelo.HL * 60) + vuelo.ML + (diferenciaHoraria * 60).toInt
          if (llegada >= salida) llegada - salida else (llegada + (24 * 60)) - salida
        }.sum,

        (for {
          vuelo_indice <- (1 until itinerario.length).par // Comenzamos desde el segundo vuelo y lo convertimos a paralelo
        } yield {
          val vueloActual = itinerario(vuelo_indice)
          val vueloAnterior = itinerario(vuelo_indice - 1)
          
          // Tiempos de llegada y salida en minutos
          val llegadaAnterior = vueloAnterior.HL * 60 + vueloAnterior.ML
          val salidaActual = vueloActual.HS * 60 + vueloActual.MS
          
          // Calcular el tiempo en tierra
          if (llegadaAnterior > salidaActual) {
            salidaActual + (24 * 60) - llegadaAnterior // Si la llegada es después de la medianoche
          } else {
            salidaActual - llegadaAnterior // Si la llegada es antes de la medianoche
          }
        }).sum
      )

  tiempoEnAire + tiempoEnTierra
}

    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List()).par // Convertir a ParVector para paralelizar

      val itinerariosConTiempo = itinerariosPosibles.map { itinerario =>
        (itinerario, tiempoTotal(itinerario))
      }

      val itinerariosOrdenados = itinerariosConTiempo.seq.sortBy(_._2).take(3).map(_._1)
      itinerariosOrdenados.toList
    }

    encontrarItinerarios
  }


  // Función principal para encontrar itinerarios basados en el número de escalas
  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    // Función para calcular el número total de escalas en un itinerario
    def escalas(itinerario: List[Vuelo]): Int = {
      itinerario.map(_.Esc).sum + (itinerario.length - 1)
    }

    // Función para encontrar todos los itinerarios posibles entre dos aeropuertos de manera paralela
    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List()).par // Convertir a ParVector para paralelizar
      
      val itinerariosOrdenados = itinerariosPosibles.map { itinerario =>
        (itinerario, escalas(itinerario))
      }.toList.sortBy(_._2).take(3).map(_._1)

      itinerariosOrdenados.toList
    }

    encontrarItinerarios
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
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
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List()).par // Convertir a ParVector para paralelizar

      val itinerariosOrdenados = itinerariosPosibles.map { itinerario =>
        (itinerario, tiempoTotalVuelo(itinerario))
      }.toList.sortBy(_._2).take(3).map(_._1)

      itinerariosOrdenados.toList
    }

    encontrarItinerarios
  }

  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    def atiempo(itinerario: List[Vuelo], hora: Int, min: Int): Boolean = {
      val llegada = (itinerario.last.HL * 60) + itinerario.last.ML
      val cita = (hora * 60) + min
      llegada <= cita
    }

    def el_mas_tarde(itinerario: List[Vuelo]): Int = {
      (itinerario.head.HS * 60) + itinerario.head.MS
    }

    def encontrarItinerarios(cod1: String, cod2: String, hora: Int, min: Int): List[Vuelo] = {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List()).par // Convertir a ParVector para paralelizar

      val itinerariosFiltrados = itinerariosPosibles.filter(atiempo(_, hora, min)).toList
      val itinerariosOrdenados = itinerariosFiltrados.sortBy(el_mas_tarde).take(1)
      
      if (itinerariosOrdenados.nonEmpty) itinerariosOrdenados.head else List()
    }

    encontrarItinerarios
  }

}
