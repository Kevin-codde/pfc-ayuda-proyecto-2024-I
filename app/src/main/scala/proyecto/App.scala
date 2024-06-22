/**
  * Taller 3 - Programación Funcional
  * Autores: KEVIN ANDRES BEJARANO - 2067678	
             JOHAN SEBASTIAN ACOSTA -2380393
             JUAN DAVID GUTIERREZ- 2060104
  * Profesor: Carlos A Delgado
  */
package proyecto
import datos._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object App{

  def saludo() = "Proyecto final"

  def main(args: Array[String]): Unit = {
    println(saludo())
    //println(
    //  withWarmer(new Warmer.Default) measure {
    //    (1 to 100000000).toArray
    //  }
    //)
    val itoObj = new ItinerariosPar()
    val itinerariopar = itoObj.itinerariosPar(vuelosC1, aeropuertos)
    val tiempopar = itoObj.itinerariosTiempoPar(vuelosC1, aeropuertos)
    val escalaspar = itoObj.itinerariosEscalasPar(vuelosC1, aeropuertos)
    val airepar = itoObj.itinerariosAirePar(vuelosC1, aeropuertos)
    val salidasPar = itoObj.itinerariosSalidaPar(vuelosCurso , aeropuertosCurso)

    val itoObjse = new Itinerario()
    val itinerario = itoObjse.itinerarios(vuelosC1, aeropuertos)
    val tiempo = itoObjse.itinerariosTiempo(vuelosC1, aeropuertos)
    val escalas = itoObjse.itinerariosEscalas(vuelosC1, aeropuertos)
    val aire = itoObjse.itinerariosAire(vuelosC1, aeropuertos)
    val salidas = itoObjse.itinerariosSalida(vuelosCurso , aeropuertosCurso)


    println(compararfunciones(itinerario,itinerariopar)("PHX", "DTW"))
    println(compararfunciones(tiempo,tiempopar )("PHX", "DTW"))
    println(compararfunciones(escalas,escalaspar )("PHX", "DTW"))
    println(compararfunciones(aire,airepar )("PHX", "DTW"))
    println(compararfuncionessalidas(salidas, salidasPar )("CTG", "PTY", 11, 40))
  }
  def compararfunciones(funcion1: (String, String) => List[List[Vuelo]], funcion2: (String, String) => List[List[Vuelo]])
                    (cond1: String, cond2: String): (Double, Double, Double) = {

    val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
      funcion1(cond1, cond2)
    }

    val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
      funcion2(cond1, cond2)
    }

    val tiempo1: Double = tiempoFuncion1.value
    val tiempo2: Double = tiempoFuncion2.value

    val aceleracion = tiempo1 / tiempo2

    (tiempo1, tiempo2, aceleracion)
  }
  def compararfuncionessalidas(funcion1: (String, String, Int,Int) => List[Vuelo], funcion2: (String, String,Int,Int) => List[Vuelo])
                    (cond1: String, cond2: String, numero1: Int, numero2: Int): (Double, Double, Double) = {

    val tiempoFuncion1 = withWarmer(new Warmer.Default) measure {
      funcion1(cond1, cond2,numero1,numero2)
    }

    val tiempoFuncion2 = withWarmer(new Warmer.Default) measure {
      funcion2(cond1, cond2,numero1,numero2)
    }

    val tiempo1: Double = tiempoFuncion1.value
    val tiempo2: Double = tiempoFuncion2.value

    val aceleracion = tiempo1 / tiempo2

    (tiempo1, tiempo2, aceleracion)
  }
 }




