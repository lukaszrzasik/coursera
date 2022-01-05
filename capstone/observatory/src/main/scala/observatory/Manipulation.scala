package observatory

import scala.math._

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  val GridLatNorth = 90
  val GridLatSouth = -89
  val GridLonWest = -180
  val GridLonEast = 179

  val GridMinX = 0
  val GridMinY = 0
  val GridMaxX = abs(GridLonEast) + abs(GridLonWest)
  val GridMaxY = abs(GridLatNorth) + abs(GridLatSouth)
  val GridArraySize = (GridMaxX + 1) * (GridMaxY + 1)

  var excCounterGrid = 6480001 //6480000 //6480001 //6489999 //6490000 //6480000 //6470000 //6460000 //6450000 //6499999 //6500000 //6450000 //6449999 //6449000 //6448000 //6445000 //6435000 //6425000 //6450000 //6500000 //6400000 //6300000 //6200000 //6000000 //6500000 //7000000 //6000000 //5961961 //5900000 //5810671 //5803471 //5800000 //5500000 //6000000 //7000000 //5000000 //10000000 //1000000 //100000 //10000 //1000 //100 //1
  var excStringGrid = "lrzasik:\n"

  var excCounterAvg = 100 //10 //2
  var excStringAvg = "lrzasik:\n"

  var excCounterAG = 1000000 //10000000 //100000000
  var excStringAG = "lrzasik:\n"

  def getIndexFromLoc(gloc: GridLocation): Int = {
    var x = gloc.lon - GridLonWest 
    x = if (x < GridMinX) GridMinX else if (x > GridMaxX) GridMaxX else x
    var y = GridLatNorth - gloc.lat
    y = if (y < GridMinY) GridMinY else if (y > GridMaxY) GridMaxY else y
 
    y * (GridMaxX + 1) + x
  }
  
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    object Grid {
      val GridArray = Array.fill[Option[Temperature]](GridArraySize)(None)

      def apply(gloc: GridLocation): Temperature = {
//        if (excCounterGrid < 100)
//          excStringGrid += "Grid " + excCounterGrid + ", " + gloc.toString + ", " + temperatures.toString + "\n"
//        excCounterGrid -= 1
//        if (excCounterGrid < 1) throw new Exception(excStringGrid)

        if (excCounterGrid % 100000 == 0) System.gc()

        val index = getIndexFromLoc(gloc)
        GridArray(index) match {
          case Some(temp) => temp
          case None => {
            val loc = Location(gloc.lat, gloc.lon)
            val newTemp = Visualization.predictTemperature(temperatures, loc)
            GridArray(index) = Some(newTemp)
            newTemp
          }
        }
      }
    }

//    System.gc()
    Grid.apply
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
//    val locTemp = getAverages(temperaturess)
//    makeGrid(locTemp)

    object AverageGrid {

      val AverageGridArray = Array.fill[Option[Temperature]](GridArraySize)(None)

      val YearlyGrids = 
        temperaturess //.par
          .map{ temps => makeGrid(temps) }

      def apply(gloc: GridLocation): Temperature = {
//        if (excCounterAG < 100)
//          excStringAG += "AverageGrid " + excCounterAG + ", " + gloc.toString + ", " + temperaturess.toString + "\n"
//        excCounterAG -= 1
//        if (excCounterAG < 1) throw new Exception(excStringAG)

//        if (excCounterAG % 100000 == 0) System.gc()

        val index = getIndexFromLoc(gloc)
        AverageGridArray(index) match {
          case Some(temp) => temp
          case None => {
            val tempCounter = YearlyGrids
              .aggregate((0D, 0))(
                { case ((zeroTemp, zeroCounter), grid) => (zeroTemp + grid(gloc), zeroCounter + 1) },
                { case ((temp1, counter1), (temp2, counter2)) => (temp1 + temp2, counter1 + counter2) })
            val newTemp = tempCounter._1 / tempCounter._2
            AverageGridArray(index) = Some(newTemp)
            newTemp
          }
        }
      }
    }

//    System.gc()
//    if (excCounterAvg < 100)
//      excStringAvg += "average " + excCounterAvg + ", " + temperaturess.toString + "\n"
//    excCounterAvg -= 1
//    if (excCounterAvg < 1) throw new Exception(excStringAvg)
//    excCounterGrid = 6480001
    AverageGrid.apply
//    _ => 0D
  }

  def getAverages(temperaturess: Iterable[Iterable[(Location, Temperature)]]): Iterable[(Location, Temperature)] = {
    temperaturess //.par
      .flatMap(it => it)
      .groupBy(_._1)
      .mapValues{ 
        _.aggregate((0D, 0))(
        { case ((zeroTemp, zeroCounter), (loc, temp)) => (zeroTemp + temp, zeroCounter + 1) },
        { case ((temp1, counter1), (temp2, counter2)) => (temp1 + temp2, counter1 + counter2) })}
      .mapValues{ case (temp, counter) => temp / counter }
      .toIterable
      .toList
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {

    object DeviationGrid {

      val DeviationGridArray = Array.fill[Option[Temperature]](GridArraySize)(None)

      val grid = makeGrid(temperatures)

      def apply(gloc: GridLocation): Temperature = {
        val index = getIndexFromLoc(gloc)
        DeviationGridArray(index) match {
          case Some(temp) => temp
          case None => {
            val devTemp = grid(gloc) - normals(gloc)
            DeviationGridArray(index) = Some(devTemp)
            devTemp
          }
        }
      }
    }

    DeviationGrid.apply
  }


}

