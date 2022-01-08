package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface {

  val temperaturesScale = Seq(
    (60D, Color(255,255,255)),
    (32D, Color(255,0,0)),
    (12D, Color(255,255,0)),
    (0D, Color(0,255,255)),
    (-15D, Color(0,0,255)),
    (-27D, Color(255,0,255)),
    (-50D, Color(33,0,107)),
    (-60D, Color(0,0,0))
  )

  val deviationScale = Seq(
    (7D, Color(0,0,0)),
    (4D, Color(255,0,0)),
    (2D, Color(255,255,0)),
    (0D, Color(255,255,255)),
    (-2D, Color(0,255,255)),
    (-7D, Color(0,0,255))
  )

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    Seq(
      Layer(LayerName.Temperatures, temperaturesScale, Range(1975, 1991)),
      Layer(LayerName.Deviations, deviationScale, Range(1991, 2016))
    )
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {
    Signal(
      if (sliderValue() < selectedLayer().bounds.start) selectedLayer().bounds.start
      else if (sliderValue() > selectedLayer().bounds.last) selectedLayer().bounds.last
      else sliderValue()
    )
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal("target/" + selectedLayer().layerName.id + "/" + selectedYear().toString + "/{z}/{x}-{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(selectedLayer().layerName.id.capitalize + " (" + selectedYear().toString + ")")
  }

}

// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface {
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

