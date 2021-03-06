
package urbangrowth.models.marius

import java.io.File

import org.openmole.spatialdata.model.urbandynamics.{MacroModel, MacroResult}
import org.openmole.spatialdata.utils.math.Matrix
import urbangrowth.indicators._

import scala.collection.mutable.ArrayBuffer


/** Simple model with only core mechanisms */
case class SimpleModel(
                      modelDistanceMatrix: DistanceMatrix,
                      modelConfiguration: MariusConfiguration,
                      economicMultiplier: Double,
                      sizeEffectOnSupply: Double,
                      sizeEffectOnDemand: Double,
                      distanceDecay: Double,
                      wealthToPopulationExponent: Double,
                      populationToWealthExponent: Double) extends MacroModel with Marius //with DefaultValues
 {

  override def distanceMatrix: DistanceMatrix = modelDistanceMatrix

  override def configuration: MariusConfiguration = modelConfiguration

   override def maxStep: Int = modelConfiguration.dates.length - 1

   override def toString: String = "Simple Marius Model with parameters\n\teconomicMultiplier = "+economicMultiplier+"\n\tsizeEffectOnSupply = "+sizeEffectOnSupply+
   "\n\tsizeEffectOnDemand = "+sizeEffectOnDemand+"\n\tdistanceDecay = "+distanceDecay+"\n\twealthToPopulationExponent = "+wealthToPopulationExponent+
   "\n\tpopulationToWealthExponent = "+populationToWealthExponent

  def run(): MacroResult = SimpleModel.run(this)

}


object SimpleModel {

  def apply(
             distanceMatrixFile: File,
             confFile: File,
             datesFile: File,
             economicMultiplier: Double,
             sizeEffectOnSupply: Double,
             sizeEffectOnDemand: Double,
             distanceDecay: Double,
             wealthToPopulationExponent: Double,
             populationToWealthExponent: Double
           ): SimpleModel =
    SimpleModel(MariusFile.distances(distanceMatrixFile),MariusFile.configFromFile(confFile,datesFile) ,economicMultiplier,sizeEffectOnSupply,
      sizeEffectOnDemand, distanceDecay, wealthToPopulationExponent, populationToWealthExponent)


  def run(model: SimpleModel): MacroResult = {
    println("Running "+model.toString)
    val populations: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]
    for {
      (s, i) <- model.states.zipWithIndex
      ss <- s
    }{
      val cities = ss.cities
      populations.append(cities.map {
        _.population
      }.toArray)
    }
    val popMatrix: Matrix = Matrix(populations.toArray)(Matrix.defaultImplementation).transpose
    val realPopMatrix =  Matrix(model.configuration.realPopulations.getArray)(Matrix.defaultImplementation)
    //println("Simulated : "+popMatrix.getRowDimension+" x "+popMatrix.getColumnDimension)
    //println("Target : "+model.configuration.realPopulations.getRowDimension+" x "+model.configuration.realPopulations.getColumnDimension)
    MacroResult(realPopMatrix,popMatrix)
  }


}