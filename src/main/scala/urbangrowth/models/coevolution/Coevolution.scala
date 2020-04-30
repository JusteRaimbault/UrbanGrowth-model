package urbangrowth.models.coevolution

import org.openmole.spatialdata.model.urbandynamics

/**
  * Wrapper class for spatialdata coevolution model
  * @param populationMatrix
  * @param distancesMatrices
  * @param feedbackDistancesMatrix
  * @param dates
  * @param growthRate
  * @param gravityWeight
  * @param gravityGamma
  * @param gravityDecay
  * @param feedbackWeight
  * @param feedbackGamma
  * @param feedbackDecay
  */
case class Coevolution(
                      model: urbandynamics.Coevolution
                      ) {

  def run: urbandynamics.MacroResult = model.run

}


object Coevolution {

  def apply(populationsFile: String,
            distancesFile : String,
            feedbackDistancesFile : String,
            datesFile: String,
            growthRate: Double,
            gravityWeight: Double,
            gravityGamma: Double,
            gravityDecay: Double,
            feedbackWeight: Double,
            feedbackGamma: Double,
            feedbackDecay: Double
           ): Coevolution =
    Coevolution(urbandynamics.Coevolution(populationsFile,distancesFile,feedbackDistancesFile,datesFile,growthRate,gravityWeight,gravityGamma,gravityDecay,feedbackWeight,feedbackGamma,feedbackDecay))
}


