
package urbangrowth.models.innovation

import org.openmole.spatialdata.model.urbandynamics

import scala.util.Random


/**
  * Wrapper for urbandynamics Innovation model
  * @param model
  */
case class Innovation(
                     model: urbandynamics.Innovation
                     )



object Innovation {

  def apply(populationFile: String,
            distanceFile: String,
            datesFile: String,
            seed: Int,
            growthRate: Double,
            innovationWeight: Double,
            gravityDecay: Double,
            innovationDecay: Double,
            innovationUtility: Double = 1.0,
            innovationUtilityGrowth: Double = 1.12,
            earlyAdoptersRate: Double = 0.01,
            newInnovationHierarchy: Double = 1.0,
            newInnovationPopulationProportion: Double = 0.5
           )(implicit rng: Random): Innovation = Innovation(
    urbandynamics.Innovation(populationFile,distanceFile,datesFile,seed,growthRate,innovationWeight,gravityDecay,innovationDecay,innovationUtility,innovationUtilityGrowth,earlyAdoptersRate,newInnovationHierarchy,newInnovationPopulationProportion)
  )

}

