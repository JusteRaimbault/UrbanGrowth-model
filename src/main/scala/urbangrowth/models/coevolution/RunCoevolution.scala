package urbangrowth.models.coevolution

import java.io.File

import urbangrowth.utils.io.FileUtils

import org.openmole.spatialdata.model.urbandynamics
import org.openmole.spatialdata.utils.io.CSV

object RunCoevolution {

  def run(args: Array[String]): Unit = {
    assert(args.length>=3,"Coevol model needs at least 1 parameters")
    val system = args(1)

    // FIXME default dist file?

    val res: urbandynamics.MacroResult = args.length match {
      case 3 =>  Coevolution(
        populationsFile = "data/processed/"+system+"_pops.csv",
        distancesFile = "",
        feedbackDistancesFile = "",
        datesFile = "data/processed/"+system+"_dates.csv",
        growthRate = args(2).toDouble,
        gravityWeight = 0.0,
        gravityGamma = 1.0,
        gravityDecay = 1.0,
        feedbackWeight = 0.0,
        feedbackGamma = 1.0,
        feedbackDecay = 1.0
      ).run
      case 6 =>  Coevolution(
        populationsFile = "data/processed/"+system+"_pops.csv",
        distancesFile = "",
        feedbackDistancesFile = "",
        datesFile = "data/processed/"+system+"_dates.csv",
        args(2).toDouble,args(3).toDouble, args(4).toDouble,args(5).toDouble, 0.0, 1.0, 1.0).run
    }

    CSV.writeCSV(res.simulatedPopulation.values,"res/popsim.csv",",")
    CSV.writeCSV(res.simulatedPopulation.values, "res/poptarget.csv", sep = ",")

  }

}