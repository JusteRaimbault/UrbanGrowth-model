
package urbangrowth.models.marius

import java.io.File

import org.openmole.spatialdata.model.urbandynamics.MacroResult
import org.openmole.spatialdata.utils.io.CSV
import urbangrowth.indicators._
import urbangrowth.utils.io.FileUtils

object RunMarius {

  def run(args: Array[String]): Unit = {
    assert(args.length>=6,"Marius model needs at least 4 parameters")
    val system = args(1)

    val res: MacroResult = args.length match {
      case 6 =>  SimpleModel(new File("data/processed/"+system+"_dist.csv"),new File("data/processed/"+system+"_pops.csv"),new File("data/processed/"+system+"_dates.csv"),
        args(2).toDouble,args(3).toDouble, args(4).toDouble,args(5).toDouble, 1.0,1.0).run()
      case 8 =>  SimpleModel(new File("data/processed/"+system+"_dist.csv"),new File("data/processed/"+system+"_pops.csv"),new File("data/processed/"+system+"_dates.csv"),
        args(2).toDouble,args(3).toDouble, args(4).toDouble,args(5).toDouble, args(6).toDouble,args(7).toDouble).run()
    }

    CSV.writeCSV(res.simulatedPopulation.values,"res/popsim.csv",",")
    CSV.writeCSV(res.targetPopulation.values,"res/poptarget.csv",",")

  }

}