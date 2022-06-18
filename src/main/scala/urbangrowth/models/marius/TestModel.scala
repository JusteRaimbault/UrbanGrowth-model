
package urbangrowth.models.marius

import java.io.File

object TestModel {

  def testModel: SimpleModel = SimpleModel(
    new File("data/processed/BR_dist.csv"),
    new File( "data/processed/BR_pops.csv"),
    new File("data/processed/BR_dates.csv"),
    economicMultiplier= 1.0, //5.9059803,
    sizeEffectOnSupply= 0.1, //3.60315698509,
    sizeEffectOnDemand= 0.3,//3.30481631293628,
    distanceDecay=1.698876633327205,
    wealthToPopulationExponent=1.18720491738353,
    populationToWealthExponent=1.1140844912
  )

  /*
  def testModel : BonusFixedCostModel = BonusFixedCostModel(
  distanceMatrixFile = "data/marius/RU_dist.csv"
  ,

  bonusMultiplier = 197.948890779081
  ,
  fixedCost = 0.256524806806571
  ,
  distanceDecay = 0.672263161474518
  ,
  sizeEffectOnSupply = 1.00175638801509
  ,
  sizeEffectOnDemand = 1.07926078029368
  ,
  economicMultiplier = 0.34380934416368
  ,
  populationToWealthExponent = 1.08660127543912
  ,
  wealthToPopulationExponent = 0.380435604357353
    */


  //val path = new File("/tmp/mariusmodel_log.csv")
  //path.delete
  //val out = Resource.fromFile(path)
  //out.append("step, arokato, population, wealth \n")
  /*val populations: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]
  for {
    (s, i) <- TestModel.testModel
      .states.zipWithIndex
    ss <- s
  } {
    val cities = ss.cities
    populations.append(cities.map{_.population}.toArray)
    /*for {
      (city, arokato) <- (cities zip MariusFile.arokatos)
    } {
      def line = Seq(i, arokato, city.population, city.wealth)
      out.append(line.mkString("", ",", "\n"))
    }*/
    //val totalWealth = cities.map(_.wealth).sum
    //val totalPop = cities.map(_.population).sum
    //println(s"State $i, total wealth $totalWealth, total population $totalPop")
  }
  val popMatrix = new Matrix(populations.toArray)
  println(Indicators.logmse(popMatrix.transpose(),TestModel.testModel.modelConfiguration.startingCities
*/



}