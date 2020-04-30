
package urbangrowth.models.marius

//import java.awt.geom._
//import org.geotools.referencing._
import math._
import scala.util.{Failure, Success, Try}

object Marius {

  /**
    * Compute the geodetic distance between two locations described by their coordinates in decimal degrees.
    *
    * @param p1 first coordinates
    * @param p2 second coordinates
    * @return distance between the two locations
    */
  /*def distance(p1: Position, p2: Position) = {
    lazy val calc = new GeodeticCalculator

    val v1 = new Point2D.Double(p1.longitude, p1.latitude)
    val v2 = new Point2D.Double(p2.longitude, p2.latitude)
    calc.setStartingGeographicPoint(v1)
    calc.setDestinationGeographicPoint(v2)
    calc.getOrthodromicDistance
  }*/

  /** Cache of the distance matrix between */
  /*lazy val distanceMatrix: DistanceMatrix = {
    val positions = MariusFile.positions.toVector

    positions.zipWithIndex.map {
      case (c1, i) =>
        positions.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }*/


  /**
    * load distances from file
    */
  //lazy val distanceMatrix: DistanceMatrix = MariusFile.distances

}

import Marius._

trait Marius {

  def distanceMatrix: DistanceMatrix

  def configuration: MariusConfiguration

  /** Number of simulation steps */
  def maxStep: Int

  /** Exponent of the scaling law to convert population into wealth */
  def populationToWealthExponent: Double

  /** Exponent of the scaling law between demand and population */
  def sizeEffectOnDemand: Double

  /** Exponent of the scaling law between supply and population */
  def sizeEffectOnSupply: Double

  /** Factor adjusting the values of production and consumption */
  def economicMultiplier: Double

  /** Decaying effect of distance on interaction */
  def distanceDecay: Double

  /** Exponent of the scaling law to convert wealth into population */
  def wealthToPopulationExponent: Double

  /** Create the initial state of the cities */
  def initialCities: Seq[City] = {
    val populations = configuration.initialPopulations.toSeq
    val wealths = rescaleWealth(configuration.initialPopulations.map(populationToWealth), populations)

    (populations zip wealths).map {
      case (p, w) => City(population = p, wealth = w)
    }
  }

  /**
    * Estimate the wealth of a city from its population
    *
    * @param population population of the city
    * @return the estimated wealth
    */
  def populationToWealth(population: Double): Double = pow(population, populationToWealthExponent)

  /**
    * Center the wealth distribution on the population distribution
    *
    * @param wealth the wealth distribution
    * @param population the population distribution
    * @return the centered wealth distribution
    */
  def rescaleWealth(wealth: Seq[Double], population: Seq[Double]): Seq[Double] = {
    val factor = population.sum / wealth.sum.toDouble
    wealth.map(_ * factor)
  }

  /** Initial state of the model */
  def initialState: State = {
    val cities = initialCities
    State(0, cities.toVector, cities.size)
  }

  /** Iterate through the states of the model */
  def states: Iterator[Try[State]] =
    Iterator.iterate(Try(initialState)) {
      _ match {
        case Success(s) => Try(nextState(s))
        case s @ Failure(_) => s
      }
    }.takeWhileInclusive {
      _ match {
        case Success(s) => !ended(s)
        case Failure(e) => {println(e.getMessage);false}
      }
    }

  /**
    * Stop criterion on a maximum number of state
    *
    * @param state a state of the model
    * @return true if the simulation is finished
    */
  def ended(state: State): Boolean = state.step >= maxStep

  /**
    * The next state of the model
    *
    * @param state the current state
    * @return the subsequent state
    */
  def nextState(state: State): State = {
    def updatedCities: Seq[City] =
      (state.cities zip updatedWealths(state)).map {
        case (city, updatedWealth) =>
          city.copy(
            wealth = updatedWealth,
            population = updatedPopulation(city, updatedWealth,configuration.dates(state.step + 1) - configuration.dates(state.step))
          )
      }
    //println(state.step)
    state.copy(cities = updatedCities, step = state.step + 1)
  }

  /**
    * The updated wealth of the cities as a result of economic processes:
    *  - supply,
    *  - demand,
    *  - exchange balance.
    *
    * @param state the current state of the model
    * @return
    */
  def updatedWealths(state: State): Seq[Double] = {
    val supplies = state.cities.map(c => supply(c.population))
    val demands = state.cities.map(c => demand(c.population))

    (state.cities zip supplies zip demands zip exchangeBalances(state.cities, state.network, supplies, demands)).zipWithIndex.map {
      case ((((city, supply), demand), b), i) =>
        val newWealth = city.wealth + supply - demand + b
        if (city.wealth <= 0.0 || newWealth <= 0.0) 0.0 else newWealth
    }
  }

  /**
    * The updated population of a city from its updated wealth
    *
    * @param city the city to update
    * @param updatedWealth the updated wealth
    * @return the updated population
    */
  def updatedPopulation(city: City, updatedWealth: Double,deltaT: Double): Double = {
    //assert(updatedWealth >= 0, s"Negative wealth before conversion toPop $updatedWealth")
    // negative wealth can not happen (cf updatedWealth function)
    val wealthpopupdated = wealthToPopulation(updatedWealth)
    val wealthpop = wealthToPopulation(city.wealth)
    val deltaPopulation = deltaT*(wealthpopupdated - wealthpop) / economicMultiplier
    val updatedPopulation = city.population + deltaPopulation
    //assert(updatedPopulation >= 0, s"Negative population $updatedPopulation")
    //this does not make sense, take 0 if negative. Then some conservation equations may not hold ?
    //assert(!updatedPopulation.isInfinite,s"Infinite pop $updatedPopulation")
    assert(updatedPopulation<1e9,s"divergent population : updatedWealth $updatedWealth wealthtopop $wealthpop wealthpopupdated $wealthpopupdated")

    if (updatedPopulation >= 0.0) updatedPopulation else 0.0
  }

  /**
    * Supply of a city from its population
    *
    * @param population the population the city
    * @return the supply
    */
  def supply(population: Double): Double = {
    val supply = economicMultiplier * pow(population, sizeEffectOnSupply)
    assert(!supply.isInfinite,s"Infinite supply : pop $population")
    supply
  }

  /**
    * Demand of a city from its population
    *
    * @param population the population the city
    * @return the demand
    */
  def demand(population: Double): Double = {
    val demand = economicMultiplier * pow(population, sizeEffectOnDemand)
    assert(!demand.isInfinite,s"Infinite demand : pop $population")
    demand
  }

  /**
    * Technical data structure to memoize the matrix computation
    *
    * @param cities the cities
    * @param supplies the supplies of the city
    * @param demands the demands of the city
    * @param transacted the transaction matrix
    */
  case class Transacted(cities: Seq[City], supplies: Seq[Double], demands: Seq[Double], transacted: DenseMatrix) {
    lazy val transposedTransacted = transacted.transpose
    lazy val transactedFromSum = transacted.linesContent.map(_.sum)
    lazy val transactedToSum = transposedTransacted.linesContent.map(_.sum)
    lazy val nbCities = cities.size
  }

  /**
    * Valued balance of the cities resulting from exchanges.
    *
    * @param cities the cities
    * @param network the network of cities
    * @param supplies the supplies of the cities indexed by city index
    * @param demands the demands of the cities indexed by city index
    * @return the exchange balance of the cities indexed by city index
    */
  def exchangeBalances(
                        cities: Seq[City],
                        network: FullNetwork,
                        supplies: Seq[Double],
                        demands: Seq[Double]): Seq[Double] = {
    val transacted = Transacted(cities, supplies, demands, transactions(cities, network, supplies, demands))

    def unsatisfieds: Seq[Double] =
      for {
        (d, i) <- transacted.demands.zipWithIndex
      } yield d - transacted.transactedToSum(i)

    def unsolds: Seq[Double] =
      for {
        (s, i) <- transacted.supplies.zipWithIndex
      } yield s - transacted.transactedFromSum(i)

    (unsolds zip unsatisfieds zip bonuses(transacted) zip totalFixedCosts(transacted)).map {
      case (((unsold, unsatisfied), bonus), totalFixedCost) => unsatisfied - unsold + bonus - totalFixedCost
    }
  }

  /**
    * Match cities according to their interaction potential and compute the transacted quantity
    *
    * @param cities the cities
    * @param network the network of cities
    * @param supplies the supplies of the cities indexed by city index
    * @param demands the demands of the cities indexed by city index
    * @return the
    */
  def transactions(
                    cities: Seq[City],
                    network: FullNetwork,
                    supplies: Seq[Double],
                    demands: Seq[Double]): DenseMatrix = {

    val indexedSupplies = supplies.toIndexedSeq
    val indexedDemands = demands.toIndexedSeq

    val interactionMatrixValue = interactionPotentialMatrix(indexedSupplies, indexedDemands, network)
    val fromInteractionPotentialSum = interactionMatrixValue.linesContent.map(_.sum)
    val toInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, ip) =>
        if (ip > 0) {
          val fSupply = indexedSupplies(from)
          val tDemand = indexedDemands(to)
          val toIPSum = toInteractionPotentialSum(to)
          val fromIPSum = fromInteractionPotentialSum(from)
          assert(fSupply >= 0 && tDemand >= 0, s"supply or demand not good, $fSupply $tDemand")

          val normalisedIPFrom = if(fromIPSum > 0) ip / fromIPSum else 0.0
          val normalisedIPTo = if(toIPSum > 0 ) ip / toIPSum else 0.0

          val t = min(normalisedIPFrom * fSupply, normalisedIPTo * tDemand)
          assert(!t.isNaN, s"Transacted is NaN: from $from to $to , ip%from : $normalisedIPFrom supplyfrom  $fSupply todemand $tDemand ip%to $normalisedIPTo  fromipsum $fromIPSum toipsum $toIPSum")
          t
        } else 0.0
    }
  }

  /**
    * Compute the interaction potential matrix of the whole network of city.
    *
    * @param supplies masses of the cities of origin
    * @param demands masses of the cities of destination
    * @param network the interaction network
    * @return the interaction potential matrix as an adjacency matrix
    */
  def interactionPotentialMatrix(supplies: Seq[Double], demands: Seq[Double], network: FullNetwork): DenseMatrix = {
    val iM1 = supplies.toArray
    val iM2 = demands.toArray
    network.mapNodes {
      (i, j) => interactionPotential(iM1(i), iM2(j), distanceMatrix(i)(j))
    }
  }

  /**
    * Compute the interaction potential as a gravity model.
    *
    * @param mass1 mass of the 1st city
    * @param mass2 mass of the 2nd city
    * @param distance distance between the cities
    * @return
    */
  def interactionPotential(mass1: Double, mass2: Double, distance: Double): Double = {
    val potential: Double = (math.abs(mass1) * math.abs(mass2)) / math.pow(distance, distanceDecay)
    // this can not happen
    //assert(potential >= 0, s"Error in potential computing gave $potential for $mass1 $mass2 $distance")
    assert(!potential.isNaN,s"distance $distance mass1 $mass1 mass2 $mass2")
    potential
  }

  /**
    * Convert a quantity of wealth into a quantity of population
    *
    * @param wealth the stock of wealth
    * @return the matching population
    */
  def wealthToPopulation(wealth: Double): Double = pow(wealth, wealthToPopulationExponent)

  /**
    * Stub for bonuses injection. No bonuses are modeled in this version.
    *
    * @param transacted information on all transactions between cities
    * @return a sequence of bonuses indexed by city index
    */
  def bonuses(transacted: Transacted): Seq[Double] = transacted.supplies.map(_ => 0.0)

  /**
    * Stub for fixed costs injection. No fixed cost are modeled in this version.
    *
    * @param transacted information on all transactions between cities
    * @return a sequence of fixed costs indexed by city index
    */
  def totalFixedCosts(transacted: Transacted): Seq[Double] = transacted.supplies.map(_ => 0.0)

}



