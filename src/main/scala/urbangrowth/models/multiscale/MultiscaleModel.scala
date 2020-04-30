package urbangrowth.models.multiscale


import org.openmole.spatialdata.model.urbandynamics


case class MultiscaleModel(
                          model: urbandynamics.MultiscaleModel
                          )



object MultiscaleModel {


  def apply(timeSteps: Int,
            macroNcities: Int,
            macroInitialHierarchy: Double,
            macroInitialMaxPop: Double,
            macroRange : Double,
            macroGrowthRate: Double,
            macroInteractionDecay: Double,
            macroInteractionWeight: Double,
            macroInteractionGamma: Double,
            mesoGridSize: Int,
            mesoCenterDensity: Double,
            mesoAlpha: Double,
            mesoBeta: Double,
            mesoNdiff: Int,
            mesoTimeSteps: Int,
            macroMesoBetaUpdateMax: Double,
            macroMesoAlphaUpdateMax: Double,
            mesoMacroCongestionCost: Double,
            mesoMacroDecayUpdateMax: Double
           ): MultiscaleModel = MultiscaleModel(
    urbandynamics.MultiscaleModel(timeSteps, macroNcities, macroInitialHierarchy, macroInitialMaxPop, macroRange, macroGrowthRate, macroInteractionDecay, macroInteractionWeight,
      macroInteractionGamma, mesoGridSize, mesoCenterDensity, mesoAlpha, mesoBeta, mesoNdiff, mesoTimeSteps, macroMesoBetaUpdateMax, macroMesoAlphaUpdateMax, mesoMacroCongestionCost, mesoMacroDecayUpdateMax
    )
  )

}