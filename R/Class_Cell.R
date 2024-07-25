#' @title Class Cell (R6)
#' Model cell
#' @description Instantiate a \code{Cell}.
#' @importFrom R6 R6Class
#' @export
Cell <-
  R6::R6Class(
    classname = "Cell",
    public =
      list(
        #' @field cellIdx Cell index
        cellIdx = NULL,
        #' @field processDomain Process domain of the cell
        processDomain = NULL,
        #' @field currency Currency of the cell
        currency = NULL,
        #' @field linkedBoundsList List of boundaries linked to cell
        linkedBoundsList = NULL,

        #' @description Instantiate a \code{Cell} object.
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @return The ojbect of class \code{Cell}.
        initialize = function(cellIdx, processDomain, currency){
            self$cellIdx <- cellIdx
            self$processDomain <- processDomain
            self$currency <- currency
            self$linkedBoundsList <- list(upstreamBounds = list(), downstreamBounds = list())
          },
        #' @method Method Cell$populateDependencies
        #' @description Placeholder for populate dependencies methods.
        #' @return NULL
        populateDependencies = function(){
          NULL
        }

      )
  )



#' @title Class Cell_Water (R6) Water cell
#' @description Instantiate a water cell. Inherits from class
#'   \code{\link{Cell}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water <-
  R6::R6Class(
    classname = "Cell_Water",

    #' @inherit Cell return details
    inherit = Cell,

    public =
      list(
        #' @field waterVolume volume of water stored in cell
        waterVolume = NULL,
        #' @field linkedSoluteCells solute cells that are linked to the water
        #'   cell
        linkedSoluteCells = NULL,

        #' @description Create a new water cell
        #' @param waterVolume the volume of water in the cell
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param ... Inherited parameters
        #' @return The object of class \code{Cell_Water}.

        initialize =
          function(..., waterVolume = NULL){
            super$initialize(...)

            self$waterVolume <- waterVolume
          }
      )
)


#' @title Class Cell_Water_Stream (R6) A water cell in the stream processing
#'   domain
#' @description Instantiate a water cell. Inherits from class
#'   \code{\link{Cell_Water}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water_Stream <-
  R6::R6Class(
    classname = "Cell_Water_Stream",

    #' @inherit Cell_Water return details
    inherit = Cell_Water,
    public =
      list(
        #' @field channelWidth Average width of stream channel in cell
        channelWidth = NULL,
        #' @field channelLength Average length of channel in stream cell
        channelLength = NULL,
        #' @field channelArea Area of surface water in stream cell
        channelArea = NULL,
        #' @field channelDepth Average depth of channel in stream cell
        channelDepth = NULL,
        #' @field waterVolume The volume of water in the cell calculated from
        #'   the \code{channelLength, channelArea, channelDepth} parameters
        waterVolume = NULL,
        #' @field channelResidenceTime Mean residence time of water in the
        #'   channel compartment
        channelResidenceTime = NULL,
        #' @field hydraulicLoad The hydraulic load of water in the channel
        #'   compartment
        hydraulicLoad = NULL,

        #' @description Instantiate a \code{Class Cell_Water_Stream} object.
        #'   Class \code{Class Cell_Water_Stream} inherits from class
        #'   \code{Cell}.
        #' @param ... Parameters inherit from Class \code{\link{Cell_Water}} and
        #'   thus \code{\link{Cell}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param channelWidth the width of the stream channel surface (distance
        #'   from left bank to right bank)
        #' @param channelLength is the length of the stream channel surface for
        #'   the cell (distance of cell from upstream to downstream)
        #' @param channelDepth  the height of the water surface above the
        #'   streambed
        #' @return The object of class \code{Cell_Water_Stream}.
        #'
        initialize =
          function(
            ...,
            channelWidth,
            channelLength,
            channelDepth
          ){
            channelArea <- channelWidth * channelLength
            waterVolume <- channelArea * channelDepth

            super$initialize(...)

            self$channelWidth <- channelWidth
            self$channelLength <- channelLength
            self$channelDepth <- channelDepth
            self$channelArea <- channelArea
            self$waterVolume <- waterVolume

          },


        #' @method Method Cell_Water_Stream$populateDependencies
        #' @description Populates the fields in the cells that depend on
        #'   boundaries being instantiated before the trade, store, update
        #'   sequence can be run.
        #' @return Updates cell values for \code{channelResidenceTime,
        #'   hydraulicLoad} based on cell values (\code{channelLength}) and
        #'   upstream/downstream boundary values (\code{channelVelocity}).
        populateDependencies = function(){

          usChannelVelocity <- mean(sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$channelVelocity))
          dsChannelVelocity <- mean(sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$channelVelocity))

          channelVelocity <- mean(c(usChannelVelocity, dsChannelVelocity))

          self$channelResidenceTime <- self$channelLength / channelVelocity
          self$hydraulicLoad <- self$channelDepth / self$channelResidenceTime
        },

        #' @method Method Cell_Water_Stream$update
        #' @description Runs the update method on all cells of class
        #'   \code{Cell_Water_Stream}.  In this current version of the model,
        #'   this simply adjusts the height of the water in the stream cell
        #'   based on the water volume, i.e., it holds the channel area constant
        #'   with changes in discharge.
        #' @return Updates cell values based on trades and stores.
        update = function(){

          self$channelDepth <- self$waterVolume / self$channelArea

          self$populateDependencies()

          return()
        }

      )
  )


#' @title Class Cell_Water_Soil (R6) Soil cell
#' @description Instantiate a soil cell. Inherits from class
#'   \code{\link{Cell_Water}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water_Soil <- R6::R6Class(
  classname = "Cell_Water_Soil",

  #' @inherit Cell return details
  inherit = Cell_Water,

  public = list(
    #' @field saturationVolume The max volume of water that can be held within the cell.
    saturationVolume = NA,
    #' @field cellVolume The volume of the soil cell calculate from from
    #'   the \code{channelLength, channelWidth, channelHeight} parameters.
    cellVolume = NULL,
    #' @field cellHeight The height of the soil cell.
    cellHeight = NULL,
    #' @field cellWidth The width of the soil cell.
    cellWidth = NULL,
    #' @field cellLength The length of the soil cell.
    cellLength = NULL,
    #' @field cellPorosity The porosity of the soil cell.
    cellPorosity = NULL,
    #' @field cellMatricPotential The matric potential of the soil cell.
    cellMatricPotential = NULL,
    #' @field cellGravimetricPotential The total pressure gradient for the soil cell.
    cellGravimetricPotential = NULL,
    #' @field cellSoilType The soil type of the cell. For notation purposes only.
    cellSoilType = NULL,
    #' @field waterVolume The volume of water stored in the soil cell.
    waterVolume = NULL,
    #' @field cellInput The volume of water entering the soil cell.
    cellInput = NULL,
    #' @field cellSpillOver The volume of water exiting the soil cell.
    cellSpillOver = NULL,
    #' @field cellTypePorosity List of soil types and matching porosity values.
    cellTypePorosity = NULL,
    #' @field cellTypeHydraulicConductivity List of average hydraulic conductivity
    #' for soil types with units of m s-1. Values found on https://structx.com/Soil_Properties_007.html
    cellTypeHydraulicConductivity = NULL,
    #' @field cellHydraulicConductivity The average hydraulic conductivity
    #' assigned based on soil type with units of m s-1.
    cellHydraulicConductivity = NULL,

    #' @description Create a new water cell
    #' @param saturationVolume The max volume of water that can be in the cell.
    #' @param cellIdx Character string denoting the index for the cell
    #' @param processDomain Character string indicating process domain of
    #'   cell (soil, groundwater, or stream)
    #' @param currency Character string with either water or name of solute
    #' @param ... Inherited parameters
    #' @param cellVolume The volume of the soil cell.
    #' @param cellHeight The height of the soil cell.
    #' @param cellWidth The width of the soil cell.
    #' @param cellLength The length of the soil cell.
    #' @param cellPorosity The porosity of the soil cell.
    #' @param cellMatricPotential The matric potential of the soil cell.
    #' @param cellGravimetricPotential The total pressure gradient for the soil cell.
    #' @param cellSoilType The soil type of the cell. For notation purposes only.
    #' @param waterVolume The volume of water already present in the cell.
    #' @param cellInput The volume of water entering the soil cell.
    #' @param cellSpillOver The volume of water exiting the soil cell.
    #' @param cellTypePorosity List of soil types and matching porosity values.
    #' @param cellTypeHydraulicConductivity List of average hydraulic conductivity
    #' for soil types with units of m s-1.
    #' @param cellHydraulicConductivity The average hydraulic conductivity
    #' assigned based on soil type with units of m s-1.
    #' @return The object of class \code{Cell_Water_Soil}.


    initialize = function(..., cellLength, cellHeight, cellWidth,
                          cellSoilType, initWaterVolume) {

      super$initialize(...)
      self$cellLength <- cellLength
      self$cellHeight <- cellHeight
      self$cellWidth <- cellWidth
      self$cellVolume <- cellLength * cellWidth * cellHeight
      # self$cellMatricPotential <- cellMatricPotential
      # self$cellGravimetricPotential <- cellGravimetricPotential
      self$cellSoilType <- gsub("([A-Za-z])\\s+([A-Za-z])", "\\1\\2", cellSoilType)
      self$cellSoilType <- tolower(self$cellSoilType)
      self$waterVolume <- initWaterVolume #define the initial water volume

      #Currently from https://stormwater.pca.state.mn.us/index.php/Soil_water_storage_properties. Probably better resources?
      self$cellTypePorosity <- list(
        sand = 0.43,
        loamysand = 0.44,
        sandyloam = 0.45,
        loam = 0.47,
        siltloam = 0.50,
        sandyclayloam = 0.4,
        clayloam = 0.46,
        siltyclayloam = 0.49,
        sandyclay = 0.47,
        clay = 0.47)

      #Average hydraulic conductivity
      # Brady, N. C., & Weil, R. R. (2002). The Nature and Properties of Soils (13th ed.). Prentice Hall.
      # Sumner, M. E. (2000). Handbook of Soil Science. CRC Press.
      self$cellTypeHydraulicConductivity <- list( #with units of m s-1
        sand = 1e-2,
        loamysand = 1e-3, #from first loamy row
        sandyloam = 1e-4,
        loam = 1e-5, #from second loamy row
        siltloam = 1e-6,
        sandyclayloam = 1e-7,
        clayloam = 1e-8,
        siltyclayloam = 1e-9,
        sandyclay = 1e-10, #from ninth row (sandy clayey loam)
        clay = 1e-11)

      #search a list based on soil type for value
      self$cellPorosity <-
        if (self$cellSoilType %in% names(self$cellTypePorosity)) {
          cellPorosity <- self$cellTypePorosity[[self$cellSoilType]]
        } else {
          return(print("Err: Soil type not in dictionary."))
        }

      self$cellHydraulicConductivity <-
        if (self$cellSoilType %in% names(self$cellTypeHydraulicConductivity)) {
          cellHydraulicConductivity <- self$cellTypeHydraulicConductivity[[self$cellSoilType]]
        } else {
          return(print("Err: Soil type not in dictionary."))
        }


      self$saturationVolume <- self$cellPorosity * self$cellVolume
      self$cellSpillOver <- 0

    },

    #' @method Method Cell_Water_Soil$populateDependencies
    #' @description Populates the fields in the cells that depend on
    #'   boundaries being instantiated before the trade, store, update
    #'   sequence can be run.
    #' @return Updates cell values for \code{cellSpillOver}
    #' based on cell values (\code{saturationVolume, waterVolume}) and
    #'   upstream/downstream boundary values (\code{cellInput}).
    populateDependencies = function(){

      usWaterVolume <- sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$waterVolume)
      usSaturationVolume <- sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$saturationVolume)

      usSpillOver <- sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$spillOver)

      dsWaterVolume <- sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$waterVolume)
      dsSaturationVolume <- sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$saturationVolume)


    },

    #' @method Method Cell_Water_Soil$update
    #' @description Runs the update method on all cells of class
    #'   \code{Cell_Water_Soil}.  In this current version of the model,
    #'   this simply adjusts the height of the water in the stream cell
    #'   based on the water volume, i.e., it holds the channel area constant
    #'   with changes in discharge.
    #' @return Updates cell values based on trades and stores.
    update = function(){

      self$waterVolume <- if(self$cellSpillOver > 0) {
        self$waterVolume <- self$saturationVolume
      } else {
        self$waterVolume <- self$waterVolume + self$cellInput
      }

      self$populateDependencies()

      return(self$waterVolume)
    }
  )
)



#' @title Class Cell_Solute (R6)
#' A cell containing a solute.  Must be linked to a water cell.
#' @description Instantiate a \code{Cell_Solute} object. Class
#'   \code{Cell_Solute} inherits from class \code{Cell}.
#' @importFrom R6 R6Class
#' @export

Cell_Solute <-
  R6::R6Class(
    classname = "Cell_Solute",

    #' @inherit Cell return details
    inherit = Cell,
    public =
      list(
        #' @field linkedCell The water cell to which the solute cell is linked
        linkedCell = NULL,
        #' @field linkedReactionCells solute cells that are linked to the reaction
        #'   cell
        linkedReactionCells = NULL,


        #' @param ... Parameters inherit from Class \code{\link{Cell}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param linkedCell the cell containing the water in which this solute is located
        #' @return The object of class \code{Cell_Solute}.

        initialize =
          function(..., linkedCell){
            super$initialize(...)

            self$linkedCell <- linkedCell
          }
      )
  )


#' @title Class Cell_Solute_Stream (R6)
#' A stream cell containing a solute.  Must be linked to a solute cell.
#' @description Instantiate a \code{Cell_Solute_Stream} object. Class
#'   \code{Cell_Solute_Stream} inherits from class \code{Cell_Solute}.
#' @importFrom R6 R6Class
#' @export

Cell_Solute_Stream <-
  R6::R6Class(
    classname = "Cell_Solute_Stream",

    #' @inherit Cell return details
    inherit = Cell_Solute,
    public =
      list(
        #' @field concentration Solute concentration in user specified units;
        #'   user must ensure consistency in units
        concentration = NULL,
        #' @field amount Solute amount in user specified units (mass or mols)
        amount = NULL,


        #' @param ... Parameters inherit from Class \code{\link{Cell_Solute}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param concentration the concentration of the solute in user specified units
        #'   (mass or mols per unit volume)
        #' @param linkedCell the cell containing the water in which this solute is located
        #' @return The object of class \code{Cell_Solute_Stream}.

        initialize =
          function(..., concentration){
            super$initialize(...)

            self$concentration <- concentration
            self$amount <- self$concentration * self$linkedCell$waterVolume #initial amount of solute in the cell
          },

        #' @method Method Cell_Solute_Stream$update
        #' @description Runs the update method on all cells of class
        #'   \code{Cell_Solute_Stream}.
        #' @return Updates cell values based on trades and stores.
        update = function(){

          self$concentration <- self$amount / self$linkedCell$waterVolume

          return()
        }


      )
  )


#' @title Class Cell_Solute_Soil (R6)
#' A soil cell containing a solute.  Must be linked to a solute cell.
#' @description Instantiate a \code{Cell_Solute_Soil} object. Class
#'   \code{Cell_Solute_Soil} inherits from class \code{Cell_Solute}.
#' @importFrom R6 R6Class
#' @export

Cell_Solute_Soil <-
  R6::R6Class(
    classname = "Cell_Solute_Soil",

    #' @inherit Cell return details
    inherit = Cell_Solute,
    public =
      list(
        #' @field concentration Solute concentration in user specified units;
        #'   user must ensure consistency in units
        concentration = NULL,
        #' @field massSoluteInCell The original mass of solute in the soil Cell
        massSoluteInCell = NULL,
        #' @field fracMassSpillOver The fraction of mass of solute leaving the soil cell
        fracMassSpillOver = NULL,


        #' @param ... Parameters inherit from Class \code{\link{Cell_Solute}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param concentration the concentration of the solute in user specified units
        #'   (mass or mols per unit volume)
        #' @param linkedCell the cell containing the water in which this solute is located
        #' @param massSoluteInCell The original mass of solute in the soil cell
        #' @param fracMassSpillOver The fraction of solute mass leaving the soil cell
        #' @return The object of class \code{Cell_Solute_Soil}.

        initialize =
          function(..., concentration){
            super$initialize(...)


            self$concentration <- concentration
          },

          #' @method Method Cell_Solute$update
          #' @description Runs the update method on all cells of class
          #'   \code{Cell_Solute}.
          #' @return Updates cell values based on trades and stores.
          update = function(){

            self$concentration <- (self$massSoluteInCell - self$fracMassSpillOver) / self$linkedCell$waterVolume
            return()
          }
      )
  )



#' @title Class Cell_Water_Soil_Rxn (R6) Soil_Rxn cell
#' @description Instantiate a Soil_Rxn cell. Inherits from class
#'   \code{\link{Cell_Water}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water_Soil_Rxn <- R6::R6Class(
  classname = "Cell_Water_Soil_Rxn",

  #' @inherit Cell return details
  inherit = Cell, #is this the best cell to inherit from? cell_solute or cell_water_soil

  public = list(
    #' @field cellIdx Cell index
    cellIdx = NULL,
    #' @field processDomain Process domain of the cell
    processDomain = NULL,
    #' @field currency Currency of the cell
    currency = NULL,
    #' @field linkedBoundsList List of boundaries linked to cell
    linkedBoundsList = NULL,
    #' @field reactionVolume The volume of water in the rxn cell
    reactionVolume = NULL,
    #' @field initSoluteMass The mass of the solute in the reaction cell
    initSoluteMass = NULL,
    #' @field reactionConstant First-order reaction constant in the rxn cell
    reactionConstant = NULL,
    #' @field soluteMassReacted The final mass of the solute after rxn
    soluteMassReacted = NULL,
    #' @field linkedCell The water cell to which the solute cell is linked
    linkedCell = NULL,


    #' @param ... Parameters inherit from Class \code{\link{Cell}}
    #' @param cellIdx Character string denoting the index for the cell
    #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
    #' @param currency Character string with either water or name of solute
    #' @param reactionVolume The volume of water in the rxn cell
    #' @param initSoluteMass The mass of the solute in the reaction cell
    #' @param reactionConstant The first-order reaction constant n the rxn cell
    #' @param soluteMassReacted The final mass of the solute after rxn
    #' @param linkedCell the cell containing the water in which this solute is located
    #' @return The ojbect of class \code{Cell_Water_Soil_Rxn}.


    initialize = function(..., reactionConstant, linkedCell,
                          reactionVolume) {

      super$initialize(...)

      browser()

      self$linkedCell <- linkedCell

      self$reactionConstant <- reactionConstant

      self$reactionVolume <- reactionVolume
      self$initSoluteMass <- self$linkedCell$concentration #the reaction cell
      #should have a fraction of the total solute cell concentration; the transport
      #boundary should have the other fraction


    },


    #' @method Method Cell_Water_Soil_Rxn$update
    #' @description Runs the trade method on all cells of class
    #'   \code{Cell_Water_Soil_Rxn}.  In this current version of the model,
    #'   this simply adjusts the solute amount in the soil cell
    #'   based on the reaction constant.
    #' @return Updates cell values based on trades and stores.
    update = function(){

      return()

    }
  )
)





