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
    #' @field fieldCapacity The max volume of water that can be held within the cell.
    fieldCapacity = NA,
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


    #' @description Create a new water cell
    #' @param fieldCapacity The max volume of water that can be in the cell.
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
    #' @return The object of class \code{Cell_Water_Soil}.


    initialize = function(..., cellLength, cellHeight, cellWidth,
                          cellPorosity, cellSoilType, initWaterVolume) {

      super$initialize(...)
      self$cellLength <- cellLength
      self$cellHeight <- cellHeight
      self$cellWidth <- cellWidth
      self$cellVolume <- cellLength * cellWidth * cellHeight
      self$cellPorosity <- cellPorosity #pore space/total volume, change this to search a list based on soil type for value
      # self$cellMatricPotential <- cellMatricPotential
      # self$cellGravimetricPotential <- cellGravimetricPotential
      self$cellSoilType <- cellSoilType
      self$waterVolume <- initWaterVolume #define the initial water volume
      # do we need to add cellInput here and calculate spillOver here?
      #Or should this be using discharge in the Class_Boundary_Transport_Water class as the input?
      # self$cellInput <- cellInput


      self$fieldCapacity <- cellPorosity * self$cellVolume #Really a measure of saturation volume currently. FC will be calculated later.

    },

    #' @method Method Cell_Water_Soil$populateDependencies
    #' @description Populates the fields in the cells that depend on
    #'   boundaries being instantiated before the trade, store, update
    #'   sequence can be run.
    #' @return Updates cell values for \code{cellSpillOver}
    #' based on cell values (\code{fieldCapacity, waterVolume}) and
    #'   upstream/downstream boundary values (\code{cellInput}).
    populateDependencies = function(){

      usWaterVolume <- sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$waterVolume)
      usfieldCapacity <- sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$fieldCapacity)

      dsWaterVolume <- sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$waterVolume)
      usfieldCapacity <- sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$fieldCapacity)


    },

    #' @method Method Cell_Water_Soil$update
    #' @description Runs the update method on all cells of class
    #'   \code{Cell_Water_Soil}.  In this current version of the model,
    #'   this simply adjusts the height of the water in the stream cell
    #'   based on the water volume, i.e., it holds the channel area constant
    #'   with changes in discharge.
    #' @return Updates cell values based on trades and stores.
    update = function(){

      self$populateDependencies()

      return()
    }
  )
)



