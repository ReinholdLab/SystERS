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
    #' @param cellTypePorosity
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
      # do we need to add cellInput here and calculate spillOver here?
      #Or should this be using discharge in the Class_Boundary_Transport_Water class as the input?
      # self$cellInput <- cellInput

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

      self$cellPorosity <-
        if (self$cellSoilType %in% names(self$cellTypePorosity)) {
          cellPorosity <- self$cellTypePorosity[[self$cellSoilType]]
          } else {
          return(print("Err: Soil type not in dictionary."))
          }
        #search a list based on soil type for value

      self$saturationVolume <- self$cellPorosity * self$cellVolume



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
      browser()
      # self$waterVolume <- if(self$cellSpillOver > 0) {
      #   self$waterVolume <- self$saturationVolume
      # } else {
      #   self$waterVolume <- self$waterVolume + self$cellInput
      # }

      self$populateDependencies()

      return(self$waterVolume)
    }
  )
)



