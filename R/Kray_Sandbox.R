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
    #' @field field_capacity The max volume of water that can be held within the cell.
    field_capacity = NA,
    #' @field waterVolume The volume of water stored in cell.
    waterVolume = NULL,
    #' @field cellVolume The volume of the soil cell.
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
    #' @field cellSoilType The soil type of the soil cell.For notation only.
    cellSoilType = NULL,
    #' @field cellStorage The amount of water in storage within the soil cell.
    cellStorage = NULL,


    #' @description Create a new water cell
    #' @param field_capacity The max volume of water that can be in the cell.
    #' @param waterVolume The volume of water stored in cell.
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
    #' @param cellSoilType The soil type of the soil cell. For notation only.
    #' @param cellStorage The amount of water in storage within the soil cell.
    #' @return The object of class \code{Cell_Water_Soil}.


    initialize = function(...,field_capacity, waterVolume, cellLength, cellHeight, cellWidth,
                          cellPorosity, cellMatricPotential, cellSoilType, cellStorage) {

      super$initialize(...)
      self$cellLength <- cellLength
      self$cellHeight <- cellHeight
      self$cellWidth <- cellWidth
      self$cellPorosity <- cellPorosity
      self$waterVolume <- waterVolume
      self$cellVolume <- cellLength * cellWidth * cellHeight
      self$cellMatricPotential <- cellMatricPotential
      self$cellSoilType <- cellSoilType
      self$cellStorage <- cellStorage

      self$field_capacity <- self$cellPorosity * self$cellLength * self$cellHeight * self$cellWidth

    }
  )
)
