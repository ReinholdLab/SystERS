#' @title Class SoilCell (R6) Soil cell
#' @description Instantiate a soil cell. Inherits from class
#'   \code{\link{Cell}}.
#' @importFrom R6 R6Class
#' @export

SoilCell<-
  R6::R6Class(
    Classname = "SoilCell",

    #' @inherit Cell return details
    inherit = Cell,

    public = list(
      #' @field field_capacity The max volume of water that can be held within the cell.
      field_capacity = NULL,
      #' @field cell_storage The amount of water already contained within the cell.
      cell_storage = NULL,
      #' @field number_cell User defined number of cells in a soil column.
      number_cell = NULL,
      #' @field ground_water The volume of water in the final soil cell.
      ground_water = NULL,

      #' @description Create a new water cell
      #' @param field_capacity The volume of water in the cell.
      #' @param cell_storage The volume of water already present in cell.
      #' @param number_cell The number of cells.
      #' @param ground_water The volume of water in the final soil cell.
      #' @param cellIdx Character string denoting the index for the cell
      #' @param processDomain Character string indicating process domain of
      #'   cell (soil, groundwater, or stream)
      #' @param currency Character string with either water or name of solute
      #' @param ... Inherited parameters
      #' @return The object of class \code{SoilCell}.
      #'

      initialize = function(..., field_capacity, cell_storage, number_cell, ground_water) {

          super$initialize(...)

          self$field_capacity <- field_capacity
          self$cell_storage <- cell_storage
          self$number_cell <- number_cell
          self$ground_water <- ground_water
        },
      transport_funct <- function() BoundarySoilTransport$new(self)
    )
  )

#' @title Class Soil Boundary (R6)
#' Boundary that connects to soil cells.
#' @description Instantiate a boundary
#' @importFrom R6 R6Class
#' @export

SoilBoundary <-
  R6::R6Class(
    classname = "SoilBoundary",

    public =
      list(
        #' @field boundaryIdx Character string naming the boundary
        boundaryIdx = NULL,
        #' @field currency Character string naming the currency
        currency = NULL,
        #' @field timeInterval Model time step
        timeInterval = NULL,
        #' @field SoilCellAbove The cell above the boundary (\code{NA} if
        #'   the boundary is above the edge of the model)
        SoilCellAbove = NULL,
        #' @field SoilCellBelow The cell downstream of the boundary (\code{NA} if
        #'   the boundary is at the downstream edge of the model)
        SoilCellBelow = NULL,
        #' @field usModBound Topologically, is the boundary at the upper soil cell of the model? (TRUE/FALSE)
        usModBound = NULL,
        #' @field dsModBound Topologically, is the boundary at the lower soil cell of the model? (TRUE/FALSE)
        dsModBound = NULL,

        #' @description Instantiate a boundary
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param SoilCellAbove  Cell (if one exists) above of the boundary
        #' @param SoilCellBelow Cell (if one exists) below of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary
        initialize =
          function(
    boundaryIdx,
    currency,
    timeInterval,

    SoilCellAbove,
    SoilCellBelow
          ){

            self$boundaryIdx <- boundaryIdx
            self$currency <- currency

            self$timeInterval <- timeInterval

            self$SoilCellAbove <- SoilCellAbove
            self$SoilCellBelow <- SoilCellBelow

            # Is this boundary a model boundary? Check to see if it has either
            # no u/s or no d/s cell...
            self$usModBound <- !(is.environment(self$upstreamCell)) # has no u/s cell
            self$dsModBound <- !(is.environment(self$downstreamCell)) # has no d/s cell

          }

      )
  )


#' @title Class Boundary_Soil_Transport (R6)
#' A model boundary that transports water to and from soil cells
#' @description Transport boundary for water between soil cells
#' @importFrom R6 R6Class
#' @export

BoundarySoilTransport <-
  R6::R6Class(
    classname = "BoundarySoilTransport",

    #' @inherit Boundary return details
    inherit = SoilBoundary,

    public =
      list(
        #' @field input Water input to the top boundary in user specified units (volume/time)
        input = NULL,
        #' @field spill_over Water volume passed through the boundary
        spill_over = NULL,

        #' @description Instantiate a water transport in soil boundary
        #' @param ... Parameters inherit from Class \code{\link{SoilBoundary}}
        #' @param input Volume of water entering the top soil cell (a.k.a. I)
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param SoilCellAbove  Cell (if one exists) above the soil boundary
        #' @param SoilCellBelow Cell (if one exists) below the soil boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that transports water
        initialize =
          function(..., input, SoilCell = SoilCell$new()){

            super$initialize(...)

            field_capacity <- SoilCell$self$field_capacity
            cell_storage <- SoilCell$self$cell_storage
            number_cell <- SoilCell$self$number_cell
            ground_water <- SoilCell$self$ground_water

            self$input <- as.numeric(input) # as.numeric is here in case reading from sparse table
            spill_over <- (input + cell_storage) - field_capacity

            if((input + cell_storage) <= field_capacity) {
              print(paste0("The final amount of water in soil cell", number_cell, "is:", inflow + cell_storage))
              cat("\n")

              cell_storage <- inflow + cell_storage

              if(cell_storage != 0) {
                ground_water <- cell_storage
              } else {
                ground_water <- 0
              }
              print(field_capacity, spill_over, ground_water)
              break
            } else if ((inflow + cell_storage) > field_capacity) {
              spill_over <- (inflow + cell_storage) - field_capacity
              cell_storage <- field_capacity

              print(paste0("The amount of spillover in cell", number_cell, "entering the next cell is: "), spill_over)
              cat("\n")
            }
          },

        #' @method Method Boundary_Soil_Transport$store
        #' @description Runs the store method on soil cells in the model.
        #' @return Updated store values.
        store = function(spill_over){
          inflow <- spill_over
          return(inflow)
        }
      )
  )
