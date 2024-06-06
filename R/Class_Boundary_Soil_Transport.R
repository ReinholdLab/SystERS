#' #' @title Class Boundary_Soil_Transport (R6)
#' #' A model boundary that transports water to and from soil cells
#' #' @description Transport boundary for water between soil cells
#' #' @importFrom R6 R6Class
#' #' @export
#'
#' BoundarySoilTransport <-
#'   R6::R6Class(
#'     classname = "BoundarySoilTransport",
#'
#'     #' @inherit Boundary return details
#'     inherit = SoilBoundary,
#'     public =
#'       list(
#'         #' @field input Water input to the top boundary in user specified units (volume/time)
#'         input = NULL,
#'         #' @field spill_over Water volume passed through the boundary
#'         spill_over = NULL,
#'
#'         #' @description Instantiate a water transport in soil boundary
#'         #' @param ... Parameters inherit from Class \code{\link{SoilBoundary}}
#'         #' @param input Volume of water entering the top soil cell (a.k.a. I)
#'         #' @param boundaryIdx String indexing the boundary
#'         #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
#'         #' @param SoilCellAbove  Cell (if one exists) above the soil boundary
#'         #' @param SoilCellBelow Cell (if one exists) below the soil boundary
#'         #' @param timeInterval  Model time step
#'         #' @return A model boundary that transports water
#'         initialize =
#'           function(..., input){
#'
#'             super$initialize(...)
#'
#'             self$input <- as.numeric(input) # as.numeric is here in case reading from sparse table
#'           },
#'
#'         #' @method Method Boundary_Soil_Transport$store
#'         #' @description Runs the store method on soil cells in the model.
#'         #' @return Updated store values.
#'         store = function(){
#'           #I'm not sure what to do here
#'           return() #what goes in here?
#'         }
#'       )
#'   )
