#' @title Class Soil Boundary (R6)
#' Boundary that connects to soil cells.
#' @description Instantiate a boundary
#' @importFrom R6 R6Class
#' @export
#'
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
        #' @field SoilCellAbove The cell upstream of the boundary (\code{NA} if
        #'   the boundary is at the upstream edge of the model)
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
