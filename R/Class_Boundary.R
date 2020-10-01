#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCell  the upstream cell
#' @param downstreamCell the downstream cell
#' @param timeInterval  the model time step
#'
#' @export
#'
Boundary <-
  R6::R6Class(
    classname = "Boundary",
    public =
      list(
        boundaryIdx = NULL,
        currency = NULL,
        boundarySuperClass = NULL,

        timeInterval = NULL,

        upstreamCell = NULL,
        downstreamCell = NULL,

        usModBound = NULL,
        dsModBound = NULL,

        initialize =
          function(
            boundaryIdx,
            currency,
            boundarySuperClass,
            timeInterval,

            upstreamCell,
            downstreamCell
            ){

            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass

            self$timeInterval <- timeInterval

            self$upstreamCell <- upstreamCell
            self$downstreamCell <- downstreamCell

            # Is this boundary a model boundary? Check to see if it has either
            # no u/s or no d/s cell...
            self$usModBound <- !(is.environment(self$upstreamCell)) # has no u/s cell
            self$dsModBound <- !(is.environment(self$downstreamCell)) # has no d/s cell

          }

      )
  )





