#' @title Class Boundary (R6)
#' Boundary that connects to cells.
#' @description Instantiate a boundary
#' @export
#'
Boundary <-
  R6::R6Class(
    classname = "Boundary",
    public =
      list(
        #' @field boundaryIdx Character string naming the boundary
        boundaryIdx = NULL,
        #' @field currency Character string naming the currency
        currency = NULL,
        #' @field boundarySuperClass Character string describing whether the
        #'   boundary is a transport or reaction boundary
        boundarySuperClass = NULL,
        #' @field timeInterval Model time step
        timeInterval = NULL,
        #' @field upstreamCell The cell upstream of the boundary (\code{NA} if
        #'   the boundary is at the upstream edge of the model)
        upstreamCell = NULL,
        #' @field downstreamCell The cell downstream of the boundary (\code{NA} if
        #'   the boundary is at the downstream edge of the model)
        downstreamCell = NULL,
        #' @field usModBound Topologically, is the boundary at the upstream edge of the model? (TRUE/FALSE)
        usModBound = NULL,
        #' @field dsModBound Topologically, is the boundary at the downstream edge of the model? (TRUE/FALSE)
        dsModBound = NULL,

        #' @description Instantiate a boundary
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param boundarySuperClass String indicating the super class of the boundary, e.g., \code{transport} or \code{reaction}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary
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





