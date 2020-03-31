#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCell  the upstream cell
#' @param downstreamCell the downstream cell
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
        upstreamCell = NULL,
        downstreamCell = NULL,
        calculateOrder = NULL,
        initialize =
          function(boundaryIdx, currency, boundarySuperClass, upstreamCell, downstreamCell, calculateOrder){
            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass
            self$upstreamCell <- upstreamCell
            self$downstreamCell <- downstreamCell
            self$calculateOrder <- calculateOrder
          }

      )
  )

