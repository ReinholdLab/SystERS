#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param downstreamCellIdx the name of the downstream cell as a character.
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
        upstreamCellIdx = NULL,
        downstreamCellIdx = NULL,
        calculateOrder = NULL,
        initialize =
          function(boundaryIdx, currency, boundarySuperClass, upstreamCellIdx, downstreamCellIdx, calculateOrder){
            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass
            self$upstreamCellIdx <- upstreamCellIdx
            self$downstreamCellIdx <- downstreamCellIdx
            self$calculateOrder <- calculateOrder
          }

      )
  )

