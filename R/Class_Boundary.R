#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param downstreamCellIdx the name of the downstream cell as a character.
#'
Boundary <-
  R6::R6Class(
    classname = "Boundary",
    public =
      list(
        boundaryIdx = NULL,
        upstreamCellIdx = NULL,
        downstreamCellIdx = NULL,
        initialize =
          function(){
            self$boundaryIdx <- boundaryIdx
            self$upstreamCellIdx <- upstreamCellIdx
            self$downstreamCellIdx <- downstreamCellIdx
          }

      )
  )


#' @title waterTransportPerTime
#'
#' @description Calculate the discharge of water (Volume/Time).
#'
#' @param boundaryIdx the name of the boundary.
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param additionalWaterInputOrLoss the discharge (Volume/Time) of water to be
#'   added, if any.  Units must match those of @param discharge, etc.  Default
#'   value is \code{0}.
#'
#' @return the discharge of water to pass onto the downstream cell.
#'
Boundary$set(
  which = "public",
  name = "waterTransportPerTime",
  value =
    function(
      boundaryIdx,
      upstreamCell,
      additionalWaterInputOrLoss = 0
    ){
      return(upstreamCell$discharge + additionalWaterInputOrLoss)
    }
)


#' @title soluteTransportPerTime
#'
#' @description Calculate load, i.e., the rate of solute mass moving from the
#'   upstream cell into the boundary (Mass/Time).
#'
#' @param boundaryIdx
#'
#' @param boundaryIdx the name of the boundary.
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param additionalSoluteInputOrLoss the concentration of solute to be added,
#'   if any.  Units must be consistent with other measures of concentration.
#'   Default value is \code{0}.
#'
#' @return the load to pass onto the downstream cell
Boundary$set(
  which = "public",
  name = "soluteTransportPerTime",
  value =
    function(
      boundaryIdx,
      upstreamCell,
      additionalSoluteInputOrLoss = 0
    ){
      return(
        upstreamCell$discharge * upstreamCell$soluteConcentration +
        boundaryIdx$waterTransportPerTime * additionalSoluteInputOrLoss
        )
    }
)

