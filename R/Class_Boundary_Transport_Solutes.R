#' @title Boundary_Transport_Solute
#'
#' @description Transport boundary for solutes between cells
#'
#' @param linkedBound the boundary containing the water that is advecting the solute in
#'   this bouddary
#' @param load the load (amount per time) of solute in this boundary
#' @param amount the amount of solute (mass or mols) in this boundary
#'
#' @export
#'
Boundary_Transport_Solute <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute",
    inherit = Boundary,
    public =
      list(
        load = NULL,
        amount = NULL,
        linkedBound = NULL,
        initialize =
          function(
            ..., linkedBound, load
          ){
            super$initialize(...)

            self$linkedBound <- linkedBound
            self$load <- load
            self$amount <- load * self$timeInterval
          } # close initialize
      ) # close public
  ) # close R6 class


#' @method trade_static
#'
#' @description Calculate the amount of solute to pass through the boundary.
#'
#' @return The amount of solute and the load to pass through the boundary.
#'
#' @export
#'
Boundary_Transport_Solute$set(
  which = "public",
  name = "trade_static",
  value =
    function(){
      # get the discharge and solute concentration in the water transport
      # boundary to which this solute transport boundary is linked
      discharge <- self$linkedBound$discharge # L s-1

      if(!self$usModBound) {
        upstreamConcentration <- self$upstreamCell$linkedCell$concentration # g  m-3
        # multiply discharge by concentration to get load
        self$load <- self$linkedBound$discharge * upstreamConcentration # g s-1
      }

      # mass to of solute to trade
      self$amount <- self$load * self$timeInterval # g

      if(!self$usModBound){
        # solute mass to remain
        soluteToRemain <- self$upstreamCell$soluteMass - soluteToTrade
        if(soluteToRemain < 0) warning(
          paste("You are about to remove more solute from a cell than it held at the start of the timestep.
                      Boundary is", self$boundaryIdx,
                "Cell is", self$upstreamCell$upstreamCellIdx
          )
        )
      }

      return(list(load = self$load, amount = self$amount))
    }
)
