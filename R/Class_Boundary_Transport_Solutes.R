#' @title Class Boundary_Transport_Solute (R6)
#' Boundary to transport solutes between cells
#' @description Transport boundary for solutes between cells
#' @export
Boundary_Transport_Solute <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute",

    #' @inherit Boundary
    inherit = Boundary,

    public =
      list(
        #' @field load Load (amount per time) of solute moved through this
        #'   boundary
        load = NULL,
        #' @field amount Amount of solute (mass or mols) moved through this
        #'   boundary
        amount = NULL,
        #' @field linkedBound The boundary containing the water that is
        #'   advecting the solute in this boundary
        linkedBound = NULL,


        #' @description Instantiate a transport boundary for solutes between
        #'   cells
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
        #' @param boundarySuperClass String indicating the super class of the
        #'   boundary, e.g., \code{transport} or \code{reaction}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @param linkedBound Water boundary to which this solute boundary is
        #'   linked
        #' @param load The rate ([mols per time] or [mass per time]) at which
        #'   solute moves through this boundary
        #' @return A transport boundary for solutes between cells
        initialize =
          function(..., linkedBound, load){
            super$initialize(...)

            self$linkedBound <- linkedBound
            self$load <- load
            self$amount <- self$load * self$timeInterval
          }, # close initialize



        #' @method Method Boundary_Transport_Solute$trade_static
        #' @description Calculate the amount of solute to pass through the boundary.
        #' @return The amount of solute and the load to pass through the boundary.
        trade_static = function(){
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
            soluteToRemain <- self$upstreamCell$amount - self$amount
            if(soluteToRemain < 0) warning(
              paste("You are about to remove more solute from a cell than it held at the start of the timestep.
                      Boundary is", self$boundaryIdx,
                    "Cell is", self$upstreamCell$upstreamCellIdx
              )
            )
          }

          return(list(load = self$load, amount = self$amount))
        } # close trade_static function definition
      ) # close public
  ) # close R6 class

