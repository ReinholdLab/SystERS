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
        #' @param ... Parameters inherit from Class \code{\link{Boundary}}
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
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



        #' @method Method Boundary_Transport_Solute$trade
        #' @description Calculate the amount of solute to pass through the
        #'   boundary.
        #' @return Updates the \code{load} and \code{amount} in the boundary.
        #'   Returns a list of length 2 corresponding to both \code{load} and
        #'   \code{amount}.
        trade = function(){
          # get the discharge and solute concentration in the water transport
          # boundary to which this solute transport boundary is linked
          discharge <- self$linkedBound$discharge # L s-1

          if(!self$usModBound) {
            upstreamConcentration <- self$upstreamCell$concentration # g  m-3
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
                      Boundary is ",
              print(self$boundaryIdx)
              )
            )
          }

          return(list(load = self$load, amount = self$amount))
        }, # close trade function definition


        #' @method Method Boundary_Transport_Solute$store
        #' @description Runs the store method on solute cells in the model for
        #'   solute transport boundaries.
        #' @return Updated store values.
        store = function(){
          self$upstreamCell$amount <- self$upstreamCell$amount - self$amount
          self$downstreamCell$amount <- self$downstreamCell$amount + self$amount
          return(c(self$upstreamCell$amount, self$downstreamCell$amount))
        }

      ) # close public
  ) # close R6 class

