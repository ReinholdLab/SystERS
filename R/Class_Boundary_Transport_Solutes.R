#' @title Class Boundary_Transport_Solute (R6)
#' Boundary to transport solutes between cells
#' @description Transport boundary for solutes between cells
#' @importFrom R6 R6Class
#' @export
Boundary_Transport_Solute <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute",

    #' @inherit Boundary return details
    inherit = Boundary,

    public =
      list(
        #' @field linkedBound The boundary containing the water that is
        #' #'   advecting the solute in this boundary
        linkedBound = NULL,
        #' @field processDomain processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        processDomain = NULL,


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
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @return A transport boundary for solutes between cells
        initialize =
          function(..., linkedBound, processDomain){
            super$initialize(...)

            self$processDomain <- processDomain

            self$linkedBound <- linkedBound
          } # close initialize



      ) # close public
  ) # close R6 class


#' @title Class Boundary_Transport_Solute_Stream (R6)
#' Boundary to transport solutes between stream cells
#' @description Transport boundary for solutes between stream cells
#' @importFrom R6 R6Class
#' @export
Boundary_Transport_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute_Stream",

    #' @inherit Boundary return details
    inherit = Boundary_Transport_Solute,

    public =
      list(
        #' @field load Load (amount per time) of solute moved through this
        #'   boundary
        load = NULL,
        #' @field amount Amount of solute (mass or mols) moved through this
        #'   boundary
        amount = NULL,
        #' #' @field linkedBound The boundary containing the water that is
        #' #'   advecting the solute in this boundary
        #' linkedBound = NULL,


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
        #'   @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @return A transport boundary for solutes between cells
        initialize =
          function(..., load){
            super$initialize(...)


            self$load <- load
            self$amount <- self$load * self$timeInterval

          }, # close initialize



        #' @method Method Boundary_Transport_Solute_Stream$trade
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
            # soluteToRemain <- (self$upstreamConcentration * self$linkedBound$Discharge) - self$amount
            soluteToRemain <- self$upstreamCell$amount - self$amount # I switched this because the solute amount entering is greater than what was in the cell



            if(soluteToRemain < 0) stop(
              paste("You are trying to remove more solute from a cell than it held at the start of the timestep.
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



#' @title Class Boundary_Transport_Solute_Soil (R6)
#' Boundary to transport solutes between Soil cells
#' @description Transport boundary for solutes between Soil cells
#' @importFrom R6 R6Class
#' @export
Boundary_Transport_Solute_Soil <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute_Soil",

    #' @inherit Boundary return details
    inherit = Boundary_Transport_Solute,

    public =
      list(
        #' @field massSoluteInCell The original mass of solute in the soil Cell
        massSoluteInCell = NULL,
        #' @field fracMassSpillOver The fraction of mass of solute leaving the soil cell
        fracMassSpillOver = NULL,


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
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @param massSoluteInCell The original mass of solute in the soil cell
        #' @param fracMassSpillOver The fraction of solute mass leaving the soil cell
        #' @return A transport boundary for solutes between soil cells
        initialize =
          function(...){
            super$initialize(...)


          }, # close initialize



        #' @method Method Boundary_Transport_Solute_Soil$trade
        #' @description Calculate the amount of solute to pass through the
        #'   boundary.
        #' @return Updates the \code{massSoluteInCell} and \code{fracMassSpillOver} in the boundary.
        #'   Returns a list of length 2 corresponding to both \code{massSoluteInCell} and
        #'   \code{fracMassSpillOver}.
        trade = function(){

          if(!self$usModBound) {

            #initial volume and mass
            totalVolume <- self$upstreamCell$linkedCell$waterVolume
            self$massSoluteInCell <- self$upstreamCell$concentration*totalVolume *0.90 #90% of solute concentration moving from what is in cell

            #mass and volume leaving cell
            volumeSpillOver <- self$upstreamCell$linkedCell$cellSpillOver
            self$fracMassSpillOver <- (volumeSpillOver/totalVolume) * self$massSoluteInCell

            #mass balance check
            massDifference <- self$massSoluteInCell - self$fracMassSpillOver
            if(massDifference < 0) {
              stop(print ("You are removing more solute from the cell then what it held origianlly."))
            }
          }

          return(list(massSoluteInCell = self$massSoluteInCell, fracMassSpillOver = self$fracMassSpillOver))
        },



        #' @method Method Boundary_Transport_Solute_Soil$store
        #' @description Runs the store method on soil solute cells in the model for
        #'   solute transport boundaries.
        #' @return Updated store values.
        store = function(){

          self$upstreamCell$massSoluteInCell <- self$massSoluteInCell - self$fracMassSpillOver
          self$downstreamCell$massSoluteInCell <- self$massSoluteInCell + self$fracMassSpillOver

          return(c(self$upstreamCell$massSoluteInCell, self$downstreamCell$massSoluteInCell))

        }

      ) # close public
  ) # close R6 class


