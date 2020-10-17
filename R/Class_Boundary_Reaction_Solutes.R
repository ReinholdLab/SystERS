#' @title Class Boundary_Reaction_Solute (R6)
#' Reaction boundary for a solute
#' @description Reaction boundary for a solute
#' @export

Boundary_Reaction_Solute <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute",

    #' @inherit Boundary
    inherit = Boundary_Reaction_Solute,

    public =
      list(
        #' @field fractionRemovedStorage Fraction of solute removed from storage
        #'   by the boundary
        fractionRemovedStorage = NULL,
        #' @field fractionRemainingStorage Fraction of solute remaining in
        #'   storage by the boundary
        fractionRemainingStorage = NULL,
        #' @field mustBeOne Used for an error check to ensure that the fraction
        #'   of solute removed from and remaining in a cell sum to 1
        mustBeOne = NULL,
        #' @field startingAmount Amount of solute prior to reaction
        startingAmount = NULL,
        #' @field amountToRemove Amount of solute the reaction boundary removes
        amountToRemove = NULL,
        #' @field amountToRemain Amount of solute remaining after the reaction
        amountToRemain = NULL,
        #' @field processDomain Process domain of cell on which the reaction
        #'   boundary is interacting
        processDomain = NULL,
        #' @field rxnVals A data frame storing the key reaction boundary inputs
        #'   and outputs
        rxnVals = NULL,

        #' @description Instantiate a reaction boundary for a solute
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
        #' @param boundarySuperClass String indicating the super class of the
        #'   boundary, e.g., \code{transport} or \code{reaction}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @return The ojbect of class \code{Boundary_Reaction_Solute}.
        initialize =
          function(...){
            super$initialize(...)
            self$processDomain <- self$upstreamCell$processDomain
          },

        #' @method Method Boundary_Reaction_Solute$calc_removal_storage
        #' @description Calculates the fraction solute to remove from storage
        #'   based on either a user specified percent or a power law residence
        #'   time weighted approach
        #' @param pcntToRemove Fraction of solute to remove from storage
        #' @return
        calc_removal_pcnt = function(method, pcntToRemove = NULL){

          if(method == "RT-PL"){
            self$fractionRemovedStorage <- self$calc_fracRemoval_resTimeWtdPowerLaw(remaining = FALSE)
            self$fractionRemainingStorage <- self$calc_fracRemoval_resTimeWtdPowerLaw(remaining = TRUE)
          } else if(method == pcnt){
            self$fractionRemovedStorage <- pcntToRemove/100
            self$fractionRemainingStorage <- 1 - self$fractionRemovedStorage
          }
        },

        #' @method Method
        #'   Boundary_Reaction_Solute$calc_fracRemoval_resTimeWtdPowerLaw
        #' @description Calculate fraction of solute removed/remainig from a
        #'   fully saturated storage zone using a residence-time weighted
        #'   removal function assuming a power law residence time.  If remaining
        #'   = TRUE, then the function calculates the fraction of the solute
        #'   REMAINING; however if remaining = FALSE, then the function
        #'   calculates the fraction of solute REMOVED
        #' @param tauMin Minimum residence time to consider
        #' @param tauMax Maximum residence time to consider
        #' @param alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        #' @param k Uptake constant for solute in units of T-1
        calc_fracRemoval_resTimeWtdPowerLaw =
          function(tauMin, tauMax, alpha, k, remaining){

              propUptkFunc <-
                function(
                  tau,
                  tauMin,
                  tauMax,
                  alpha,
                  k,
                  remaining
                ){
                  PL_PDF <- hydrogeom::powerLawPDF(tau, tauMin, tauMax, alpha)
                  minus_k_t <- (-1*k*tau) # if the -1  is removed (and this simply expressed as -k*tau), an "invalid argument to unary operator" error is thrown

                  if(remaining){
                    out <- PL_PDF * exp(minus_k_t)
                  }else{
                    out <- PL_PDF * (1-exp(minus_k_t))
                  }
                  return(out)
                }

              propUptk <-
                integrate(
                  propUptkFunc,
                  lower = self$tauMin,
                  upper = self$tauMax,
                  tauMin = self$tauMin,
                  tauMax = self$tauMax,
                  alpha = self$alpha,
                  k = self$k,
                  remaining = remaining
                )$value
              return(propUptk)
          },




        #' @method Method Boundary_Reaction_Solute$calc_trade_stream
        #' @description Calculates the trades for reaction boundaries attached
        #'   to stream cells
        #' @return Trades for reaction boundaries attached to stream cells
        calc_trade_stream = function(){

            # Error check: do the fraction of solute removed and remaining from STORAGE sum to one?
            if( round(sum(self$fractionRemovedStorage, self$fractionRemainingStorage), 3) != 1 ) {
              msgGeneral <- "The fraction of solute removed and remaining from storage do not sum to one."
              msgDetail <- paste(
                msgGeneral,
                "\nBoundary:", self$boundaryIdx,
                "\nFraction removed from storage:", self$fractionRemovedStorage,
                "\nFraction remaining in storage:", self$fractionRemainingStorage
              )
              warning(
                noquote( strsplit (msgDetail, "\n") [[1]])
              )
            }

            # Calculate fraction removed and remaining from the cell
            self$fractionRemaining <- exp(-1 * self$qStorage * self$fractionRemovedStorage * self$timeInterval / self$upstreamCell$channelDepth)
            self$fractionRemoved <- 1 - self$fractionRemaining

            # Error check: do the fraction of solute removed and remaining from the CELL sum to one?
            self$mustBeOne <- round(sum(self$fractionRemoved, self$fractionRemaining), 3)
            if( self$mustBeOne != 1 ) {
              msgGeneral <- "The fraction of solute removed and remaining in the cell do not sum to one."
              msgDetail <- paste(
                msgGeneral,
                "\nBoundary:", self$boundaryIdx,
                "\nFraction removed from storage:", self$fractionRemovedStorage,
                "\nFraction remaining in storage:", self$fractionRemainingStorage
              )
              tmp <- data.frame(
                fracRemnStrg = self$fractionRemainingStorage,
                fracRemovStrg = self$fractionRemovedStorage,
                fracRemov = self$fractionRemoved,
                fracRmn = self$fractionRemaining)
              warning(
                noquote( strsplit (msgDetail, "\n") [[1]]),
                print( tmp )
              )
            }

            self$startingAmount <- self$upstreamCell$amount

            self$amountToRemove <- self$startingAmount * self$fractionRemoved

            self$rxnVals <-
              data.frame(
                boundary = self$boundaryIdx,
                removalMethod = self$removalMethod,
                fracRemoved = self$fractionRemoved,
                fracRemaning = self$fractionRemaining,
                fracRemovedFromStrg = self$fractionRemovedStorage,
                fracRemainingInStrg = self$fractionRemainingStorage,
                mustBeOne = self$mustBeOne,
                startingAmount = self$startingAmount,
                amountToRemove = self$amountToRemove
              )

            return()
          }
      )
  )











