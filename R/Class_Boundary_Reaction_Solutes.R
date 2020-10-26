#' @title Class Boundary_Reaction_Solute (R6)
#' Reaction boundary for a solute
#' @description Reaction boundary for a solute
#' @export

Boundary_Reaction_Solute <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute",

    #' @inherit Boundary
    inherit = Boundary,

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
        #' @field processMethodName How to process the solute, either
        #'   \code{\link{Boundary_Reaction_Solute$processMethod_RT_PL}} or
        #'   \code{\link{Boundary_Reaction_Solute$processMethod_pcnt}}
        processMethodName = NULL,
        #' @field processMethod Call to the Method  \code{pcnt} or \code{RT-PL}
        processMethod = NULL,
        #' @field tauMin Minimum residence time to consider
        tauMin = NULL,
        #' @field tauMax Maximum residence time to consider
        tauMax = NULL,
        #' @field alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        alpha = NULL,
        #' @field k Uptake constant for solute in units of T-1
        k = NULL,
        #' @field qStorage Volumeteric rate of water entering the storage zone
        qStorage = NULL,
        #' @field rxnVals A data frame storing the key reaction boundary inputs
        #'   and outputs
        rxnVals = NULL,
        #' @field pcntToRemove If process method is set to \code{pcnt}, then the
        #'   percent of solute amount (mass or mols) to remove from storage must
        #'   be specified
        pcntToRemove = NULL,


        #' @description Instantiate a reaction boundary for a solute
        #' #' @param ... Parameters inherit from Class \code{\link{Boundary}}
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
        #' @param upstreamCell  Cell to which the reaction boundary is connected
        #' @param timeInterval  Model time step
        #' @param processMethodName How to process the solute, either \code{pcnt} or
        #'   \code{RT-PL}
        #' @param tauMin Minimum residence time to consider
        #' @param tauMax Maximum residence time to consider
        #' @param alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        #' @param k Uptake constant for solute in units of T-1
        #' @param qStorage Volumeteric rate of water entering the storage zone
        #' @param pcntToRemove Percent of solute amount (mass or mols) to remove
        #'   from storage
        #' @return The ojbect of class \code{Boundary_Reaction_Solute}.
        initialize =
          function(..., processMethodName, tauMin, tauMax, alpha, k, qStorage, pcntToRemove){
            super$initialize(...)
            self$processDomain <- self$upstreamCell$processDomain
            self$processMethodName <- processMethodName
            if(processMethodName == "RT-PL"){
              self$tauMin <- tauMin
              self$tauMax <- tauMax
              self$alpha <- alpha
              self$processMethod <- self$processMethod_RT_PL
            } else if(processMethodName == "pcnt"){
              self$pcntToRemove <- pcntToRemove
              self$processMethod <- self$processMethod_pcnt
            }
            self$k <- k
            self$qStorage <- qStorage
          },


        #' @method Method Boundary_Reaction_Solute$processMethod_RT_PL
        #' @description Calculates the fraction solute to remove from storage
        #'   based on a power law residence time weighted approach
        processMethod_RT_PL = function(){
          self$fractionRemovedStorage <- self$calc_fracRemoval_resTimeWtdPowerLaw(remaining = FALSE)
          self$fractionRemainingStorage <- self$calc_fracRemoval_resTimeWtdPowerLaw(remaining = TRUE)
        },


        #' @method Method Boundary_Reaction_Solute$processMethod_pcnt
        #' @description Calculates the fraction solute to remove from storage
        #'   based on  a user specified percent
        #' @return
        processMethod_pcnt = function(){
          self$fractionRemovedStorage <- self$pcntToRemove/100
          self$fractionRemainingStorage <- 1 - self$fractionRemovedStorage
        },


        #' @method Method
        #'   Boundary_Reaction_Solute$calc_fracRemoval_resTimeWtdPowerLaw
        #' @description Calculate fraction of solute removed/remainig from a
        #'   fully saturated storage zone using a residence-time weighted
        #'   removal function assuming a power law residence time.  If remaining
        #'   = TRUE, then the function calculates the fraction of the solute
        #'   REMAINING; however if remaining = FALSE, then the function
        #'   calculates the fraction of solute REMOVED
        #' @returns Fraction of solute removed or remaining in the storage zone
        calc_fracRemoval_resTimeWtdPowerLaw = function(remaining = remaining){

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
              # in the following line, if the -1  is removed (and this
              # simply expressed as -k*tau), an "invalid argument to unary
              # operator" error is thrown
              minus_k_t <- (-1*k*tau)

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
        }
      )
  )


#' @title Class Boundary_Reaction_Solute_Stream (R6)
#' A model boundary that calculates solute removal from stream cells
#' @description Reaction boundary for solute from stream cells
#' @export
Boundary_Reaction_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream",

    #' @inherit Boundary_Reaction_Solute
    inherit = Boundary_Reaction_Solute,

    public =
      list(
        #' @description Instantiate a solute reaction boundary in the stream processing domain
        #' @param ... Parameters inherit from Class \code{\link{Boundary_Reaction_Solute}} and thus \code{\link{Boundary}}
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that calculates solute removal from stream cells in the stream processing domain
        initialize =
          function(...){
            super$initialize(...)
          }, # close initialize


        #' @method Method Boundary_Reaction_Solute_Stream$trade
        #' @description Calculates the trades for reaction boundaries attached
        #'   to stream cells
        #' @return Trades for reaction boundaries attached to stream cells
        trade = function(){

          self$calc_removal_pcnt()

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

