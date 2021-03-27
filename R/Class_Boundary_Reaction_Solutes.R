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
        #' @field tauMin Smallest residence time when solute processing is possible; must be >= tauMinWater
        tauMin = NULL,
        #' @field tauMax Largest residence time when solute processing is possible; must be <= tauMaxWater
        tauMax = NULL,
        #' @field tauMinWater Minimum residence time of water to consider; lower bound of power law describing the residence time
        #'   distribution of water in the hyporheic zone
        tauMinWater = NULL,
        #' @field tauMaxWater Maximum residence time of water to consider; upper bound of power law describing the residence time
        #'   distribution of water in the hyporheic zone
        tauMaxWater = NULL,
        #' @field alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        alpha = NULL,
        #' @field k Uptake constant for solute in units of T-1
        k = NULL,
        #' @field qStorage Volumetric rate of water entering the storage zone
        qStorage = NULL,
        #' @field volWaterInStorage Volume of water in the transient storage
        #'   zone (equal to the volume of the alluvial aquifer X porosity)
        volWaterInStorage = NULL,
        #' @field rxnVals A data frame storing the key reaction boundary inputs
        #'   and outputs
        rxnVals = NULL,
        #' @field pcntToRemove If process method is set to \code{pcnt}, then the
        #'   percent of solute amount (mass or mols) to remove from storage must
        #'   be specified
        pcntToRemove = NULL,

        #' @description Instantiate a reaction boundary for a solute #' @param
        #'   ... Parameters inherit from Class \code{\link{Boundary}}
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
        #' @param upstreamCell  Cell to which the reaction boundary is connected
        #' @param timeInterval  Model time step
        #' @param processMethodName How to process the solute, either
        #'   \code{pcnt} or \code{RT-PL}
        #' @param tauMin Minimum residence time of solute to consider (residence
        #'   time at which solute reactions begin having the potential to
        #'   occur);must be >= than \code{tauMinWater}
        #' @param tauMax Maximum residence time of solute to consider (residence
        #'   time at which solute reactions stop having the potential to occur);
        #'   must be <= \code{tauMaxWater}
        #' @param tauMinWater Minimum residence time of water to consider; bound
        #'   on lower end of power law describing the residence time
        #'   distribution
        #' @param tauMaxWater Maximum residence time of water to consider; bound
        #'   on upper end of power law describing the residence time
        #'   distribution
        #' @param alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        #' @param k Uptake constant for solute in units of T-1
        #' @param qStorage Volumetric rate of water entering the storage zone
        #' @param pcntToRemove Percent of solute amount (mass or mols) to remove
        #'   from storage
        #' @return The object of class \code{Boundary_Reaction_Solute}.
        initialize =
          function(..., processMethodName, tauMin, tauMax, tauMinWater, tauMaxWater, alpha, k, qStorage, pcntToRemove, volWaterInStorage){
            super$initialize(...)
            self$processDomain <- self$upstreamCell$processDomain
            self$processMethodName <- processMethodName
            if(processMethodName == "RT-PL"){
              self$tauMin <- tauMin
              self$tauMax <- tauMax
              self$alpha <- alpha
              self$tauMinWater <- tauMinWater
              self$tauMaxWater <- tauMaxWater
              self$processMethod <- self$processMethod_RT_PL
            } else if(processMethodName == "pcnt"){
              self$pcntToRemove <- pcntToRemove
              self$processMethod <- self$processMethod_pcnt
            }
            self$k <- k
            self$volWaterInStorage <- volWaterInStorage

            if(!is.numeric(qStorage)){
              # I have set tau_a = tauMinWater and tau_b = tauMaxWater because we want the
              # entire integral from tauMin to tauMax...
              ccdfIntegrated <- hydrogeom::powerLawIntCCDF(self$tauMinWater, self$tauMaxWater, self$tauMinWater, self$tauMaxWater, self$alpha)
              storage.1d <- self$volWaterInStorage / self$upstreamCell$linkedCell$channelArea
              self$qStorage <-  storage.1d / ccdfIntegrated
            } else {
              self$qStorage <- qStorage
            }
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
          if(is.na(self$tauMin)){
            if(remaining) {
              propUptk <- 1
            } else {
              propUptk <- 0
            }
            return(propUptk)
          } else {

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

            propUptk_part1 <-
              integrate(
                propUptkFunc,
                lower = self$tauMin,
                upper = (self$tauMax - self$tauMin)/100000,
                tauMin = self$tauMin,
                tauMax = self$tauMax,
                alpha = self$alpha,
                k = self$k,
                remaining = remaining,
                abs.tol = 0,
                subdivisions = 10000
              )$value

            propUptk_part2 <-
              integrate(
                propUptkFunc,
                lower = (self$tauMax - self$tauMin)/100000,
                upper = (self$tauMax - self$tauMin)/1000,
                tauMin = self$tauMin,
                tauMax = self$tauMax,
                alpha = self$alpha,
                k = self$k,
                remaining = remaining,
                abs.tol = 0,
                subdivisions = 10000
              )$value

            propUptk_part3 <-
              integrate(
                propUptkFunc,
                lower = (self$tauMax - self$tauMin)/1000,
                upper = self$tauMax,
                tauMin = self$tauMin,
                tauMax = self$tauMax,
                alpha = self$alpha,
                k = k,
                remaining = remaining,
                abs.tol = 0,
                subdivisions = 10000
              )$value

            propUptk <- propUptk_part1 + propUptk_part2 + propUptk_part3

            return(propUptk)
          }
        },


        #' @method Method Boundary_Reaction_Solute$store
        #' @description Runs the store method on solute cells in the model for
        #'   reactions that remove solute from cells.
        #' @return Updated store values.
        store = function(){
          self$upstreamCell$amount <- self$upstreamCell$amount - self$amountToRemove
          return(self$upstreamCell$amount)
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
        #' @description Instantiate a solute reaction boundary in the stream
        #'   processing domain
        #' @field fractionRemaining Fraction of solute removed from stream cell
        fractionRemoved = NULL,
        #' @field fractionRemaining Fraction of solute remaining in
        #'   stream cell
        fractionRemaining = NULL,
        #' @field damkohlerNum Damkohler number for the boundary
        damkohlerNum = NULL,
        #' @field damkohlerNumStorage Damkohler number for the transient storage
        #'   zone (i.e. hyporheic zone)
        damkohlerNumStorage = NULL,
        #' @field concentrationStorage Concentration of solute in storage
        concentrationStorage = NULL,
        #'
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

          self$processMethod()
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

          upstreamWaterBounds <- self$upstreamCell$linkedCell$linkedBoundsList$upstreamBounds
          hydraulicLoad <- self$upstreamCell$linkedCell$hydraulicLoad

          self$fractionRemaining <- exp(-1 * self$qStorage * self$fractionRemovedStorage  / hydraulicLoad)

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

          self$startingAmount <- self$upstreamCell$amount # mols or mass of solute in primary flow field at start of time step

          self$amountToRemove <- self$startingAmount * self$fractionRemoved
          self$amountToRemain <- self$startingAmount - self$amountToRemove

          self$damkohlerNum <- -1*(log(1-self$fractionRemoved))
          self$damkohlerNumStorage <- -1*(log(1-self$fractionRemovedStorage))

          self$rxnVals <-
            data.frame(
              boundary = self$boundaryIdx,
              processMethodName = self$processMethodName,
              fracRemoved = self$fractionRemoved,
              fracRemaning = self$fractionRemaining,
              fracRemovedFromStrg = self$fractionRemovedStorage,
              fracRemainingInStrg = self$fractionRemainingStorage,
              mustBeOne = self$mustBeOne,
              startingAmount = self$startingAmount,
              amountToRemove = self$amountToRemove
            )

          return(list(amountToRemove = self$amountToRemove, amountToRemain = self$amountToRemain))
        }
      )
  )

