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
        #' @field tauMin Smallest residence time when solute processing is
        #'   possible; must be >= tauMinWater
        tauMin = NULL,
        #' @field tauMax Largest residence time when solute processing is
        #'   possible; must be <= tauMaxWater
        tauMax = NULL,
        #' @field tauMinWater Minimum residence time of water to consider; lower
        #'   bound of power law describing the residence time distribution of
        #'   water in the hyporheic zone
        tauMinWater = NULL,
        #' @field tauMaxWater Maximum residence time of water to consider; upper
        #'   bound of power law describing the residence time distribution of
        #'   water in the hyporheic zone
        tauMaxWater = NULL,
        #' @field alpha Power law exponent describing the shape of the curve,
        #'   typically between -1.2 and -1.9
        alpha = NULL,
        #' @field k Uptake constant for solute in units of T-1
        k = NULL,
        #' @field tauRxn The minimum residence time when reaction can occur,
        #'   i.e., a value that specifies how long solute must be in the
        #'   reactive storage zone before it has the capacity to react.
        tauRxn = NULL,
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
        #' @param tauRxn A lag value that specifies how long solute must be in
        #'   the reactive storage zone before it has the capacity to react,
        #'   i.e., this is the minimum residence time when reaction can occur.
        #'   This parameter is only passed to the \code{RT-PL} Method and cannot
        #'   be applied to the \code{pcnt} Method.  The default value is
        #'   \code{0}.
        #' @param qStorage Volumetric rate of water entering the storage zone
        #' @param pcntToRemove Percent of solute amount (mass or mols) to remove
        #'   from storage
        #' @return The object of class \code{Boundary_Reaction_Solute}.
        initialize =
          function(..., processMethodName, tauMin, tauMax, tauMinWater, tauMaxWater, alpha, k, tauRxn, qStorage, pcntToRemove, volWaterInStorage){
            super$initialize(...)
            self$processDomain <- self$upstreamCell$processDomain
            self$processMethodName <- processMethodName
            if(processMethodName == "RT-PL"){
              self$tauMin <- tauMin
              self$tauMax <- tauMax
              self$alpha <- alpha
              self$tauMinWater <- tauMinWater
              self$tauMaxWater <- tauMaxWater
              self$tauRxn <- tauRxn
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

            # list containing a series of vectors with the lower and upper limits of the integration
            integTauBins <-
              list(
                bin1 =
                  c(
                    lowerB = self$tauMin,
                    upperB = self$tauMin + ((self$tauMax - self$tauMin) / 1E10) ),
                bin2 =
                  c(
                    lowerB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E10),
                    upperB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E8)
                  ),
                bin3 =
                  c(
                    lowerB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E8),
                    upperB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E5)
                  ),
                bin4 =
                  c(
                    lowerB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E5),
                    upperB = self$tauMin + ((self$tauMax - self$tauMin) / 1E2)
                  ),
                bin5 =
                  c(
                    lowerB =  self$tauMin + ((self$tauMax - self$tauMin) / 1E2),
                    upperB = self$tauMax
                  )
              )


          # specify the function that does the integration
            propUptkFunc <-
              function(
                tau,
                tauMin,
                tauMax,
                alpha,
                k,
                tauRxn,
                remaining
              ){
                PL_PDF <- hydrogeom::powerLawPDF(tau, tauMin, tauMax, alpha)

                d_tau_proc <- pmax(0, tau - tauRxn)

                #if remaining, this is the solute fraction
                soluteFraction <- exp(-k * d_tau_proc)

                #if calculating removal, this is the solute fraction
                if(!remaining){
                  soluteFraction <- 1 - soluteFraction
                }
                return(PL_PDF * soluteFraction)
              }

            # do the integration for each bin set
            propUptkIntList <-
              plyr::llply(
                integTauBins,
                function(binSet){
                  integrate(
                    propUptkFunc,
                    lower = binSet[1],
                    upper = binSet[2],
                    tauMin = self$tauMin,
                    tauMax = self$tauMax,
                    alpha = self$alpha,
                    k = self$k,
                    tauRxn = self$tauRxn,
                    remaining = remaining,
                    abs.tol = 0,
                    subdivisions = 10000
                  )$value
                }
              )

            # sum the results
            propUptk <- do.call(sum, propUptkIntList)

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
        #' @field damkohlerNum Damkohler number for the boundary.  This
        #'   Damkohler represents the change over the entire stream reach, such
        #'   that the same answer would be returned if timestep was set to be
        #'   the mean residence time of the surface water in the reach.  Recall
        #'   that hydraulic load is equal to channel depth over the mean
        #'   residence time of water in the channel and it follows that this
        #'   Damkohler is equal to uptake velocity over hydraulic load,
        #'   \code{v_f / HL}.
        damkohlerNum = NULL,
        #' @field damkohlerNumStorage Damkohler number for the transient storage
        #'   zone (i.e. hyporheic zone)
        damkohlerNumStorage = NULL,
        #' @field damkohlerNumScaledToTimeStep A Damkohler for the reach, scaled
        #'   to the timestep
        damkohlerNumScaledToTimeStep = NULL,
        #' @field concentrationStorage Concentration of solute in storage
        concentrationStorage = NULL,
        #'
        #' @param ... Parameters inherit from Class
        #'   \code{\link{Boundary_Reaction_Solute}} and thus
        #'   \code{\link{Boundary}}
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as
        #'   a character e.g., \code{water, NO3}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that calculates solute removal from stream
        #'   cells in the stream processing domain
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

          # upstreamWaterBounds <- self$upstreamCell$linkedCell$linkedBoundsList$upstreamBounds
          # hydraulicLoad <- self$upstreamCell$linkedCell$hydraulicLoad

          channelDepth <- self$upstreamCell$linkedCell$channelDepth

          k_s <- self$qStorage * self$fractionRemovedStorage / channelDepth

          self$fractionRemaining <- exp(-1 * k_s  * self$timeInterval)

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


          hydraulicLoad <- self$upstreamCell$linkedCell$hydraulicLoad
          steadyStateFracRemaining <- exp(-1 * self$qStorage * self$fractionRemovedStorage  / hydraulicLoad)
          steadyStateFracRemoved <- 1 - steadyStateFracRemaining

          self$damkohlerNum <- -1*(log(1-steadyStateFracRemoved))
          self$damkohlerNumScaledToTimeStep <- -1*(log(1-self$fractionRemoved))
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

