#' @title WaterTransportPerTime
#'
#' @description Calculate the discharge of water (Volume/Time).
#'
#' @param boundaryIdx the name of the boundary.
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param additionalWaterInputOrLoss the discharge (Volume/Time) of water to be
#'   added, if any.  Units must match those of @param discharge, etc.  Default
#'   value is \code{0}.
#'
#' @return the discharge of water to pass through the boundary.
#'
WaterTransportPerTime <-
  R6::R6Class(
    classname = "WaterTransportPerTime",
    public =
      list(
        boundary = NULL,
        dischargeToTrade = NULL,
        initialize =
          function(boundary){

            # discharge in the cell
            self$dischargeToTrade <- boundary$upstreamCell$discharge

          }
      )
  )

#' @title SoluteTransportPerTime
#'
#' @description Calculate load, i.e., the rate of solute mass moving from the
#'   upstream cell into the boundary (Mass/Time).
#'
#' @param boundary the boundary
#'
#' @return the load to pass onto the downstream cell
#'
SoluteTransportPerTime <-
  R6::R6Class(
    classname = "SoluteTransportPerTime",
    public =
      list(
        boundary = NULL,
        soluteToTrade = NULL,
        initialize = function(boundary){

          # get the discharge and solute concentration in the upstream cells
          upstreamCellDischarge <- boundary$upstreamCell$discharge
          upstreamCellConcentration <- boundary$upstreamCell$soluteConcentration

          # multiply discharge by concentration to get load
          self$soluteToTrade <- upstreamCellDischarge * upstreamCellConcentration

        }
      )
  )

########

#' @title CalcFractionalSoluteDynams
#'
#' @description Calculate the mass of solute removed and remaining.
#'
#' @param boundaryIdx the name of the boundary.
#' @param removalMethod the method by which solute is removed from the cell.
#'   Currently, only \code{RT-PL} and \code{pcnt} is supported.
#'
#' @return the mass of solute removed and remaining in the cell by the
#'   boundary.
#'
CalcFractionalSoluteDynams <-
  R6::R6Class(
    classname = "CalcFractionalSoluteDynams",
    public =
      list(
        boundary = NULL,
        removalMethod = NULL,
        fractionRemoved = NULL,
        fractionRemaining = NULL,
        fractionRemovedStorage = NULL,
        fractionRemainingStorage = NULL,
        mustBeOne = NULL,
        startingMass = NULL,
        massToRemove = NULL,
        massToRemain = NULL,
        rxnVals = NULL,
        initialize =
          function(
            boundary,
            removalMethod,
            ...
          ){

            if( is.null(removalMethod) ) {

              stop(
                paste0(
                  "The removalMethod is currently NULL for boundaryIdx = ",
                  boundary$boundaryIdx, ", .  A removalMethod must be provided.")
              )

            } else if(removalMethod == "RT-PL") {

              alpha <- boundary$upstreamCell$alpha
              k <- boundary$upstreamCell$k
              tauMin <- boundary$upstreamCell$tauMin
              tauMax <- boundary$upstreamCell$tauMax

              self$fractionRemovedStorage <- ResTmWtdFracRemovStrg$new(alpha = boundary$upstreamCell$alpha, k = boundary$upstreamCell$k, tauMin = boundary$upstreamCell$tauMin, tauMax = boundary$upstreamCell$tauMax)$fractionRemoved
              self$fractionRemainingStorage <- ResTmWtdFracRemovStrg$new(alpha = boundary$upstreamCell$alpha, k = boundary$upstreamCell$k, tauMin = boundary$upstreamCell$tauMin, tauMax = boundary$upstreamCell$tauMax)$fractionRemaining

            } else if(removalMethod == "pcnt") {

              self$fractionRemovedStorage <- self$fracRemovSimple(boundary)
              self$fractionRemainingStorage <- 1 - self$fractionRemovedStorage
            }

            if( sum(self$fractionRemovedStorage, self$fractionRemainingStorage) != 1 ) {
              msgGeneral <- "The fraction of solute removed and remaining from storage do not sum to one."
              msgDetail <- paste(
                msgGeneral,
                "\nBoundary:", boundary$boundaryIdx,
                "\nFraction removed from storage:", self$fractionRemovedStorage,
                "\nFraction remaining in storage:", self$fractionRemainingStorage
              )
              stop(
                noquote( strsplit (msgDetail, "\n") [[1]])
              )
            }

            self$fractionRemaining <- exp(-boundary$upstreamCell$qStorage * self$fractionRemovedStorage / boundary$upstreamCell$hydraulicLoad)
            self$fractionRemoved <- 1 - self$fractionRemaining

            self$mustBeOne <- self$fractionRemoved + self$fractionRemaining

            if( self$mustBeOne != 1 ) {
              msgGeneral <- "The fraction of solute removed and remaining do not sum to one."
              msgDetail <- paste(
                msgGeneral,
                "\nBoundary:", boundary$boundaryIdx,
                "\nFraction removed from storage:", self$fractionRemovedStorage,
                "\nFraction remaining in storage:", self$fractionRemainingStorage
              )
              tmp <- data.frame(
                fracRemnStrg = self$fractionRemainingStorage,
                fracRemovStrg = self$fractionRemovedStorage,
                fracRemov = self$fractionRemoved,
                fracRmn = self$fractionRemaining)
              stop(
                noquote( strsplit (msgDetail, "\n") [[1]]),
                print( tmp )
              )
            }


            # Am I doing this right??? Do I need to incorporate the water in the
            # HZ?... or is that accounted for by the steady state assumption of
            # water entering storage equaling water leaving storage...
            #
            # concentration X channelVolume = amountSolute (mass or mols)
            # fractionRemaining = exp(-vf/HL)
            # fractionRemoved = 1 - fractionRemaining (duh)
            # amountSolute X exp(-vf/HL) = mass remaining

            # have to multiply by 1000 because units of concentration are ug/L
            # but cell volume is m3 --- will need to decide how we want to
            # handle this in time as hardcoding it this way is clearly a bad
            # idea...

            self$startingMass <- boundary$upstreamCell$soluteConcentration * 1000 * boundary$upstreamCell$channelArea * boundary$upstreamCell$channelDepth

            self$massToRemove <- self$startingMass * self$fractionRemoved
            self$massToRemain <- self$startingMass * self$fractionRemaining

            self$rxnVals <-
              c(
                boundary = boundary$boundaryIdx,
                removalMethod = self$removalMethod ,
                fracRemoved = self$fractionRemoved ,
                fracRemaning = self$fractionRemaining ,
                fracRemovedFromStrg = self$fractionRemovedStorage ,
                fracRemainingInStrg = self$fractionRemainingStorage ,
                mustBeOne = self$mustBeOne ,
                startingMass = self$startingMass,
                massToRemove = self$massToRemove ,
                massToRemain = self$massToRemain
              )

          }
      )
  )

#' @title fracRemovSimple
#'
#' @description Calculates the fraction of a solute removed e using a very
#'   simple approach of removing a certain percentage of the initial solute.
#'
#' @param pcntToRemove is the percent (0-100) of the solute to remove from the
#'   cell
#'
CalcFractionalSoluteDynams$set(
  which = "public",
  name = "fracRemovSimple",
  value = function(boundary){
    return(boundary$upstreamCell$pcntToRemove / 100)
  }
)

#' @title ResTmWtdFracRemovStrg
#'
#' @description Calculates the fraction of a solute removed and
#'   remaining in the storage zone using the residence-time weighted fractional
#'   removal approach.

ResTmWtdFracRemovStrg <-
  R6::R6Class(
    classname = "ResTmWtdFracRemovStrg",
    public = list(
      # Declare attributes and methods
      alpha = NULL,
      k = NULL,
      tauMin = NULL,
      tauMax = NULL,
      freqMin = NULL,
      totalFreqIntegrate = NULL,
      totalFreq = NULL,
      shouldBeOne = NULL,
      # Define the function called by the constructor $new
      initialize = function(alpha, k, tauMin, tauMax, freqMin = tauMax^-alpha) {

        # Populate attributes
        self$alpha <- alpha;
        self$k <- k;
        self$tauMin <- tauMin;
        self$tauMax <- tauMax;
        self$freqMin <- freqMin;

        # Calculate the total area under the frequency distribution
        self$totalFreqIntegrate <- integrate(
          f = self$frequency,
          lower = tauMin,
          upper = tauMax
        );
        self$totalFreq <- self$totalFreqIntegrate$value;

        # Set the quality check on the numerical integrations
        self$shouldBeOne <-
          self$integrateFractionRemoved(tauMin = tauMin, tauMax = tauMax)$value +
          self$integrateFractionRemaining(tauMin = tauMin, tauMax = tauMax)$value;
      }
    )
  );

#' @method ResTmWtdFracRemovStrg$frequency
#'
#' @description Calculates the frequency for a given residence time
#'
ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "frequency",
  value = function(tau) {
    return( tau^-self$alpha - self$freqMin );
  }
);

#' @method ResTmWtdFracRemovStrg$density
#'
#' @description Calculates the density for a given residence time
#'
ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "density",
  value = function(tau) {
    # The normalizing constant, K, is equal to 1/self$totalFreq with units of [1/T]
    # Multiplying f(tau), i.e., self$frequency(tau) by K is the PDF with units of [1/T]
    return( self$frequency(tau) / self$totalFreq );
  }
);

#' @method ResTmWtdFracRemovStrg$fractionRemoved
#'
#' @description Calculates the fraction removed per time for a given residence time
#'
ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "fractionRemoved",
  value = function(tau) {
    return ( self$density(tau) * (1 - exp(-self$k * tau)) );
  }
);

#' @method ResTmWtdFracRemovStrg$integrateFractionRemoved
#'
#' @description  Integrates the fraction removed for a range of residence times
#'

ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "integrateFractionRemoved",
  value = function(lower = self$tauMin, upper = self$tauMax) {
    return ( integrate(f = self$fractionRemoved, lower = lower, upper = upper) );
  }
);

#' @method ResTmWtdFracRemovStrg$fractionRemaining
#'
#' @description Calculates the fraction remaining per time for a given residence time
#'
ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "fractionRemaining",
  value = function(tau) {
    return ( self$density(tau) * exp(-self$k * tau) );
  }
);

#' @method ResTmWtdFracRemovStrg$integrateFractionRemaining
#'
#' @description Integrates the fraction remaining for a range of residence times
#'
ResTmWtdFracRemovStrg$set(
  which = "public",
  name = "integrateFractionRemaining",
  value = function(lower = self$tauMin, upper = self$tauMax) {
    return ( integrate(f = self$fractionRemaining, lower = lower, upper = upper) );
  }
)

