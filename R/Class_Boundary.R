#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCellIdx  the name of the upstream cell as a character.
#' @param downstreamCellIdx the name of the downstream cell as a character.
#'
#' @export
#'
Boundary <-
  R6::R6Class(
    classname = "Boundary",
    public =
      list(
        boundaryIdx = NULL,
        currency = NULL,
        boundarySuperClass = NULL,
        upstreamCellIdx = NULL,
        downstreamCellIdx = NULL,
        calculateOrder = NULL,
        initialize =
          function(boundaryIdx, currency, boundarySuperClass, upstreamCellIdx, downstreamCellIdx, calculateOrder){
            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass
            self$upstreamCellIdx <- upstreamCellIdx
            self$downstreamCellIdx <- downstreamCellIdx
            self$calculateOrder <- calculateOrder
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
      upstreamCellIdx
    ){
      return(upstreamCell$discharge)
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
#'
#' @return the load to pass onto the downstream cell
#'
Boundary$set(
  which = "public",
  name = "soluteTransportPerTime",
  value =
    function(
      boundaryIdx,
      upstreamCell
    ){
      return(
        upstreamCell$discharge * upstreamCell$soluteConcentration
      )
    }
)

########

#' @title CalcFractionalSoluteDynams
#'
#' @description Calculate the fraction of solute removed and remaining.
#'
#' @param boundaryIdx the name of the boundary.
#' @param removalMethod the method by which solute is removed from the cell.
#'   Currently, only \code{RT-PL} and \code{pcnt} is supported.
#'
#' @return the fraction of solute removed and remaining in the cell by the
#'   boundary.
#'
Boundary$set(
  which = "public",
  name = "fractionalSoluteDynams",
  value =
    function(
      boundaryIdx,
      upstreamCellIdx,
      removalMethod,
      ...
    ){

      if( is.null(removalMethod) ) {

        stop(
          paste0(
            "The removalMethod is currently NULL for boundaryIdx = ",
            boundaryIdx, ", .  A removalMethod must be provided.")
          )

      } else if(removalMethod == "RT-PL") {

        self$fractionRemoved <- ResTmWtdFracRemovStrg$new(...)$fractionRemoved
        self$fractionRemaining <- ResTmWtdFracRemovStrg$new(...)$fractionRemaining

        stop("I need to write the code to do this.")

      } else if(removalMethod == "pcnt") {

        self$fractionRemoved <- PcntRemovStrg$new(...)$fractionRemoved
        self$fractionRemaining <- PcntRemovStrg$new(...)$fractionRemaining

      }
      return()
    }
)

#' @title PcntRemovStrg
#'
#' @description Calculates the fraction of a solute removed and remaining in the
#'   storage zone using a very simple approach of removing a certain percentage
#'   of the initial solute.
#'
#' @param pcntToRemove is the percent (0-100) of the solute to remove from the storage zone
#'
PcntRemovStrg <-
  R6::R6Class(
    classname = "PcntRemovStrg",
    public = list(
      pcntToRemove = NULL,
      initialize = function(pcntToRemove){

        self$fractionRemoved <- pcntToRemove/100
        self$fractionRemaining <- 1 - self$fractionRemoved

      }
    )
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

