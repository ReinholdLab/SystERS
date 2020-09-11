#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCell  the upstream cell
#' @param downstreamCell the downstream cell
#' @param timeInterval  the model time step
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
        calculateOrder = NULL,

        timeInterval = NULL,

        upstreamCell = NULL,
        downstreamCell = NULL,

        usModBound = NULL,
        dsModBound = NULL,

        initialize =
          function(
            boundaryIdx,
            currency,
            boundarySuperClass,
            calculateOrder,

            upstreamCell,
            downstreamCell
            ){

            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass
            self$calculateOrder <- calculateOrder

            self$timeInterval <- timeInterval

            self$upstreamCell <- upstreamCell
            self$downstreamCell <- downstreamCell

            # Is this boundary a model boundary? Check to see if it has either
            # no u/s or no d/s cell...
            self$usModBound <- !(is.environment(self$upstreamCell)) # has no u/s cell
            self$dsModBound <- !(is.environment(self$downstreamCell)) # has no d/s cell

          }

      )
  )

#' @title Boundary_Transport_Water_Stream
#'
#' @description Transport boundary for water between stream cells
#'
#' @param discharge mean stream discharge
#' @param channelVelocity mean water velocity
#' @param channelResidenceTime mean residence time of the surface water
#' @param hydraulicLoad hydraulic load of the cell
#'
#' @export
#'
Boundary_Transport_Water_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Water_Stream",
    inherit = Boundary,
    public =
      list(
        discharge = NULL,
        channelVelocity = NULL,
        channelResidenceTime = NULL,
        hydraulicLoad = NULL,
        initialize =
          function(
            ...,
            discharge,
            channelVelocity,
            channelResidenceTime,
            hydraulicLoad,
          ){
            super$initialize(...)

            self$discharge <- as.numeric(discharge) # as.numeric is b/c reading from sparse table
          } # close initialize
      ) # close public
  ) # close R6 class


#' @method populateDependencies
#'
#' @description Populate boundary dependencies
#'
#' @param boundary The boundary to update
#'
#' @export
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "populateDependencies",
  value = function(){
    # To get velocity, divide Q by the mean of x-sec area of u/s and d/s
    # cell.  Upstream model boundaries (ie, most upstream) will thus
    # have a velocity equal only to the d/s cell (because there is no
    # u/s cell).  The opposite is true for the most d/s boundaries.
    # Likewise, to get residence time, divide by the mean of u/s and d/s
    # channel lengths.  Upstream model boundaries will thus have the
    # channel length defined by the d/s cell because no u/s cell exists
    # and vice versa for d/s model boundaries. Same pattern applies to
    # hydraulic load...
    if(!any(c(self$usModBound, self$dsModBound))) {
      depth <- mean(c(self$upstreamCell$channelDepth, self$downstreamCell$channelDepth))
      widthXdepth <- mean(c(self$upstreamCell$channelWidth * self$upstreamCell$channelDepth, self$downstreamCell$channelWidth * self$downstreamCell$channelDepth))
      len <- mean(c(self$upstreamCell$channelLength, self$downstreamCell$channelLength))
    }else{
      if(self$usModBound) {
        depth <- self$downstreamCell$channelDepth
        widthXdepth <- self$downstreamCell$channelWidth * depth
        len <-self$downstreamCell$channelLength
      }
      if(self$dsModBound){
        depth <- self$upstreamCell$channelDepth
        widthXdepth <- self$upstreamCell$channelWidth * depth
        len <-self$upstreamCell$channelLength
      }
    }
    self$channelVelocity <- self$discharge / widthXdepth
    self$channelResidenceTime <- len / self$channelVelocity
    self$hydraulicLoad <- depth / self$channelResidenceTime

    return()
  }
)

#' @method trade_static
#'
#' @description Calculates the trades between the water cells using static
#'   values for the boundary conditions; these values are provided by the user.
#'
#' @param boundary The boundary for which the trade is calculated
#'
#' @export
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "trade_static",
  value =
    function(){

      # volume of water to trade
      volume <- self$discharge * timeInterval #L

      if(!self$usModBound){
        # volume of water to remain
        volumeToRemain <- self$upstreamCell$channelVolume_L - volume
        if(volumeToRemain < 0) warning(
          paste(
            "You are about to remove more water volume from a cell than it held at the start of the timestep.
             Boundary is", self$boundaryIdx,
            "Cell is", self$upstreamCell$upstreamCellIdx
          ) # close paste
        ) # close warning
      } # close if statement

      return(list(discharge = self$discharge, volume = volume))
    } # close function def
)



#' @title Boundary_Transport_Solute_Stream
#'
#' @description Transport boundary for solutes between stream cells
#'
#' @param linkedBound the boundary containing the water that is advecting the solute in
#'   this bouddary
#' @param load the load (amount per time) of solute in this boundary
#' @param amount the amount of solute (mass or mols) in this boundary
#'
#' @export
#'
Boundary_Transport_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute_Stream",
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
Boundary_Transport_Solute_Stream$set(
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

#' @title Boundary_Reaction_Solute_Stream
#'
#' @description Reaction boundary for solutes in streams.
#'
#' @export
#'
Boundary_Reaction_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream",
    inherit = Boundary_Reaction_Solute_Stream,
    public =
      list(
        fractionRemoved = NULL,
        fractionRemaining = NULL,
        fractionRemovedStorage = NULL,
        fractionRemainingStorage = NULL,
        mustBeOne = NULL,
        startingAmount = NULL,
        amountToRemove = NULL,
        amountToRemain = NULL,
        rxnVals = NULL,
        initialize =
          function(...){
            super$initialize(...)
          }
      )
  )


#' @title calc_trade_static
#'
#' @description Calculates the trades for stream cells.
#'
#' @export
#'
Boundary_Reaction_Solute_Stream$set(
  which = "public",
  name = "calc_trade_static",
  value = function(){

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
    self$mustBeOne <- self$fractionRemoved + self$fractionRemaining
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


#' @title Boundary_Reaction_Solute_Stream_Simple
#'
#' @description Reaction boundary for solutes in stream cells wherein the trade
#'   method simply removes a fraction of solute from storage based on a user
#'   defined percent (of solute to remove from storage).
#'
#' @param pcntToRemove The percent of solute to remove from storage
#'
#' @export
#'
Boundary_Reaction_Solute_Stream_Simple <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream_Simple",
    inherit = Boundary_Reaction_Solute_Stream,
    public =
      list(
        pcntToRemove = NULL,
         initialize =
          function(..., pcntToRemove){
            super$initialize(...)

            self$pcntToRemove <- pcntToRemove

          }
      )
  )

#' @title trade_static
#'
#' @description Calculates the fraction of a solute removed from a cell based on
#'   a user-defined percent of solute to remove from storage.
#'
#' @export
#'
Boundary_Reaction_Solute_Stream_Simple$set(
  which = "public",
  name = "trade_static",
  value = function(){

      self$fractionRemovedStorage <- self$fracRemovStrg()
      self$fractionRemainingStorage <- 1 - self$fractionRemovedStorage

      self$calc_trade_static()

  }
)

#' @title fracRemovStrg
#' @description Calculates the fraction of a solute removed from storage.
#'
Boundary_Reaction_Solute_Stream_Simple$set(
  which = "public",
  name = "fracRemovStrg",
  value = function(){
    return(self$pcntToRemove / 100)
  }
)


#' @title Boundary_Reaction_Solute_Stream_ResTimeWtdPowerLaw
#'
#' @description Reaction boundary for solutes in stream cells wherein the trade
#'   method simply removes a fraction of solute from storage based on a user
#'   defined percent (of solute to remove from storage).
#'
#' @param tauMin Minimum residence time to consider
#' @param tauMax Maximum residence time to consider
#' @param alpha Power law exponent describing the shape of the curve, typically between -1.2 and -1.9
#' @param k Uptake constant for solute in units of T-1
#'
#' @export
#'
Boundary_Reaction_Solute_Stream_ResTimeWtdPowerLaw <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream_ResTimeWtdPowerLaw",
    inherit = Boundary_Reaction_Solute_Stream,
    public =
      list(
        tauMin = NULL,
        tauMax = NULL,
        alpha = NULL,
        k = NULL,
        initialize =
          function(..., tauMin, tauMax, alpha, k){
            super$initialize(...)

            self$tauMin <- tauMin
            self$tauMax <- tauMax
            self$alpha <- alpha
            self$k <- k

          }
      )
  )

#' @title trade_static
#'
#' @description Calculates the fraction of a solute removed from a cell based on
#'   a suite of user defined parameters describing the uptake constant of the
#'   solute and the power law residence time distribution.
#'
Boundary_Reaction_Solute_Stream_ResTimeWtdPowerLaw$set(
  which = "public",
  name = "trade_static",
  value = function(){

    self$fractionRemovedStorage <- self$fracRemovStrg(remaining = FALSE)
    self$fractionRemainingStorage <- self$fracRemovStrg(remaining = TRUE)

    self$calc_trade_static()

  }
)


#' @title fracRemovStrg
#'
#' @description Calculates the fraction of a solute removed (and remaining) from
#'   a storage zone using the residence-time weighted fractional removal
#'   approach. If remaining = TRUE, then the function calculates the fraction of
#'   the solute REMAINING; however if remaining = FALSE, then the function
#'   calculates the fraction of solute REMOVED
Boundary_Reaction_Solute_Stream_ResTimeWtdPowerLaw$set(
  which = "public",
  name = "fracRemovStrg",
  value = function(remaining){

    propUptkFunc <-
      function(
        tau,
        tauMin ,
        tauMax ,
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

  }
)
