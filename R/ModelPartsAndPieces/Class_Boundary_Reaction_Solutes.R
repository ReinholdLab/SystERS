

#' @title Class Boundary_Reaction_Solute (R6)
#' Reaction boundary for a solute
#' @description Reaction boundary for a solute
#' @export

Boundary_Reaction_Solute <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute",
    inherit = Boundary_Reaction_Solute,
    public =
      list(
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



#' @title Boundary_Reaction_Solute_Simple
#'
#' @description Reaction boundary for solutes in cells wherein the trade
#'   method simply removes a fraction of solute from storage based on a user
#'   defined percent (of solute to remove from storage).
#'
#' @param pcntToRemove The percent of solute to remove from storage
#'
#' @export
#'
Boundary_Reaction_Solute_Simple <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Simple",
    inherit = Boundary_Reaction_Solute,
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
Boundary_Reaction_Solute_Simple$set(
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
Boundary_Reaction_Solute_Simple$set(
  which = "public",
  name = "fracRemovStrg",
  value = function(){
    return(self$pcntToRemove / 100)
  }
)


#' @title Boundary_Reaction_Solute_ResTimeWtdPowerLaw
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
Boundary_Reaction_Solute_ResTimeWtdPowerLaw <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_ResTimeWtdPowerLaw",
    inherit = Boundary_Reaction_Solute,
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
Boundary_Reaction_Solute_ResTimeWtdPowerLaw$set(
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
#'   a fully saturated storage zone using the residence-time weighted fractional
#'   removal approach. If remaining = TRUE, then the function calculates the
#'   fraction of the solute REMAINING; however if remaining = FALSE, then the
#'   function calculates the fraction of solute REMOVED
Boundary_Reaction_Solute_ResTimeWtdPowerLaw$set(
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




########  STREAM-SPECIFIC BOUNDARIES   ########


#' @title Boundary_Reaction_Solute_Stream
#'
#' @description Reaction boundary for solutes in streams
#'
#' @export
#'
Boundary_Reaction_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream",
    inherit = Boundary_Reaction_Solute,
    public =
      list(
        fractionRemoved = NULL,
        fractionRemaining = NULL,
        initialize =
          function(...){
            super$initialize(...)
          }
      )
  )

#' @title calc_trade_static
#'
#' @description Calculates the trades for stream cells
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
