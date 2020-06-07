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
        volumeToTrade = NULL,
        volumeToRemain = NULL,
        timeInterval = NULL,
        tradeType = "move",
        initialize =
          function(boundary, timeInterval){

            # discharge in the cell
            self$dischargeToTrade <- boundary$upstreamCell$discharge # L s-1

            # volume of water to trade
            self$volumeToTrade <- self$dischargeToTrade * timeInterval #L

            # volume of water to remain
            self$volumeToRemain <- boundary$upstreamCell$channelVolume_L - self$volumeToTrade

            if(self$volumeToRemain < 0) warning("You are about to remove more water volume from a cell than it currently holds.")

            return(list(discharge = self$dischargeToTrade, volumeToTrade = self$volumeToTrade, volumeToRemain = self$volumeToRemain))

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
        load = NULL,
        soluteToTrade = NULL,
        soluteToRemain = NULL,
        timeInterval = NULL,
        upstreamCellDischarge = NULL,
        upstreamCellConcentration = NULL,
        tradeType = "move",
        initialize = function(boundary, timeInterval){
          self$boundary <- boundary
          self$timeInterval <- timeInterval

          # get the discharge and solute concentration in the upstream cells
          self$upstreamCellDischarge <- boundary$upstreamCell$discharge # L s-1
          self$upstreamCellConcentration <- boundary$upstreamCell$soluteConcentration # ug N L-1

          # multiply discharge by concentration to get load
          self$load <- self$upstreamCellDischarge * self$upstreamCellConcentration # ug N s-1

          # mass to of solute to trade
          self$soluteToTrade <- self$load * self$timeInterval # ug N

          self$soluteToRemain <- boundary$upstreamCell$soluteMass - self$soluteToTrade

          if(self$soluteToRemain < 0) warning("You are about to remove more solute mass from a cell than is present at the start of a time step")

          return(list(load = self$load, massToTrade = self$soluteToTrade, massToRemain = self$soluteToRemain))
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
        tradeType = "remove",
        initialize =
          function(
            boundary,
            removalMethod,
            timeInterval,
            ...
          ){
            self$boundary <-boundary
            self$removalMethod <- removalMethod

            if( is.null(self$removalMethod) ) {

              stop(
                paste0(
                  "The removalMethod is currently NULL for boundaryIdx = ",
                  boundary$boundaryIdx, ", .  A removalMethod must be provided.")
              )

            } else if(self$removalMethod == "RT-PL") {

              self$fractionRemovedStorage <- self$resTmWtdFracRemovStrg(boundary, remaining = FALSE)
              # print(self$fractionRemovedStorage)
              self$fractionRemainingStorage <- self$resTmWtdFracRemovStrg(boundary, remaining = TRUE)
              # print(self$fractionRemainingStorage)

            } else if(self$removalMethod == "pcnt") {

              self$fractionRemovedStorage <- self$fracRemovSimple(boundary)
              self$fractionRemainingStorage <- 1 - self$fractionRemovedStorage
            }

            if( round(sum(self$fractionRemovedStorage, self$fractionRemainingStorage), 3) != 1 ) {
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

            self$fractionRemaining <- exp(-boundary$upstreamCell$qStorage * self$fractionRemovedStorage * timeInterval / boundary$upstreamCell$channelDepth)
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

            self$startingMass <- boundary$upstreamCell$soluteMass

            self$massToRemove <- self$startingMass * self$fractionRemoved
            self$massToRemain <- self$startingMass * self$fractionRemaining

            self$rxnVals <-
              data.frame(
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

            # print(self$rxnVals)

          }
      )
  )




#' @title fracRemovSimple
#'
#' @description Calculates the fraction of a solute removed using a very simple
#'   approach of removing a certain percentage of the initial solute.
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


#' @title resTmWtdFracRemovStrg
#'
#' @description Calculates the fraction of a solute removed and
#'   remaining in the storage zone using the residence-time weighted fractional
#'   removal approach. If remaining = TRUE, then the function
#' calculates the fraction of the solute REMAINING; however if remaining = FALSE,
#' then the function calculates the fraction of solute REMOVED
CalcFractionalSoluteDynams$set(
  which = "public",
  name = "resTmWtdFracRemovStrg",
  value = function(boundary,
                   remaining){

    prop.uptk.funct <-
      function(
        tau,
        tau_0 ,
        tau_n ,
        a,
        k,
        remaining
      ){
        PL_PDF <- hydrogeom::powerLawPDF(tau, tau_0, tau_n, a)
        minus.k.t <- (-1*k*tau) # if the -1  is removed, an "invalid argument to unary operator" error is thrown

        if(remaining){
          out <- PL_PDF * exp(minus.k.t)
        }else{
          out <- PL_PDF * (1-exp(minus.k.t))
        }
        return(out)
      }

    prop.uptk <-
      integrate(
        prop.uptk.funct,
        lower = boundary$upstreamCell$tauMin,
        upper = boundary$upstreamCell$tauMax,
        tau_0 = boundary$upstreamCell$tauMin,
        tau_n = boundary$upstreamCell$tauMax,
        a = boundary$upstreamCell$alpha,
        k = boundary$upstreamCell$k,
        remaining = remaining
      )$value
    return(prop.uptk)

  }
)
