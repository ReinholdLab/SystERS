#' @title Boundary
#'
#' @description Boundary superclass
#'
#' @param boundaryIdx the name of the boundary.
#' @param currency the name of the currency handled by the boundary as a character e.g., \code{H20}
#' @param boundarySuperClass the super class of the boundary, e.g., \code{transport} or \code{reaction}
#' @param upstreamCell  the upstream cell
#' @param downstreamCell the downstream cell
#' @param pcntToRemove is the percent of the solute to remove fom a cell by the
#'   reaction boundary IF the removal method is set to \code{pcnt}
#' @param alpha is the exponent of the power law used to represent the shape of
#'   the residence time distribution of the hyporheic zone.
#' @param tauMin is the minimum residence time for the hyporheic zone.
#' @param tauMax is the maximum residence time for the hyporheic zone.
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

        upstreamCell = NULL,
        downstreamCell = NULL,

        usModBound = NULL,
        dsModBound = NULL,

        linkedBound = NULL,

        initialize =
          function(
            boundaryIdx,
            currency,
            boundarySuperClass,
            calculateOrder,

            upstreamCell,
            downstreamCell,

            linkedBound
            ){

            self$boundaryIdx <- boundaryIdx
            self$currency <- currency
            self$boundarySuperClass <- boundarySuperClass
            self$calculateOrder <- calculateOrder

            self$upstreamCell <- upstreamCell
            self$downstreamCell <- downstreamCell

            # Is this boundary a model boundary? Check to see if it has either
            # no u/s or no d/s cell...
            self$usModBound <- !(is.environment(self$upstreamCell)) # has no u/s cell
            self$dsModBound <- !(is.environment(self$downstreamCell)) # has no d/s cell

            # other boundary it is linked to
            self$linkedBound <- linkedBound

          }

      )
  )


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
#' @description Populate boundary dependencies
#' @param boundary The boundary to update
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "populateDependencies",
  value = function(boundary){
    # To get velocity, divide Q by the mean of x-sec area of u/s and d/s
    # cell.  Upstream model boundaries (ie, most upstream) will thus
    # have a velocity equal only to the d/s cell (because there is no
    # u/s cell).  The opposite is true for the most d/s boundaries.
    # Likewise, to get residence time, divide by the mean of u/s and d/s
    # channel lengths.  Upstream model boundaries will thus have the
    # channel length defined by the d/s cell because no u/s cell exists
    # and vice versa for d/s model boundaries. Same pattern applies to
    # hydraulic load...
    if(!any(c(boundary$usModBound, boundary$dsModBound))) {
      depth <- mean(c(boundary$upstreamCell$channelDepth, boundary$downstreamCell$channelDepth))
      widthXdepth <- mean(c(boundary$upstreamCell$channelWidth * boundary$upstreamCell$channelDepth, boundary$downstreamCell$channelWidth * boundary$downstreamCell$channelDepth))
      len <- mean(c(boundary$upstreamCell$channelLength, boundary$downstreamCell$channelLength))
    }else{
      if(boundary$usModBound) {
        depth <- boundary$downstreamCell$channelDepth
        widthXdepth <- boundary$downstreamCell$channelWidth * depth
        len <-boundary$downstreamCell$channelLength
      }
      if(boundary$dsModBound){
        depth <- boundary$upstreamCell$channelDepth
        widthXdepth <- boundary$upstreamCell$channelWidth * depth
        len <-boundary$upstreamCell$channelLength
      }
    }
    boundary$channelVelocity <- boundary$discharge / widthXdepth
    boundary$channelResidenceTime <- len / boundary$channelVelocity
    boundary$hydraulicLoad <- depth / boundary$channelResidenceTime

    return()
  }
)

#' @method trade_static
#' @description Calculates the trades between the water cells using static
#'   values for the boundary conditions; these values are provided by the user.
#' @param boundary The boundary for which the trade is calculated
#' @param timeInterval  The model time step
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "trade_static",
  value =
    function(boundary, timeInterval){
      self$boundary <- boundary

      # volume of water to trade
      volume <- boundary$discharge * timeInterval #L

      if(!boundary$usModBound){
        # volume of water to remain
        volumeToRemain <- boundary$upstreamCell$channelVolume_L - volume
        if(volumeToRemain < 0) warning(
          paste(
            "You are about to remove more water volume from a cell than it held at the start of the timestep.
             Boundary is", boundary$boundaryIdx,
            "Cell is", boundary$upstreamCell$upstreamCellIdx
          ) # close paste
        ) # close warning
      } # close if statement

      return(list(discharge = boundary$discharge, volume = volume))
    } # close function def
)



#' @title Boundary_Transport_Solute_Stream
#' @description Transport boundary for solutes between stream cells.
#'
Boundary_Transport_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Solute_Stream",
    inherit = Boundary,
    public =
      list(
        load = NULL,
        amount = NULL,
        initialize =
          function(
            ..., timeStep, load
          ){
            super$initialize(...)

            self$load <- load
            self$amount <- load * timeStep
          } # close initialize
      ) # close public
  ) # close R6 class


#' @method trade_static
#'
#' @description Calculate the amount of solute to pass through the boundary.
#'
#' @param boundary the boundary
#'
#' @return The amount of solute and the load to pass through the boundary.
#'
Boundary_Transport_Solute_Stream$set(
  which = "public",
  name = "trade_static",
  value =
    function(timeInterval){
      # get the discharge and solute concentration in the water transport
      # boundary to which this solute transport boundary is linked
      discharge <- self$linkedBound$discharge # L s-1

      if(!self$usModBound) {
        upstreamConcentration <- self$upstreamCell$linkedCell$concentration # g  m-3
        # multiply discharge by concentration to get load
        self$load <- self$linkedBound$discharge * upstreamConcentration # g s-1
      }

      # mass to of solute to trade
      self$amount <- self$load * timeInterval # g

      if(!boundary$usModBound){
        # solute mass to remain
        soluteToRemain <- boundary$upstreamCell$soluteMass - soluteToTrade
        if(soluteToRemain < 0) warning(
          paste("You are about to remove more solute from a cell than it held at the start of the timestep.
                      Boundary is", boundary$boundaryIdx,
                "Cell is", boundary$upstreamCell$upstreamCellIdx
          )
        )
      }

      return(list(load = self$load, amount = self$amount))
    }
)




  #' @title Boundary_Reaction_Solute_Stream
  #' @description Reaction boundary for solutes in stream cells.
  #'
Boundary_Reaction_Solute_Stream <-
  R6::R6Class(
    classname = "Boundary_Reaction_Solute_Stream",
    inherit = Boundary,
    public =
      list(
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
            super$initialize(...)

            self$removalMethod <- removalMethod



            ##################### AMR stopped editing  ported code here 09/10/2020 at 1:10 PM  ###########


            if( is.null(self$removalMethod) ) {

              warning(
                paste0(
                  "The removalMethod is currently NULL for boundaryIdx = ",
                  self$boundaryIdx, ", .  A removalMethod must be provided.")
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
              warning(
                noquote( strsplit (msgDetail, "\n") [[1]])
              )
            }

            self$fractionRemaining <- exp(-1 * boundary$qStorage * self$fractionRemovedStorage * timeInterval / boundary$upstreamCell$channelDepth)
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
              warning(
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

            self$rxnVals <-
              data.frame(
                boundary = boundary$boundaryIdx,
                removalMethod = self$removalMethod ,
                fracRemoved = self$fractionRemoved ,
                fracRemaning = self$fractionRemaining ,
                fracRemovedFromStrg = self$fractionRemovedStorage ,
                fracRemainingInStrg = self$fractionRemainingStorage ,
                mustBeOne = self$mustBeOne ,
                startingMass = self$startingMass ,
                massToRemove = self$massToRemove
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
    return(boundary$pcntToRemove / 100)
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
        lower = boundary$tauMin,
        upper = boundary$tauMax,
        tau_0 = boundary$tauMin,
        tau_n = boundary$tauMax,
        a = boundary$alpha,
        k = boundary$k,
        remaining = remaining
      )$value
    return(prop.uptk)

  }
)
