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

        discharge = NULL,
        channelVelocity = NULL,
        channelResidenceTime = NULL,
        hydraulicLoad = NULL,

        pcntToRemove = NULL,
        k = NULL,
        qStorage = NULL,

        usModBound = NULL,
        dsModBound = NULL,

        soluteLoad = NULL,

        linkedTo = NULL,

        initialize =
          function(
            boundaryIdx,
            currency,
            boundarySuperClass,
            calculateOrder,

            upstreamCell,
            downstreamCell,

            discharge,
            channelVelocity,
            channelResidenceTime,
            hydraulicLoad,

            pcntToRemove,
            k,
            qStorage,

            soluteLoad,

            linkedTo
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

            self$discharge <- as.numeric(discharge) # as.numeric is b/c reading from sparse table



            self$soluteLoad <- soluteLoad

            self$pcntToRemove <- as.numeric(pcntToRemove) # as.numeric is b/c reading from excel sparse table with NAs
            self$k <- k
            self$qStorage <- qStorage

            # other boundary it is linked to
            self$linkedTo <- linkedTo

          }

      )
  )

#' Populate boundary dependencies
#'
Boundary$set(
  which = "public",
  name = "populateDependencies",
  value = function(boundary){
    if(boundary$boundarySuperClass == "transport" & boundary$currency == "H2O"){

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
    }
    return()
  }
)
