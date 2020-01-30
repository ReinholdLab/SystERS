#' @title Class Boundary (R6)
#'
#' @description Instantiate a \code{Boundary} object.
#'
#' @param boundaryIdx is the index for the boundary.
#' @param inputCellIdx is the index for the upstream cell.
#' @param outputCellIdx is the index for the downstream cell.
#'
#' @export
#'
#' @return The ojbect of class \code{Boundary}.
#'

Boundary <-
  R6::R6Class(
    classname = "Boundary",
    public =
      list(
        boundaryIdx = NULL, # index of boundary in network
        inputCellIdx = NULL, # upstream cell/node
        outputCellIdx = NULL,
        proportionOfInputCellFlow = NULL,

        initialize =
          function(boundaryIdx, inputCellIdx, outputCellIdx, proportionOfInputCellFlow){

            self$boundaryIdx <- boundaryIdx
            self$inputCellIdx <- inputCellIdx
            self$outputCellIdx <- outputCellIdx
            self$proportionOfInputCellFlow <- proportionOfInputCellFlow

          }
      )
  )

#' @title Class StreamBoundary (R6)
#'
#' @description Instantiate a \code{StreamBoundary} object. Class
#'   \code{StreamBoundary} inherits from class \code{Boundary}. The following
#'   parameters will be calculated on the fly from the remaining parameters if
#'   they are not specified directly by the user: \code{channelArea,
#'   channelVelocity, channelResidenceTime, qStorage, hydraulicLoad}.
#'
#' @param qChannel is the surface water discharge, Q, in user specified units.
#' @param alpha is the exponent of the power law used to represent the shape of
#'   the residence time distribution of the hyporheic zone.
#' @param aquiferVolume is the volume of the stream aquifer/hyporheic zone.
#' @param porosity is the porosity of the streambed and hyporheic zone.
#' @param channelWidth is the width of the stream channel surface.
#' @param channelLength is the length of the stream channel surface for the
#'   given reach represented by the \code{StreamBoundary} specified herein.
#' @param channelArea is the area of the stream channel surface for the given
#'   \code{StreamBoundary}.
#' @param channelDepth is the height of the water surface above the streambed.
#' @param channelVelocity is the mean velocity of the water in the stream
#'   channel.
#' @param channelResidenceTime is the mean residence time of the reach
#'   represented by the \code{StreamBoundary} specified herein.
#' @param qStorage is the total hyporheic exchange rate, i.e., the rate at which
#'   water is downwelling into and - since this model assumes steady state -
#'   upwelling up from the hyporheic zone.
#' @param hydraulicLoad is the hydraulic load for the reach represented by the
#'   \code{StreamBoundary} specified herein.
#' @param proportionOfUpstreamQ is the proportion of the water at the upstream
#'   cell to enter into the stream boundary.  In cases where a boundary has only
#'   one upstream cell, this value should be set to \code{1.0}. However, if flow
#'   from an upstream cell is divergent between one or more downstream
#'   boundaries, this parameter needs to be adjusted accordingly.
#' @param tauMin is the minimum residence time for the hyporheic zone.
#' @param tauMax is the maximum residence time for the hyporheic zone.
#'
#' @export
#'
#' @return The object of class \code{Boundary}.
#'
StreamBoundary <-
  R6::R6Class(
    classname = "StreamBoundary",
    inherit = Boundary,
    public =
      list(
        qChannel = NULL,
        alpha = NULL,
        aquiferVolume = NULL,
        porosity = NULL,
        channelWidth = NULL,
        channelLength = NULL,
        channelArea = NULL,
        channelDepth = NULL,
        channelVelocity = NULL,
        channelResidenceTime = NULL,
        qStorage = NULL,
        hydraulicLoad = NULL,
        proportionOfUpstreamQ = NULL,
        tauMin = NULL,
        tauMax = NULL,

        initialize =
          function(...,
                   qChannel,
                   alpha,
                   aquiferVolume,
                   porosity,
                   channelWidth,
                   channelLength,
                   channelArea,
                   channelDepth,
                   channelVelocity,
                   channelResidenceTime,
                   qStorage,
                   hydraulicLoad,
                   proportionOfUpstreamQ,
                   tauMin,
                   tauMax){

            super$initialize(...)

            self$qChannel <- qChannel
            self$alpha <- alpha
            self$aquiferVolume <- aquiferVolume
            self$porosity <- porosity
            self$channelWidth <- channelWidth
            self$channelLength <- channelLength
            self$channelDepth <- channelDepth
            self$channelArea <- ifelse(!is.null(channelArea), channelArea, channelWidth * channelLength)
            self$channelVelocity <- ifelse(!is.null(channelVelocity), channelVelocity, qChannel / (channelWidth * channelDepth))
            self$channelResidenceTime <- ifelse(!is.null(channelResidenceTime), channelResidenceTime, channelLength / channelVelocity)
            self$qStorage <- ifelse(!is.null(qStorage), qStorage, (aquiferVolume * porosity) / (channelArea) )
            self$hydraulicLoad <- ifelse(!is.null(hydraulicLoad), hydraulicLoad, channelDepth / channelResidenceTime)
            self$proportionOfUpstreamQ <- proportionOfUpstreamQ
            self$tauMin <- tauMin
            self$tauMax <- tauMax

          }
      )
  )


#' @title WaterFluxCalculator
#'
#' @description Calculate the flux of water (from the upstream cell) into a boundary.
#'
#' @param upstreamCellIdx is the name of the upstream cell as a vector.
#' @param downstreamCellIdx is the name of the downstream cell as a vector.
#' @param additionalInput is the volume of water to be added, if any.  Default value is \code{0}.
#'
Boundary$set(
  which = "public",
  name = "waterVolume",
  value =
    function(
      upstreamCell,
      downstreamCell,
      additionalInput = 0
    ){
      return(upstreamCell$waterVolume * self$proportionOfInputCellFlow + additionalInput)
    }
)

