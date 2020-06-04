#' @title Class Cell (R6)
#'
#' @description Instantiate a \code{Cell} object.
#'
#' @param cellIdx is the index for the cell
#' @param pcntToRemove is the percent of the solute to remove fom a cell by the
#'   reaction boundary IF the removal method is set to \code{pcnt}
#'
#' @export
#'
#' @return The ojbect of class \code{Cell}.
#'

Cell <-
  R6::R6Class(
    classname = "Cell",
    public =
      list(
        cellIdx = NULL,
        pcntToRemove = NULL,
        initialize =
          function(cellIdx, pcntToRemove = NULL){
            self$cellIdx <- cellIdx
            self$pcntToRemove <- pcntToRemove
          }
      )
  )

#' @title Class StreamCell (R6)
#'
#' @description Instantiate a \code{StreamCell} object. Class \code{StreamCell}
#'   inherits from class \code{Cell}. The following parameters will be
#'   calculated on the fly from the remaining parameters if they are not
#'   specified directly by the user: \code{channelArea, channelVelocity,
#'   channelResidenceTime, qStorage, hydraulicLoad}.
#'
#' @param solute concentration is the concentration of the solute in user
#'   specified units.
#' @param discharge is the water discharge, Q, in user specified units.
#' @param alpha is the exponent of the power law used to represent the shape of
#'   the residence time distribution of the hyporheic zone.
#' @param aquiferVolume is the volume of the stream aquifer/hyporheic zone.
#' @param porosity is the porosity of the streambed and hyporheic zone.
#' @param channelWidth is the width of the stream channel surface.
#' @param channelLength is the length of the stream channel surface for the
#'   given reach represented by the \code{StreamCell} specified herein.
#' @param channelArea is the area of the stream channel surface for the given
#'   \code{StreamCell}.
#' @param channelDepth is the height of the water surface above the streambed.
#' @param channelVelocity is the mean velocity of the water in the stream
#'   channel.
#' @param channelResidenceTime is the mean residence time of the reach
#'   represented by the \code{StreamCell} specified herein.
#' @param qStorage is the total hyporheic exchange rate, i.e., the rate at which
#'   water is downwelling into and - since this model assumes steady state -
#'   upwelling up from the hyporheic zone.
#' @param hydraulicLoad is the hydraulic load for the reach represented by the
#'   \code{StreamCell} specified herein.
#' @param tauMin is the minimum residence time for the hyporheic zone.
#' @param tauMax is the maximum residence time for the hyporheic zone.
#'
#' @export
#'
#' @return The object of class \code{Cell}.
#'
StreamCell <-
  R6::R6Class(
    classname = "StreamCell",
    inherit = Cell,
    public =
      list(
        soluteConcentration = NULL,
        soluteMass = NULL,
        discharge = NULL,
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
        tauMin = NULL,
        tauMax = NULL,
        k = NULL,
        channelVolume_L = NULL,
        channelVolume_m3 = NULL,

        initialize =
          function(...,
                   soluteConcentration,
                   discharge,
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
                   tauMin,
                   tauMax,
                   k
          ){

            super$initialize(...)

            self$soluteConcentration <- soluteConcentration
            self$discharge <- discharge
            self$alpha <- alpha
            self$aquiferVolume <- aquiferVolume
            self$porosity <- porosity
            self$channelWidth <- channelWidth
            self$channelLength <- channelLength
            self$channelDepth <- channelDepth
            self$channelArea <- ifelse(!is.null(channelArea), channelArea, channelWidth * channelLength)
            self$channelVelocity <- ifelse(!is.null(channelVelocity), channelVelocity, discharge / (channelWidth * channelDepth))
            self$channelResidenceTime <- ifelse(!is.null(channelResidenceTime), channelResidenceTime, channelLength / channelVelocity)
            self$qStorage <- ifelse(!is.null(qStorage), qStorage, stop("qStorage must be provided for each cell") )
            self$hydraulicLoad <- ifelse(!is.null(hydraulicLoad), hydraulicLoad, channelDepth / channelResidenceTime)
            self$tauMin <- tauMin
            self$tauMax <- tauMax
            self$k <- k

            self$channelVolume_m3 <- self$channelArea*self$channelDepth
            self$channelVolume_L <- 1000 *  self$channelVolume_m3

            # have to multiply by 1000 because units of concentration are ug/L
            # but cell volume is m3 --- will need to decide how we want to
            # handle this in time as hardcoding it this way is clearly a bad
            # idea...anyhow, this gives units of ug NO3-N
            self$soluteMass <- self$soluteConcentration * self$channelVolume_L

          }
      )
  )



