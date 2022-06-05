#' @title Class Cell (R6)
#' Model cell
#' @description Instantiate a \code{Cell}.
#' @importFrom R6 R6Class
#' @export
Cell <-
  R6::R6Class(
    classname = "Cell",
    public =
      list(
        #' @field cellIdx Cell index
        cellIdx = NULL,
        #' @field processDomain Process domain of the cell
        processDomain = NULL,
        #' @field currency Currency of the cell
        currency = NULL,
        #' @field linkedBoundsList List of boundaries linked to cell
        linkedBoundsList = NULL,

        #' @description Instantiate a \code{Cell} object.
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @return The ojbect of class \code{Cell}.
        initialize = function(cellIdx, processDomain, currency){
            self$cellIdx <- cellIdx
            self$processDomain <- processDomain
            self$currency <- currency
            self$linkedBoundsList <- list(upstreamBounds = list(), downstreamBounds = list())
          },
        #' @method Method Cell$populateDependencies
        #' @description Placeholder for populate dependencies methods.
        #' @return NULL
        populateDependencies = function(){
          NULL
        }

      )
  )



#' @title Class Cell_Water (R6) Water cell
#' @description Instantiate a water cell. Inherits from class
#'   \code{\link{Cell}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water <-
  R6::R6Class(
    classname = "Cell_Water",

    #' @inherit Cell return details
    inherit = Cell,

    public =
      list(
        #' @field waterVolume volume of water stored in cell
        waterVolume = NULL,
        #' @field linkedSoluteCells solute cells that are linked to the water
        #'   cell
        linkedSoluteCells = NULL,

        #' @description Create a new water cell
        #' @param waterVolume the volume of water in the cell
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param ... Inherited parameters
        #' @return The object of class \code{Cell_Water}.

        initialize =
          function(..., waterVolume = NULL){
            super$initialize(...)

            self$waterVolume <- waterVolume
          }
      )
)


#' @title Class Cell_Water_Stream (R6) A water cell in the stream processing
#'   domain
#' @description Instantiate a water cell. Inherits from class
#'   \code{\link{Cell_Water}}.
#' @importFrom R6 R6Class
#' @export

Cell_Water_Stream <-
  R6::R6Class(
    classname = "Cell_Water_Stream",

    #' @inherit Cell_Water return details
    inherit = Cell_Water,
    public =
      list(
        #' @field channelWidth Average width of stream channel in cell
        channelWidth = NULL,
        #' @field channelLength Average length of channel in stream cell
        channelLength = NULL,
        #' @field channelArea Area of surface water in stream cell
        channelArea = NULL,
        #' @field channelDepth Average depth of channel in stream cell
        channelDepth = NULL,
        #' @field waterVolume The volume of water in the cell calculated from
        #'   the \code{channelLength, channelArea, channelDepth} parameters
        waterVolume = NULL,
        #' @field channelResidenceTime Mean residence time of water in the
        #'   channel compartment
        channelResidenceTime = NULL,
        #' @field hydraulicLoad The hydraulic load of water in the channel
        #'   compartment
        hydraulicLoad = NULL,

        #' @description Instantiate a \code{Class Cell_Water_Stream} object.
        #'   Class \code{Class Cell_Water_Stream} inherits from class
        #'   \code{Cell}.
        #' @param ... Parameters inherit from Class \code{\link{Cell_Water}} and
        #'   thus \code{\link{Cell}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of
        #'   cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param channelWidth the width of the stream channel surface (distance
        #'   from left bank to right bank)
        #' @param channelLength is the length of the stream channel surface for
        #'   the cell (distance of cell from upstream to downstream)
        #' @param channelDepth  the height of the water surface above the
        #'   streambed
        #' @return The object of class \code{Cell_Water_Stream}.
        #'
        initialize =
          function(
            ...,
            channelWidth,
            channelLength,
            channelDepth
          ){
            channelArea <- channelWidth * channelLength
            waterVolume <- channelArea * channelDepth

            super$initialize(...)

            self$channelWidth <- channelWidth
            self$channelLength <- channelLength
            self$channelDepth <- channelDepth
            self$channelArea <- channelArea
            self$waterVolume <- waterVolume

          },


        #' @method Method Cell_Water_Stream$populateDependencies
        #' @description Populates the fields in the cells that depend on
        #'   boundaries being instantiated before the trade, store, update
        #'   sequence can be run.
        #' @return Updates cell values for \code{channelResidenceTime,
        #'   hydraulicLoad} based on cell values (\code{channelLength}) and
        #'   upstream/downstream boundary values (\code{channelVelocity}).
        populateDependencies = function(){

          usChannelVelocity <- mean(sapply(self$linkedBoundsList$upstreamBounds, function(bound) bound$channelVelocity))
          dsChannelVelocity <- mean(sapply(self$linkedBoundsList$downstreamBounds, function(bound) bound$channelVelocity))

          channelVelocity <- mean(c(usChannelVelocity, dsChannelVelocity))

          self$channelResidenceTime <- self$channelLength / channelVelocity
          self$hydraulicLoad <- self$channelDepth / self$channelResidenceTime
        },

        #' @method Method Cell_Water_Stream$update
        #' @description Runs the update method on all cells of class
        #'   \code{Cell_Water_Stream}.  In this current version of the model,
        #'   this simply adjusts the height of the water in the stream cell
        #'   based on the water volume, i.e., it holds the channel area constant
        #'   with changes in discharge.
        #' @return Updates cell values based on trades and stores.
        update = function(){

          self$channelDepth <- self$waterVolume / self$channelArea

          self$populateDependencies()

          return()
        }

      )
  )


#' @title Class Cell_Solute (R6)
#' A cell containing a solute.  Must be linked to a water cell.
#' @description Instantiate a \code{Cell_Solute} object. Class
#'   \code{Cell_Solute} inherits from class \code{Cell}.
#' @importFrom R6 R6Class
#' @export

Cell_Solute <-
  R6::R6Class(
    classname = "Cell_Solute",

    #' @inherit Cell return details
    inherit = Cell,
    public =
      list(
        #' @field concentration Solute concentration in user specified units;
        #'   user must ensure consistency in units
        concentration = NULL,
        #' @field amount Solute amount in user specified units (mass or mols)
        amount = NULL,
        #' @field linkedCell The water cell to which the solute cell is linked
        linkedCell = NULL,

        #' @param ... Parameters inherit from Class \code{\link{Cell}}
        #' @param cellIdx Character string denoting the index for the cell
        #' @param processDomain Character string indicating process domain of cell (soil, groundwater, or stream)
        #' @param currency Character string with either water or name of solute
        #' @param concentration the concentration of the solute in user specified units
        #'   (mass or mols per unit volume)
        #' @param linkedCell the cell containing the water in which this solute is located
        #' @return The object of class \code{Cell_Solute}.

        initialize =
          function(
            ...,
            linkedCell,
            concentration
          ){
            super$initialize(...)

            self$linkedCell <- linkedCell

            self$concentration <- concentration
            self$amount <- self$concentration * self$linkedCell$waterVolume
          },

        #' @method Method Cell_Solute$update
        #' @description Runs the update method on all cells of class
        #'   \code{Cell_Solute}.
        #' @return Updates cell values based on trades and stores.
        update = function(){

          self$concentration <- self$amount / self$linkedCell$waterVolume

          return()
        }


      )
  )

