#' @title Class Cell (R6)
#'
#' @description Instantiate a \code{Cell} object.
#'
#' @param cellIdx is the index for the cell
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
        currency = NULL,
        initialize =
          function(cellIdx){
            self$cellIdx <- cellIdx
            self$processDomain <- processDomain
            self$currency <- currency
          }
      )
  )


#' @title Class Cell_Water (R6)
#'
#' @description Instantiate a \code{Class Cell_Water} object. Class
#'   \code{Class Cell_Water} inherits from class \code{Cell}.
#'
#' @param waterVolume the volume of water in the cell
#'
#' @export
#'
Cell_Water <-
  R6::R6Class(
    classname = "Cell_Water",
    inherit = Cell,
    public =
      list(
        waterVolume = NULL,
        # linkedSoluteCells = NULL,

        initialize =
          function(
            waterVolume,
            # linkedSoluteCells,
            ...
            ){

            super$initialize(...)

            self$waterVolume <- waterVolume
            # self$linkedSoluteCells <- linkedSoluteCells
          }
      )
  )

Cell_Water$set(
  which = "public",
  name = "linkedSoluteCells",
  value = function(linkedSoluteCells){
    self$linkedSoluteCells <- linkedSoluteCells
  }
)




#' @title Class Cell_Water_Stream (R6)
#'
#' @description Instantiate a \code{Class Cell_Water_Stream} object. Class
#'   \code{Class Cell_Water_Stream} inherits from class \code{Cell}.
#'
#' @param channelWidth the width of the stream channel surface (distance from
#'   left bank to right bank)
#' @param channelLength is the length of the stream channel surface for the cell
#'   (distance of cell from upstream to downstream)
#' @param channelArea the area of the stream channel surface of the water in the
#'   cell
#' @param channelDepth  the height of the water surface above the streambed
#' @param channelVolume the volume of water in the cell
#'
#' @export
#'
#' @return The object of class \code{Cell_Water_Stream}.
#'
Cell_Water_Stream <-
  R6::R6Class(
    classname = "Cell_Water_Stream",
    inherit = Cell,
    public =
      list(
        channelWidth = NULL,
        channelLength = NULL,
        channelArea = NULL,
        channelDepth = NULL,

        # waterVolume = NULL,

        initialize =
          function(
            ...,
            channelWidth,
            channelLength,
            channelDepth
          ){
            super$initialize(...)

            self$channelWidth <- channelWidth
            self$channelLength <- channelLength
            self$channelDepth <- channelDepth
            self$channelArea <- self$channelWidth * self$channelLength
            self$waterVolume <- self$channelArea * self$channelDepth
          }
      )
  )


#' @title Class Cell_Solute (R6)
#'
#' @description Instantiate a \code{Cell_Solute} object. Class
#'   \code{Cell_Solute} inherits from class \code{Cell}.
#'
#' @param concentration the concentration of the solute in user specified units
#'   (mass or mols per unit volume)
#' @param amount the amount of the solute in user specified units (mass or mols)
#' @param linkedCell the cell containing the water in which this solute is located
#'
#' @export
#'
#' @return The object of class \code{Cell_Solute}.
#'
Cell_Solute <-
  R6::R6Class(
    classname = "Cell_Solute",
    inherit = Cell,
    public =
      list(
        concentration = NULL,
        amount = NULL,

        linkedCell = NULL,

        initialize =
          function(
            ...,
            linkedCell,
            concentration
          ){
            super$initialize(...)

            self$concentration <- concentration
            self$amount <- self$concentration * self$linkedCell$waterVolume

          }
      )
  )
