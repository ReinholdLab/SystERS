#' @title Class Cell (R6)
#'
#' @description Instantiate a \code{Cell} object.
#'
#' @param cellIdx is the index or name of the cell
#' @param inputBoundaryIdxVect is a vector containing the indicies of the upstream boundaries.
#' @param outputBoundaryIdxVect is a vector
#'
#' @return The ojbect of class Cell
#'
#' @export
#'

Cell <-
  R6::R6Class(
    classname = "Cell",
    public =
      list(
        cellIdx = NULL, # index in network
        inputBoundaryIdxVect = NULL, # upstream boundaries (i.e., input links)
        outputBoundaryIdxVect = NULL, # downstream boundaries (i.e., output links)

        initialize =
          function(cellIdx, inputBoundaryIdxVect, outputBoundaryIdxVect){

            self$cellIdx <- cellIdx;
            self$inputBoundaryIdxVect <- inputBoundaryIdxVect;
            self$outputBoundaryIdxVect <- outputBoundaryIdxVect;


          }
      )
  )

#' @title Class StreamCell (R6)
#'
#' @description Instantiate a \code{StreamCell} object.  \code{StreamCell}
#'   inherits from class \code{Cell}.
#'
#' @param waterVolume is the volume of water in the cell.
#' @param soluteMass is the mass of the solute in the cell.
#' @param soluteConcentration will be calculated from the
#'   \code{waterVolume} and \code{soluteMass}.  Therefore, it is the end user's
#'   responsibility to ensure that the units of volume and mass are consistent
#'   throughout their simulation.
#'
#' @return The ojbect of class \code{StreamCell}
#'
#' @export

StreamCell <-
  R6::R6Class(
    classname = "StreamCell",
    inherit = Cell,
    public =
      list(
        waterVolume = NULL,
        soluteMass = NULL,
        soluteConcentration = NULL,
        initialize =
          function(..., waterVolume, soluteMass, soluteConcentration){
            super$initialize(...);
            self$waterVolume <- waterVolume;
            self$soluteMass <- soluteMass;
            self$soluteConcentration <- (soluteMass / waterVolume);
          }
      )
  )
