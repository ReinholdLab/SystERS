#' @title Class WQModel (R6)
#'
#' @description Define the WQ model and the network topology of cells and
#'   boundaries
#'
#' @export
#'
#' @return The ojbect of class \code{WQModel}.
#'
#' @param boundsTable At this point, \code{booundsTable} needs to be a table
#'   specifying the currency, type of boundary (reaction or transport), the
#'   boundaryIdx (name as character), the upstreamCellIdx, the
#'   downstreamCellIdx, and the calculateOrder.
#' @param cellsTable must contain the processDomain, cellIdx (as character) and
#'   all other neccessary values to have a cell representing a process domain.

WQModel <-
  R6::R6Class(
    classname = "WQModel",

    public =
      list(
        boundsTable = NULL,
        cellsTable = NULL,
        initialize =
          function(boundsTable, cellsTable) {
            self$boundsTable <- boundsTable
            self$cellsTable <- cellsTable
          }
      )
  )



## Need to add error checks to ensure that sublist names and structures are
## appropriate and that the network structure is appropriate
