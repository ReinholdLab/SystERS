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
        cells = NULL,
        bounds = NULL,

        initialize =
          function(boundsTable, cellsTable) {
            self$boundsTable <- boundsTable
            self$cellsTable <- cellsTable

            # generate the stream cells from the cellsTable
            self$cells <-
              plyr::llply(
                1:nrow(cellsTable),
                function(rowNum){
                  StreamCell$new(
                    cellIdx = cellsTable$cellIdx[rowNum],
                    soluteConcentration = cellsTable$initConcNO3[rowNum],
                    discharge = cellsTable$discharge[rowNum],
                    alpha = cellsTable$alpha[rowNum],
                    aquiferVolume = cellsTable$aquiferVolume[rowNum],
                    porosity = cellsTable$porosity[rowNum],
                    channelWidth = cellsTable$channelWidth[rowNum],
                    channelLength = cellsTable$channelLength[rowNum],
                    channelArea = cellsTable$channelLength[rowNum],
                    channelDepth = cellsTable$channelDepth[rowNum],
                    channelVelocity = cellsTable$channelVelocity[rowNum],
                    channelResidenceTime = cellsTable$channelResidenceTime[rowNum],
                    qStorage = cellsTable$qStorage[rowNum],
                    hydraulicLoad = cellsTable$hydraulicLoad[rowNum],
                    tauMin = cellsTable$tauMin[rowNum],
                    tauMax = cellsTable$tauMax[rowNum]
                  )
                }
              )

            # Generate the boundaries from the boundsTable in the model

            # First, order the bounds table by the calculate order column. This
            # ordering makes it so that you can call the boundaries in the the
            # correct order simply by looping through the list of bounds.
            boundsTable <- boundsTable[order(boundsTable$calculateOrder), ]

            self$bounds <-
              plyr::llply(
              1:nrow(boundsTable),
              function(rowNum) {
                Boundary$new(
                  boundaryIdx = boundsTable$boundaryIdx[rowNum],
                  currency = boundsTable$currency[rowNum],
                  boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                  upstreamCellIdx = boundsTable$upstreamCellIdx[rowNum],
                  downstreamCellIdx = boundsTable$downstreamCellIdx[rowNum],
                  calculateOrder = boundsTable$calculateOrder[rowNum]
                )
              }
            )


          } # closes initialize function
      ) # closes public list
  ) # closes WQ model



## Need to add error checks to ensure that sublist names and structures are
## appropriate and that the network structure is appropriate
