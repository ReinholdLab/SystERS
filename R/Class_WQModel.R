#' @title Class WQModel (R6)
#'
#' @description Define the WQ model and the network topology of cells and
#'   boundaries
#'
#' @export
#'
#' @return The ojbect of class \code{WQModel}.  Boundaries and cells are stored
#'   in lists called \code{bounds} and \code{cells}, respectively.
#'
#' @param boundsTable At this point, \code{booundsTable} needs to be a table
#'   specifying the currency, type of boundary (reaction or transport), the
#'   boundaryIdx (name as character), the upstreamCellIdx, the
#'   downstreamCellIdx, and the calculateOrder.
#' @param cellsTable must contain the processDomain, cellIdx (as character) and
#'   all other neccessary values to have a cell representing a process domain.
#' @param unitsTable is a reference table for the end user, containing the units
#'   of their input parameters.  At this time, the model doesn't do anything
#'   with this table.  It is only for reference.

WQModel <-
  R6::R6Class(
    classname = "WQModel",

    public =
      list(
        modelName = NULL,
        boundsTable = NULL,
        cellsTable = NULL,
        unitsTable = NULL,
        cells = NULL,
        bounds = NULL,
        soluteRemovalMethod = NULL,
        pcntToRemove = NULL,
        k = NULL,
        timeInterval = NULL,

        initialize =
          function(modelName, boundsTable, cellsTable, unitsTable, soluteRemovalMethod, k, timeInterval, ...) {
            # set duration of each time step
            self$timeInterval = timeInterval

            self$modelName <- modelName
            self$cellsTable <- cellsTable

            # A series of error checks:
            if( any( duplicated(self$cellsTable) ) ){
              stop("At least one specification for a cell is duplicated in the cellsTable.")
            }
            if(length(cellsTable$cellIdx) != length(unique(cellsTable$cellIdx))) {
              stop("A cell name was duplicated in the cellsTable.  All cell names must be unique.")
            }

            self$unitsTable <- unitsTable

            self$soluteRemovalMethod <- soluteRemovalMethod

            self$k <- k

            # generate the stream cells from the cellsTable
            self$cells <-
              plyr::llply(
                1:nrow(cellsTable),
                function(rowNum){
                  StreamCell$new(
                    cellIdx = cellsTable$cellIdx[rowNum],
                    soluteConcentration = cellsTable$initConcNO3[rowNum],
                    pcntToRemove = cellsTable$pcntToRemove[rowNum],
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
                    tauMax = cellsTable$tauMax[rowNum],
                    k = self$k
                  )
                }
              )
            names(self$cells) <- cellsTable$cellIdx

            # Generate the boundaries from the boundsTable in the model

            # First, order the bounds table by the calculate order column. This
            # ordering makes it so that you can call the boundaries in the the
            # correct order simply by looping through the list of bounds.
            boundsTable <- boundsTable[order(boundsTable$calculateOrder), ]
            self$boundsTable <- boundsTable

            # A series of error checks:
            if( any( duplicated(self$boundsTable) ) ){
              stop("At least one specification for a boundary is duplicated.")
            }
            if(length(boundsTable$boundaryIdx) != length(unique(boundsTable$boundaryIdx))) {
              stop("A boundary name was duplicated in the boundsTable.  All boundary names must be unique.")
            }
            if( any(!(unique(boundsTable$downstreamCellIdx) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
              stop("In the boundsTable, a name of a downstream cell was provided that refers to a cell that has not been instantiated.")
            }
            if( any(!(unique(boundsTable$upstreamCellIdx) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
              stop("In the boundsTable, a name of an upstream cell was provided that refers to a cell that has not been instantiated.")
            }


            self$bounds <-
              plyr::llply(
                1:nrow(boundsTable),
                function(rowNum) {

                  usCellIdx <- self$cells[[ which(names(self$cells) ==  self$boundsTable$upstreamCellIdx[rowNum]) ]]$cellIdx
                  dsCellIdx <- self$cells[[ which(names(self$cells) ==  self$boundsTable$downstreamCellIdx[rowNum]) ]]$cellIdx

                  # print(paste("bound:", boundsTable$boundaryIdx[rowNum] , ";   u/s cell:", usCellIdx, ";   d/s cell:", dsCellIdx, sep = ""))

                    Boundary$new(
                      boundaryIdx = boundsTable$boundaryIdx[rowNum],
                      currency = boundsTable$currency[rowNum],
                      boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                      upstreamCell = self$cells[[ usCellIdx ]],
                      downstreamCell = self$cells[[ dsCellIdx ]],
                      calculateOrder = boundsTable$calculateOrder[rowNum]
                    )

                }
              )
            names(self$bounds) <- boundsTable$boundaryIdx


          } # closes initialize function
      ) # closes public list
  ) # closes WQ model


#' @method WQModel$trade
#'
#' @description Calculates all the trades for the model
#'
WQModel$set(
  which = "public",
  name = "trade",
  value = function(...){
    # empty vectors to populate
    boundIdx <- NA
    tradeVals <- NA
    tradeCurrency <- NA
    rxnVals <- NULL
    valName <- NA

    for(i in 1:length(self$bounds) ){

      tradeCurrency[i] <- self$bounds[[i]]$currency
      boundIdx[i] <- self$bounds[[i]]$boundaryIdx

      if(self$bounds[[i]]$currency == "H2O" & self$bounds[[i]]$boundarySuperClass == "transport"){
        tradeVals[i] <- WaterTransportPerTime$new(self$bounds[[i]], self$timeInterval)$volumeToTrade
        valName[i] <- "water volume"
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "transport"){
        tradeVals[i] <- SoluteTransportPerTime$new(self$bounds[[i]], self$timeInterval)$soluteToTrade
        valName[i] <- "solute mass"
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "reaction"){
        tradeVals[i] <- CalcFractionalSoluteDynams$new(boundary = self$bounds[[i]], removalMethod = self$soluteRemovalMethod, timeInterval = self$timeInterval)$massToRemove
        valName[i] <- "solute mass"
        if(is.null(rxnVals) ) {

          newVals <- CalcFractionalSoluteDynams$new(boundary = self$bounds[[i]], removalMethod = self$soluteRemovalMethod, timeInterval = self$timeInterval)$rxnVals
          rxnVals <- data.frame(matrix(nrow = 0, ncol = length(newVals)))
          colnames(rxnVals) <- names(newVals)
          rxnVals[1, ] <- newVals

        } else {
          rxnVals <- rbind(rxnVals, CalcFractionalSoluteDynams$new(boundary = self$bounds[[i]], removalMethod = self$soluteRemovalMethod, timeInterval = self$timeInterval)$rxnVals)
          }
      }

    }
    tradeDf <- data.frame(boundIdx, tradeCurrency, tradeVals, valName)
    rxnValDf <- rxnVals
    return(list(tradeDf, rxnValDf))
  }
)




