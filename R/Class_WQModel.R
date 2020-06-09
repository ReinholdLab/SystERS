#' @title Class WQModel (R6)
#'
#' @description Define the WQ model and the network topology of cells and
#'   boundaries
#'
#' @export
#'
#' @return The ojbect of class \code{WQModel}.  Boundaries and cells are stored
#'   in lists called \code{bounds} and \code{cells}, respectively.  **Importantly,
#'   as of 06/07/2020, the subsequent calculations done by the model will only
#'   return correct trade values for topologies that are linear or have convergent
#'   flow.  Calculations will be wrong for models with divergent transport
#'   boundaries.***
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
            self$timeInterval <- timeInterval

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

                    channelWidth = cellsTable$channelWidth[rowNum],
                    channelLength = cellsTable$channelLength[rowNum],
                    channelDepth = cellsTable$channelDepth[rowNum],

                    alpha = cellsTable$alpha[rowNum],
                    tauMin = cellsTable$tauMin[rowNum],
                    tauMax = cellsTable$tauMax[rowNum]
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
            if( any(!(unique(boundsTable$downstreamCellIdx[!is.na(boundsTable$downstreamCellIdx)]) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
              stop("In the boundsTable, a name of a downstream cell was provided that refers to a cell that has not been instantiated.")
            }
            if( any(!(unique(boundsTable$upstreamCellIdx[!is.na(boundsTable$upstreamCellIdx)]) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
              stop("In the boundsTable, a name of an upstream cell was provided that refers to a cell that has not been instantiated.")
            }


            self$bounds <-
              plyr::llply(
                1:nrow(boundsTable),
                function(rowNum) {

                  usCrit <- self$boundsTable$upstreamCellIdx[rowNum]
                  dsCrit <- self$boundsTable$downstreamCellIdx[rowNum]

                  if(is.na(usCrit)){
                    usCell <- NA
                  } else{
                    usCell <- self$cells[[ which(names(self$cells) ==  usCrit) ]]
                  }

                  if(is.na(dsCrit)){
                    dsCell <- NA
                  } else{
                    dsCell <- self$cells[[ which(names(self$cells) ==  dsCrit) ]]
                  }

                  # print(paste("bound:", boundsTable$boundaryIdx[rowNum] , ";   u/s cell:", usCellIdx, ";   d/s cell:", dsCellIdx, sep = ""))

                    Boundary$new(
                      boundaryIdx = boundsTable$boundaryIdx[rowNum],
                      currency = boundsTable$currency[rowNum],
                      boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                      upstreamCell = usCell,
                        # self$cells[[ usCellIdx ]],
                      downstreamCell = dsCell,
                        # self$cells[[ dsCellIdx ]],
                      calculateOrder = boundsTable$calculateOrder[rowNum],
                      pcntToRemove = boundsTable$pcntToRemove[rowNum],
                      discharge = boundsTable$discharge[rowNum],
                      k = self$k,
                      soluteLoad = boundsTable$soluteLoad[rowNum],
                      qStorage = boundsTable$qStorage[rowNum],
                      linkedTo = boundsTable$linkedTo[rowNum]
                    )

                }
              )
            names(self$bounds) <- boundsTable$boundaryIdx

            # bounds with links...link 'em up...
            linkedBounds <- self$bounds[ unlist(plyr::llply(self$bounds, function(bound) !is.na( bound$linkedTo))) ]
            for(bound in linkedBounds){
              bound$linkedTo <- self$bounds[[which(sapply(self$bounds, function(b) b$boundaryIdx) == bound$linkedTo)]]
            }

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
    usCellIdx <- NA
    dsCellIdx <- NA
    tradeType <- NA

    for(i in 1:length(self$bounds) ){

      tradeCurrency[i] <- self$bounds[[i]]$currency
      boundIdx[i] <- self$bounds[[i]]$boundaryIdx
      if(! self$bounds[[i]]$usModBound){
        usCellIdx[i] <- self$bounds[[i]]$upstreamCell$cellIdx
      } else{
        usCellIdx[i] <- NA
      }
      if(! self$bounds[[i]]$dsModBound) {
        dsCellIdx[i]  <- self$bounds[[i]]$downstreamCell$cellIdx
      } else {
        dsCellIdx[i] <- NA
      }

      if(self$bounds[[i]]$currency == "H2O" & self$bounds[[i]]$boundarySuperClass == "transport"){
        newCalc <- WaterTransportPerTime$new(self$bounds[[i]], self$timeInterval)
        tradeVals[i] <- newCalc$volumeToTrade
        valName[i] <- "water volume  (L)"
        tradeType[i] <- newCalc$tradeType
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "transport"){
        newCalc <- SoluteTransportPerTime$new(self$bounds[[i]], self$timeInterval)
        tradeVals[i] <- newCalc$soluteToTrade
        valName[i] <- "solute mass (ug)"
        tradeType[i] <- newCalc$tradeType
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "reaction"){
        newCalc <- CalcFractionalSoluteDynams$new(boundary = self$bounds[[i]], removalMethod = self$soluteRemovalMethod, timeInterval = self$timeInterval)
        tradeVals[i] <- newCalc$massToRemove
        valName[i] <- "solute mass (ug)"
        tradeType[i] <- newCalc$tradeType

        # Output the key values from the reactions occuring in the timestep
        newVals <- newCalc$rxnVals
        if(is.null(rxnVals) ) {
          rxnVals <- newVals
        } else {
          rxnVals <- rbind(rxnVals, newVals)
          }
      }

    }
    tradeDf <- data.frame(boundIdx, tradeCurrency, tradeVals, valName, usCellIdx, dsCellIdx, tradeType)
    rxnValDf <- rxnVals
    return(list(tradeDf, rxnValDf))
  }
)



#' @method WQModel$store
#'
#' @description Calculates all the stores for the model
#'
WQModel$set(
  which = "public",
  name = "store",
  value = function(...){
    cells <- self$cells
    tradeTable <- self$trade()[[1]]
    return(CalcStores$new(cells, tradeTable)$stores)
  }
)

#' @method WQModel$update
#'
#' @description Does all the updates for the model
#'
WQModel$set(
  which = "public",
  name = "update",
  value = function(...){
    cells <- self$cells
    storesList <- self$store()
    return(UpdateCells$new(cells, storesList))
  }
)

