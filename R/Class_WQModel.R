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
        k = NULL,

        initialize =
          function(modelName, boundsTable, cellsTable, unitsTable, soluteRemovalMethod, k, ...) {
            self$modelName <- modelName
            self$cellsTable <- cellsTable

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
            names(self$cells) <- cellsTable$cellIdx

            print(sapply(self$cells, function(cell) cell$cellIdx))

            # Generate the boundaries from the boundsTable in the model

            # First, order the bounds table by the calculate order column. This
            # ordering makes it so that you can call the boundaries in the the
            # correct order simply by looping through the list of bounds.
            boundsTable <- boundsTable[order(boundsTable$calculateOrder), ]
            self$boundsTable <- boundsTable

            print(boundsTable)
            print(paste("All boundaries unique:", length(boundsTable$boundaryIdx) == length(unique(boundsTable$boundaryIdx))))
            print(
              paste(
                "Any d/s cell Idxs NOT listed as a cellIdx:", any(!(unique(boundsTable$downstreamCellIdx) %in% unique(sapply(self$cells, function(cell) cell$cellIdx))))
              )
            )
            print(
              paste(
                "Any u/s cell Idxs NOT listed as a cellIdx:", any(!(unique(boundsTable$upstreamCellIdx) %in% unique(sapply(self$cells, function(cell) cell$cellIdx))))
              )
            )

            self$bounds <-
              plyr::llply(
                1:nrow(boundsTable),
                # 1:4,
                function(rowNum) {

                  usCellIdx <- self$cells[[ which(names(self$cells) ==  self$boundsTable$upstreamCellIdx[rowNum]) ]]$cellIdx
                  dsCellIdx <- self$cells[[ which(names(self$cells) ==  self$boundsTable$downstreamCellIdx[rowNum]) ]]$cellIdx

                  print(paste("bound:", boundsTable$boundaryIdx[rowNum] , ";   u/s cell:", usCellIdx, ";   d/s cell:", dsCellIdx, sep = " "))

                    Boundary$new(
                      boundaryIdx = boundsTable$boundaryIdx[rowNum],
                      currency = boundsTable$currency[rowNum],
                      boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                      upstreamCell = self$cells[[ usCellIdx ]],
                      downstreamCell = self$cells[[ dsCellIdx ]],
                      calculateOrder = boundsTable$calculateOrder[rowNum]
                    )
                    print(paste("Boundary", boundsTable$boundaryIdx[rowNum], "successfully instantiated."))

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

    for(i in 1:4
        # length(self$bounds)
        ){

      bound <- self$bounds[[i]]

      tradeCurrency[i] <- bound$currency
      boundIdx[i] <- bound$boundaryIdx

      modelEnvName <- self$modelName

      if(bound$currency == "H2O" & bound$boundarySuperClass == "transport"){
        tradeVals[i] <-
          WaterTransportPerTime$new(modelEnvName,
                                    bound$boundaryIdx,
                                    bound$upstreamCellIdx)$dischargeToTrade
      }
      if(bound$currency == "NO3" & bound$boundarySuperClass == "transport"){
        tradeVals[i] <-
          SoluteTransportPerTime$new(modelEnvName,
                                     bound$boundaryIdx,
                                     bound$upstreamCellIdx)$soluteToTrade
      }


      # if(bound$currency == "NO3" & bound$boundarySuperClass == "reaction"){
      #   tradeVals[i] <- CalcFractionalSoluteDynams$new(modelEnv,
      #                                                  bound$boundaryIdx,
      #                                                  bound$upstreamCellIdx,
      #                                                  self$soluteRemovalMethod)
      # }

    }

    # rm(bound)


    return(data.frame(boundIdx, tradeCurrency, tradeVals))
  }
)




