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
        boundsTransportTable_water = NULL,
        boundsTransportTable_solute = NULL,
        boundsReactionTable_solute = NULL,
        cellsTable_water = NULL,
        cellsTable_solute = NULL,
        unitsTable = NULL,
        cells = NULL,
        bounds = NULL,
        soluteRemovalMethod = NULL,
        timeInterval = NULL,
        storeData = NULL,

        initialize =
          function(boundsTransportTable_water,
                   boundsTransportTable_solute,
                   boundsReactionTable_solute,
                   cellsTable_water,
                   cellsTable_solute,
                   unitsTable,
                   soluteRemovalMethod,
                   k,
                   timeInterval,
                   ...) {
            # set duration of each time step
            self$timeInterval <- timeInterval

            self$cellsTable_water <- cellsTable_water
            self$cellsTable_solute <- cellsTable_solute

            lapply(c(self$cellsTable_water, self$cellsTable_solute), function(t) {
              # A series of error checks:
              if( any( duplicated(t) ) ){
                stop("At least one specification for a cell is duplicated in one of the cell tables.")
              }
              if(length(t$cellIdx) != length(unique(t$cellIdx))) {
                stop("A cell name was duplicated in one of the cell tables.  All cell names must be unique.")
              }
            }
            )

            self$unitsTable <- unitsTable

            self$soluteRemovalMethod <- soluteRemovalMethod

            # generate the stream cells from the cells tables

            # water cells
            cells_stream_water <-
              plyr::llply(
                1:nrow(self$cellsTable_water),
                function(rowNum){
                  Cell_Water_Stream$new(
                    cellIdx = self$cellsTable_water$cellIdx[rowNum],

                    processDomain = self$cellsTable_water$processDomain[rowNum],

                    channelWidth = self$cellsTable_water$channelWidth[rowNum],
                    channelLength = self$cellsTable_water$channelLength[rowNum],
                    channelDepth = self$cellsTable_water$channelDepth[rowNum]
                  )
                }
              )
            names(cells_stream_water) <- self$cellsTable_water$cellIdx

            # solute cells
            cells_stream_solute <-
              plyr::llply(
                1:nrow(self$cellsTable_solute),
                function(rowNum){
                  Cell_Water_Stream$new(
                    cellIdx = self$cellsTable_solute$cellIdx[rowNum],
                    processDomain = self$cellsTable_solute$processDomain[rowNum],
                    currency = self$cellsTable_solute$currency[rowNum],

                    concentration = self$cellsTable_solute$concentration[rowNum],
                    linkedCell = self$cellsTable_solute$linkedCell[rowNum]
                  )
                }
              )
            names(cells_stream_solute) <- self$cellsTable_solute$cellIdx

            # store all cells
            self$cells <- c(cells_stream_water, cells_stream_solute)


            # Generate the boundaries from the three tables with boundaries info
            self$boundsTransportTable_water <- boundsTransportTable_water
            self$boundsTransportTable_solute <- boundsTransportTable_solute
            self$boundsReactionTable_solute <- boundsReactionTable_solute

            boundsTables <- list(bounds_stream_water_transport = self$boundsTransportTable_water,
                                 bounds_stream_solute_transport = self$boundsTransportTable_solute,
                                 bounds_stream_solute_reaction = self$boundsReactionTable_solute)

            # A series of error checks:
            llply(
              1:length(boundsTables),
              function(i) {
                tblName <- names(boundsTables)[i]
                t <- boundsTables[[i]]
                if( any( duplicated(t$boundsTable) ) ){
                  stop(paste0("At least one specification for a boundary is duplicated in the table", tblName, "."))
                }
                if(length(t$boundaryIdx) != length(unique(t$boundaryIdx))) {
                  stop(paste0("A boundary name was duplicated in the table:", tblName, ".  All boundary names must be unique."))
                }
                if( any(!(unique(t$downstreamCellIdx[!is.na(t$downstreamCellIdx)]) %in% unique(sapply(t$cells, function(cell) cell$cellIdx)))) ){
                  stop(paste0("In the table", tblName, ", a name of a downstream cell was provided that refers to a cell that has not been instantiated."))
                }
                if( any(!(unique(t$upstreamCellIdx[!is.na(t$upstreamCellIdx)]) %in% unique(sapply(t$cells, function(cell) cell$cellIdx)))) ){
                  stop(paste0("In the table", tblName, ", a name of a upstream cell was provided that refers to a cell that has not been instantiated."))
                }
              }
            )

            # First, create the water transport boundaries
            self$bounds <-
              plyr::llply(
                1:nrow(boundsTables[["bounds_transport_water"]]),
                function(rowNum) {

                  usCrit <- boundsTables[["bounds_transport_water"]]$upstreamCellIdx[rowNum]
                  dsCrit <- boundsTables[["bounds_transport_water"]]$downstreamCellIdx[rowNum]

                  if(is.na(usCrit)){
                    upstreamCell <- NA
                  } else{
                    upstreamCell <- self$cells[[ which(names(self$cells) ==  usCrit) ]]
                  }

                  if(is.na(dsCrit)){
                    downstreamCell <- NA
                  } else{
                    downstreamCell <- self$cells[[ which(names(self$cells) ==  dsCrit) ]]
                  }

                  Boundary$new(
                    boundaryIdx = boundsTable$boundaryIdx[rowNum],
                    currency = boundsTable$currency[rowNum],
                    boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                    upstreamCell = upstreamCell,
                    downstreamCell = downstreamCell,
                    discharge = boundsTable$discharge[rowNum]
                  )

                }
              )
            names(self$bounds) <- boundsTable$boundaryIdx

            # Solute transport boundaries
            bounds_transport_solute <-
              plyr::llply(
                1:nrow(boundsTables[["bounds_transport_solute"]]),
                function(rowNum) {
                  Boundary$new(
                    boundaryIdx = boundsTable$boundaryIdx[rowNum],
                    currency = boundsTable$currency[rowNum],
                    boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                    linkedBound = boundsTable$linkedBound[rowNum],
                    concentration = boundsTable$concentration,
                    load = boundsTable$load
                  )

                }
              )
            names(bounds_transport_solute) <- boundsTables[["bounds_transport_solute"]]$boundaryIdx
            # add the  u/s and d/s cells from the linked cells table
            plyr::llply(
              1:nrow(bounds_transport_solute),
              function(b) {
                b$upstreamCell <- b$linkedBound$upstreamCell
                b$downstreamCell <- b$linkedBound$downstreamCell
              }
            )

            # Create solute reaction boundaries
            bounds_react_solute <-
              plyr::llply(
                1:nrow(boundsTables[["bounds_react_solute"]]),
                function(rowNum) {
                  Boundary$new(
                    boundaryIdx = boundsTable$boundaryIdx[rowNum],
                    currency = boundsTable$currency[rowNum],
                    boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                    upstreamCell = boundsTable$upstreamCell[rowNum],
                    pcntToRemove = boundsTable$pcntToRemove[rowNum],
                    qStorage = boundsTable$qStorage[rowNum],
                    alpha = boundsTable$alpha[rowNum],
                    tauMin = boundsTable$tauMin[rowNum],
                    tauMax = boundsTable$tauMax[rowNum],
                    k = boundsTable$k[rowNum]
                  )

                }
              )
            names(bounds_transport_solute) <- boundsTables[["bounds_transport_solute"]]$boundaryIdx


            # Add the solute transport and solute reaction boundaries to the bounds list
            self$bounds <- c(self$bounds, bounds_transport_solute, bounds_react_solute)

























            # bounds with links...link 'em up...
            linkedBounds <- self$bounds[ unlist(plyr::llply(self$bounds, function(bound) !is.na( bound$linkedTo))) ]
            for(bound in linkedBounds){
              bound$linkedTo <- self$bounds[[which(sapply(self$bounds, function(b) b$boundaryIdx) == bound$linkedTo)]]
            }

            # populate dependencies
            lapply(self$bounds, function(b) Boundary$public_methods$populateDependencies(b) )

            # initialize store info
            self$storeData <- self$initializeStores()

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
    boundIdx <- rep(NA, length(self$bounds))
    tradeVals <- rep(NA, length(self$bounds))
    tradeCurrency <- rep(NA, length(self$bounds))
    rxnVals <- NULL
    valName <- rep(NA, length(self$bounds))
    upstreamCell <- as.list(rep(NA, length(self$bounds)))
    downstreamCell <- as.list(rep(NA, length(self$bounds)))
    upstreamCellIdx <- rep(NA, length(self$bounds))
    downstreamCellIdx <- rep(NA, length(self$bounds))
    tradeType <- rep(NA, length(self$bounds))
    valIdx <- rep(NA, length(self$bounds))
    trades <- list()

    # print(length(self$bounds))

    for(i in 1:length(self$bounds) ){

      tradeCurrency[i] <- self$bounds[[i]]$currency
      boundIdx[i] <- self$bounds[[i]]$boundaryIdx
      if(! self$bounds[[i]]$usModBound){
        upstreamCell[[i]] <- self$bounds[[i]]$upstreamCell
        upstreamCellIdx[i] <- upstreamCell[[i]]$cellIdx

      }
      # else{
      #   upstreamCellIdx[i] <- NA
      # }
      if(! self$bounds[[i]]$dsModBound) {
        downstreamCell[[i]] <- self$bounds[[i]]$downstreamCell
        downstreamCellIdx[i]  <- downstreamCell[[i]]$cellIdx
      }
      # else {
      #   downstreamCellIdx[i] <- NA
      # }

      if(self$bounds[[i]]$currency == "H2O" & self$bounds[[i]]$boundarySuperClass == "transport"){
        newCalc <- WaterTransportPerTime$new(self$bounds[[i]], self$timeInterval)
        trades[[i]]<- newCalc
        tradeVals[i] <- newCalc$volumeToTrade
        valIdx[i] <- "volumeToTrade"
        valName[i] <- "water volume  (L)"
        tradeType[i] <- newCalc$tradeType
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "transport"){
        newCalc <- SoluteTransportPerTime$new(self$bounds[[i]], self$timeInterval)
        trades[[i]] <- newCalc
        tradeVals[i] <- newCalc$soluteToTrade
        valIdx[i] <- "soluteToTrade"
        valName[i] <- "solute mass (ug)"
        tradeType[i] <- newCalc$tradeType
      }

      if(self$bounds[[i]]$currency == "NO3" & self$bounds[[i]]$boundarySuperClass == "reaction"){
        newCalc <- CalcFractionalSoluteDynams$new(boundary = self$bounds[[i]], removalMethod = self$soluteRemovalMethod, timeInterval = self$timeInterval)
        trades[[i]] <- newCalc
        tradeVals[i] <- newCalc$massToRemove
        valIdx[i] <- "massToRemove"
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

    } # close for loop

    # tradeList <- list(trades = I(trades), boundIdx = boundIdx, tradeCurrency = tradeCurrency, tradeVals = tradeVals, valIdx = valIdx, valName = valName, upstreamCellIdx = upstreamCellIdx, downstreamCellIdx = downstreamCellIdx, tradeType = tradeType, upstreamCell = I(upstreamCell), downstreamCell = I(downstreamCell))
    tradeDf <- data.frame(trades = I(trades), boundIdx, tradeCurrency, tradeVals, valIdx, valName, upstreamCellIdx, downstreamCellIdx, tradeType, upstreamCell = I(upstreamCell), downstreamCell = I(downstreamCell))
    class(tradeDf$trades) <- "list"
    class(tradeDf$upstreamCell) <- "list"
    class(tradeDf$downstreamCell) <- "list"
    rxnValDf <- rxnVals
    return(list(tradeDf, rxnValDf))
  }
)

#' @method WQModel$initializeStores
#'
#' @description Structured info for storing all the stores for the model
#'
WQModel$set(
  which = "public",
  name = "initializeStores",
  value = function(...){

    return(CalcStores$new(cells = self$cells, bounds = self$bounds))
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
    tradeTable <- self$trade()[[1]]
    # initStores <- self$storeData

    # return( CalcStores$public_methods$doCalc( tradeTable = tradeTable, initStores = initStores ) )
    return( self$storeData$doCalc( tradeTable = tradeTable ) )

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
    tradeTable <- self$trade()[[1]]
    timeInterval <- self$timeInterval

    return( c(UpdateCells$new(cells), UpdateBounds$new(tradeDf = tradeTable, timeInterval = timeInterval)) )
  }
)

