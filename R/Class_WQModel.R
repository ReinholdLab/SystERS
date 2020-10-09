#' @title Class WQModel (R6)
#'
#' @description Define the WQ model and the network topology of cells and
#'   boundaries
#'
#' @export
#'
#' @return The ojbect of class \code{WQModel}.  Boundaries and cells are stored
#'   in lists called \code{bounds} and \code{cells}, respectively.  #'
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
        boundsTransportTable_water_int = NULL,
        boundsTransportTable_water_ext = NULL,

        boundsTransportTable_solute_int = NULL,
        boundsTransportTable_solute_ext = NULL,

        boundsReactionTable_solute_int = NULL,

        cellsTable_water_stream = NULL,
        cellsTable_solute_stream = NULL,

        cellsTable_water_soil = NULL,
        cellsTable_solute_soil = NULL,

        cellsTable_water_groundwater = NULL,
        cellsTable_solute_groundwater = NULL,

        unitsTable = NULL,

        cells = NULL,
        bounds = NULL,
        soluteRemovalMethod = NULL,
        timeInterval = NULL,
        storeData = NULL,

        initialize =
          function(
            boundsTransportTable_water_int,
            boundsTransportTable_water_ext,
            boundsTransportTable_solute_int,
            boundsTransportTable_solute_ext,
            boundsReactionTable_solute_int,
            cellsTable_water_stream = NULL,
            cellsTable_solute_stream = NULL,
            cellsTable_water_soil = NULL,
            cellsTable_solute_soil = NULL,
            cellsTable_water_groundwater = NULL,
            cellsTable_solute_groundwater = NULL,
            unitsTable,
            soluteRemovalMethod,
            timeInterval,
            ...
            ) {
            # set duration of each time step
            self$timeInterval <- timeInterval

            # store the units table
            self$unitsTable <- unitsTable

            # Create objects for each of the different types of cells
            self$cellsTable_water_stream <- cellsTable_water_stream
            self$cellsTable_solute_stream <- cellsTable_solute_stream

            self$cellsTable_water_soil <- cellsTable_water_soil
            self$cellsTable_solute_soil <- cellsTable_solute_soil

            self$cellsTable_water_groundwater <- cellsTable_water_groundwater
            self$cellsTable_solute_groundwater <- cellsTable_solute_groundwater

            # Store these cell table objects in a list
            cellTableList <-
              list(
                cells_water_stream = self$cellsTable_water_stream,
                cells_solute_stream = self$cellsTable_solute_stream,
                cells_water_soil = self$cellsTable_water_soil,
                cells_solute_soil = self$cellsTable_solute_soil,
                cells_water_groundwater = self$cellsTable_water_groundwater,
                cells_solute_groundwater = self$cellsTable_solute_groundwater
              )
            # Delete any tables for types of cells that are not specified
            cellTablesToKeep <- sapply(cellTableList, function(df) !is.null(df))
            cellTableList <- cellTableList[cellTablesToKeep]

            # Error check to see if any cell specifications are duplicated
            lapply(
              cellTableList,
              function(t) {
                # A series of error checks:
                if( any( duplicated(t) ) ){
                  stop("At least one specification for a cell is duplicated in one of the cell tables.")
                }
                if(length(t$cellIdx) != length(unique(t$cellIdx))) {
                  stop("A cell name was duplicated in one of the cell tables.  All cell names must be unique.")
                }
              }
            )

            # generate the cells from the cells tables

            # water cells
            if("cells_water_stream" %in% names(cellsTableList)){
              cells_water_stream <-
                plyr::llply(
                  1:nrow(self$cellsTable_water_stream),
                  function(rowNum){
                    Cell_Water_Stream$new(
                      cellIdx = self$cellsTable_water_stream$cellIdx[rowNum],
                      currency = self$cellsTable_water_stream$currency[rowNum],
                      processDomain = self$cellsTable_water_stream$processDomain[rowNum],

                      channelWidth = self$cellsTable_water_stream$channelWidth[rowNum],
                      channelLength = self$cellsTable_water_stream$channelLength[rowNum],
                      channelDepth = self$cellsTable_water_stream$channelDepth[rowNum]
                    )
                  }
                )
              names(cells_water_stream) <- self$cellsTable_water_stream$cellIdx
            }

            # solute cells
            if("cells_solute_stream" %in% names(cellsTableList)){
              cells_solute_stream <-
                plyr::llply(
                  1:nrow(self$cellsTable_solute_stream),
                  function(rowNum){
                    Cell_Solute_Stream$new(
                      cellIdx = self$cellsTable_solute_stream$cellIdx[rowNum],
                      processDomain = self$cellsTable_solute_stream$processDomain[rowNum],
                      currency = self$cellsTable_solute_stream$currency[rowNum],

                      concentration = self$cellsTable_solute_stream$concentration[rowNum],
                      linkedCell = self$cellsTable_solute_stream$linkedCell[rowNum]
                    )
                  }
                )
              names(cells_solute_stream) <- self$self$cellsTable_solute_stream$cellIdx
            }

            # store all cells
            self$cells <- mget(names(cellsTableList))
              # c(cells_stream_water, cells_stream_solute)


            # Generate the boundaries from the three tables with boundaries info
            self$boundsTransportTable_water_int <- boundsTransportTable_water_int
            self$boundsTransportTable_water_ext <- boundsTransportTable_water_ext

            self$boundsTransportTable_solute_int <- boundsTransportTable_solute_int
            self$boundsTransportTable_solute_ext <- boundsTransportTable_solute_ext

            self$boundsReactionTable_solute_int <- boundsReactionTable_solute_int



            ### Make boundaries
            boundsTableList <-
              list(
                bounds_transport_water_ext = self$boundsTransportTable_water_int,
                bounds_transport_water_ext = self$boundsTransportTable_water_ext,
                bounds_transport_solute_int = self$boundsTransportTable_solute_int,
                bounds_transport_solute_ext = self$boundsTransportTable_solute_ext,
                bounds_reaction_solute_int = self$boundsReactionTable_solute_int
              )

            # A series of error checks:
            llply(
              1:length(boundsTableList),
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
            bounds_transport_water_ext <-
              plyr::llply(
                1:nrow(boundsTableList[["bounds_transport_water_ext"]]),
                function(rowNum) {

                  locationOfBoundInNetwork <- boundsTableList[["bounds_transport_water_ext"]]$locationOfBoundInNetwork[rowNum]

                  if(!(locationOfBoundInNetwork %in% c("upstream", "downstream"))) stop("External model boundaries must have a 'locationOfBoundInNetwork' with a value of either 'upstream' or 'downstream'.")
                  if(locationOfBoundInNetwork == "upstream") {
                    upstreamCell <- NA
                    downstreamCell <- boundsTableList[["bounds_transport_water_ext"]]$cellIdx[rowNum]
                  }
                  if(locationOfBoundInNetwork == "downstream") {
                    upstreamCell <- boundsTableList[["bounds_transport_water_ext"]]$cellIdx[rowNum]
                    downstreamCell <- NA
                  }

                  Boundary$new(
                    boundaryIdx = boundsTableList[["bounds_transport_water_ext"]]$boundaryIdx[rowNum],
                    currency = boundsTableList[["bounds_transport_water_ext"]]$currency[rowNum],
                    boundarySuperClass = boundsTableList[["bounds_transport_water_ext"]]$boundarySuperClass[rowNum],
                    upstreamCell = upstreamCell,
                    downstreamCell = downstreamCell,
                    discharge = boundsTableList[["bounds_transport_water_ext"]]$discharge[rowNum]
                  )

                }
              )
            names(bounds_transport_water_ext) <- boundsTableList[["bounds_transport_water_ext"]]

            bounds_transport_water_int <-
              plyr::llply(
                1:nrow(boundsTableList[["bounds_transport_water_int"]]),
                function(rowNum) {
                  Boundary$new(
                    boundaryIdx = boundsTableList[["bounds_transport_water_int"]]$boundaryIdx[rowNum],
                    currency = boundsTableList[["bounds_transport_water_int"]]$currency[rowNum],
                    boundarySuperClass = boundsTableList[["bounds_transport_water_int"]]$boundarySuperClass[rowNum],
                    upstreamCell = boundsTableList[["bounds_transport_water_int"]]$upstreamCell[rowNum],
                    downstreamCell = boundsTableList[["bounds_transport_water_int"]]$downstreamCell[rowNum],
                    discharge = boundsTableList[["bounds_transport_water_int"]]$discharge[rowNum]
                  )

                }
              )
            names(bounds_transport_water_int) <- boundsTableList[["bounds_transport_water_int"]]

            # Solute transport boundaries
            solute_transport_df <-
              rbind(
                boundsTableList[["bounds_transport_solute_int"]],
                boundsTableList[["bounds_transport_solute_us"]],
                boundsTableList[["bounds_transport_solute_ds"]]
              )

            bounds_transport_solute <-
              plyr::llply(
                1:nrow(solute_transport_df),
                function(rowNum) {
                  Boundary$new(
                    boundaryIdx = solute_transport_df$boundaryIdx[rowNum],
                    currency = solute_transport_df$currency[rowNum],
                    boundarySuperClass = solute_transport_df$boundarySuperClass[rowNum],
                    linkedBound = solute_transport_df$linkedBound[rowNum],
                    concentration = solute_transport_df$concentration,
                    load = solute_transport_df$load
                  )

                }
              )
            names(bounds_transport_solute) <- solute_transport_df$boundaryIdx
            # add the  u/s and d/s cells from the linked cells table

            #################  AM review this code here and below (10/09/2020)
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
            names(bounds_react_solute) <- boundsTables[["bounds_react_solute"]]$boundaryIdx


            # Add the solute transport and solute reaction boundaries to the bounds list
            self$bounds <- c(self$bounds, bounds_transport_solute, bounds_react_solute)




###### AMR: Stopped here 1:18 PM 09/14/2020






















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

