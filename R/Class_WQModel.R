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

        cellsTableList = NULL,
        boundsTableList = NULL,

        solute_transport_df = NULL,

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

            #### GENERAL

            # set duration of each time step
            self$timeInterval <- timeInterval

            # store the units table
            self$unitsTable <- unitsTable


            #### CELLS

            # Create objects for each of the different types of cells
            self$cellsTable_water_stream <- cellsTable_water_stream
            self$cellsTable_solute_stream <- cellsTable_solute_stream

            self$cellsTable_water_soil <- cellsTable_water_soil
            self$cellsTable_solute_soil <- cellsTable_solute_soil

            self$cellsTable_water_groundwater <- cellsTable_water_groundwater
            self$cellsTable_solute_groundwater <- cellsTable_solute_groundwater

            # Store these cell table objects in a list
            cellsTableList <-
              list(
                cells_water_stream = self$cellsTable_water_stream,
                cells_solute_stream = self$cellsTable_solute_stream,
                cells_water_soil = self$cellsTable_water_soil,
                cells_solute_soil = self$cellsTable_solute_soil,
                cells_water_groundwater = self$cellsTable_water_groundwater,
                cells_solute_groundwater = self$cellsTable_solute_groundwater
              )
            # Delete any tables for types of cells that are not specified
            cellTablesToKeep <- sapply(cellsTableList, function(df) !is.null(df))
            self$cellsTableList <- cellsTableList[cellTablesToKeep]

            # Error check to see if any cell specifications are duplicated
            self$errorCheckCellInputs()

            # generate the cells from the cells tables
            cells_water_stream <- self$initializeWaterCells_stream()
            names(cells_water_stream) <- self$cellsTable_water_stream$cellIdx

            cells_solute_stream <- self$initializeSoluteCells_stream()
            names(cells_solute_stream) <- self$cellsTable_solute_stream$cellIdx

            # store all cells
            self$cells <- mget(names(self$cellsTableList))
              # c(cells_stream_water, cells_stream_solute)


            #### BOUNDARIES

            # Generate the boundaries from the three tables with boundaries info
            self$boundsTransportTable_water_int <- boundsTransportTable_water_int
            self$boundsTransportTable_water_ext <- boundsTransportTable_water_ext

            self$boundsTransportTable_solute_int <- boundsTransportTable_solute_int
            self$boundsTransportTable_solute_ext <- boundsTransportTable_solute_ext

            self$boundsReactionTable_solute_int <- boundsReactionTable_solute_int

            self$boundsTableList <-
              list(
                bounds_transport_water_ext = self$boundsTransportTable_water_int,
                bounds_transport_water_ext = self$boundsTransportTable_water_ext,
                bounds_transport_solute_int = self$boundsTransportTable_solute_int,
                bounds_transport_solute_ext = self$boundsTransportTable_solute_ext,
                bounds_reaction_solute_int = self$boundsReactionTable_solute_int
              )

            self$solute_transport_df <-
              rbind(
                self$boundsTableList[["bounds_transport_solute_int"]],
                self$boundsTableList[["bounds_transport_solute_us"]],
                self$boundsTableList[["bounds_transport_solute_ds"]]
              )

            # Run a few checks on the boundary inputs
            self$errorCheckBoundaryInputs()


            # First, create the water transport boundaries, followed by the solute transport boundaries
            bounds_transport_water_ext <- self$initializeExternalWaterTransportBoundaries()
            names(bounds_transport_water_ext) <- self$boundsTableList[["bounds_transport_water_ext"]]

            bounds_transport_water_int <- self$initializeInternalWaterTransportBoundaries()
            names(bounds_transport_water_int) <- self$boundsTableList[["bounds_transport_water_int"]]

            bounds_transport_solute <- self$initializeSoluteTransportBoundaries()
            names(bounds_transport_solute) <- solute_transport_df$boundaryIdx






            # Create solute reaction boundaries
            bounds_react_solute <- self$initializeSoluteReactionBoundaries()
            names(bounds_react_solute) <- self$boundsTableList[["bounds_react_solute"]]$boundaryIdx


            # Add the solute transport and solute reaction boundaries to the bounds list
            self$bounds <- c(self$bounds, bounds_transport_solute, bounds_react_solute)


            # populate dependencies
            lapply(self$bounds, function(b) Boundary$public_methods$populateDependencies(b) )

            # initialize store info
            # self$storeData <- self$initializeStores()

          } # closes initialize function
      ) # closes public list
  ) # closes WQ model



#' @method errorCheckCellInputs
#'
#' @description Error check to see if any cell specifications are duplicated
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "errorCheckCellInputs",
  value =
    function(){
      lapply(
        self$cellsTableList,
        function(t) {
          # A series of error checks:
          if( any( duplicated(t) ) ){
            stop("At least one specification for a cell is duplicated in one of the cell tables.")
          }
          if(length(t$cellIdx) != length(unique(t$cellIdx))) {
            stop("A cell name was duplicated in one of the cell tables.  All cell names must be unique.")
          }
        }
      ) # close lapply
    } # close function
)



#' @method initializeWaterCells_stream
#'
#' @description Instantiate the water cells in the stream process domain
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeWaterCells_stream",
  value =
    function(){
      if("cells_water_stream" %in% names(self$cellsTableList)){
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
      }
    }
)

#' @method initializeSoluteCells_stream
#'
#' @description Instantiate the solute cells in the stream process domain
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeSoluteCells_stream",
  value =
    function(){
      if("cells_solute_stream" %in% names(self$cellsTableList)){
          plyr::llply(
            1:nrow(self$cellsTable_solute_stream),
            function(rowNum){
              Cell_Solute_Stream$new(
                cellIdx = self$cellsTable_solute_stream$cellIdx[rowNum],
                processDomain = self$cellsTable_solute_stream$processDomain[rowNum],
                currency = self$cellsTable_solute_stream$currency[rowNum],

                concentration = self$cellsTable_solute_stream$concentration[rowNum],
                linkedCell = cells_water_stream[[ self$cellsTable_solute_stream$linkedCell[rowNum] ]]
              )
            }
          )
      }
    }
)

#' @method linkSoluteCellsToWaterCells_stream
#'
#' @description Link the solute cells to the water cells in the stream process domain
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "linkSoluteCellsToWaterCells_stream",
  value =
    function(cells_water_stream = cells_water_stream){
      lapply(
        cells_water_stream,
        function(c) {
          # identify the water cells to which the solute cells are connected
          cellIdxs <- self$cellsTable_solute_stream$cellIdx[self$cellsTable_solute_stream$linkedCell == c$cellIdx]
          theCells <- cells_solute_stream[ names(cells_solute_stream) == cellIdxs]
          names(theCells) <- cellIdxs
          # link the solute cells to the water cells
          c$linkedSoluteCells(theCells)
        }
      )
    }
)


#' @method errorCheckBoundaryInputs
#'
#' @description Error check to see if any boundary specifications are duplicated
#'   or have references to cells that do not exist.
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "errorCheckBoundaryInputs",
  value =
    function(){
      llply(
        1:length(self$boundsTableList),
        function(i) {
          tblName <- names(self$boundsTableList)[i]
          t <- self$boundsTableList[[i]]
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
    }
)

#' @method initializeExternalWaterTransportBoundaries
#'
#' @description Instantiate the transport boundaries that are on the physical edges of the model
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeExternalWaterTransportBoundaries",
  value =
    function(){
      plyr::llply(
        1:nrow(self$boundsTableList[["bounds_transport_water_ext"]]),
        function(rowNum) {

          locationOfBoundInNetwork <- self$boundsTableList[["bounds_transport_water_ext"]]$locationOfBoundInNetwork[rowNum]

          if(!(locationOfBoundInNetwork %in% c("upstream", "downstream"))) stop("External model boundaries must have a 'locationOfBoundInNetwork' with a value of either 'upstream' or 'downstream'.")
          if(locationOfBoundInNetwork == "upstream") {
            upstreamCell <- NA
            downstreamCell <- self$boundsTableList[["bounds_transport_water_ext"]]$cellIdx[rowNum]
          }
          if(locationOfBoundInNetwork == "downstream") {
            upstreamCell <- self$boundsTableList[["bounds_transport_water_ext"]]$cellIdx[rowNum]
            downstreamCell <- NA
          }

          Boundary$new(
            boundaryIdx = self$boundsTableList[["bounds_transport_water_ext"]]$boundaryIdx[rowNum],
            currency = self$boundsTableList[["bounds_transport_water_ext"]]$currency[rowNum],
            boundarySuperClass = self$boundsTableList[["bounds_transport_water_ext"]]$boundarySuperClass[rowNum],
            upstreamCell = self$cells[[upstreamCell]],
            downstreamCell = self$cells[[downstreamCell]],
            discharge = self$boundsTableList[["bounds_transport_water_ext"]]$discharge[rowNum]
          )

        }
      )
    }
)


#' @method initializeInternalWaterTransportBoundaries
#'
#' @description Instantiate the transport boundaries that are internal to the physical edges of the model
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeInternalWaterTransportBoundaries",
  value =
    function(){
      plyr::llply(
        1:nrow(self$boundsTableList[["bounds_transport_water_int"]]),
        function(rowNum) {
          Boundary$new(
            boundaryIdx = self$boundsTableList[["bounds_transport_water_int"]]$boundaryIdx[rowNum],
            currency = self$boundsTableList[["bounds_transport_water_int"]]$currency[rowNum],
            boundarySuperClass = self$boundsTableList[["bounds_transport_water_int"]]$boundarySuperClass[rowNum],
            upstreamCell = self$cells[[  self$boundsTableList[["bounds_transport_water_int"]]$upstreamCell[rowNum] ]],
            downstreamCell = self$cells[[ self$boundsTableList[["bounds_transport_water_int"]]$downstreamCell[rowNum] ]],
            discharge = self$boundsTableList[["bounds_transport_water_int"]]$discharge[rowNum]
          )
        }
      )
    }
)


#' @method initializeSoluteTransportBoundaries
#'
#' @description Instantiate the solute transport boundaries
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeSoluteTransportBoundaries",
  value =
    function(){
      plyr::llply(
        1:nrow(self$solute_transport_df),
        function(rowNum) {
          Boundary$new(
            boundaryIdx = self$solute_transport_df$boundaryIdx[rowNum],
            currency = self$solute_transport_df$currency[rowNum],
            boundarySuperClass = self$solute_transport_df$boundarySuperClass[rowNum],
            linkedBound = c(boundsTransportTable_water_ext, bounds_transport_water_int)[[ self$solute_transport_df$linkedBound[rowNum] ]],
            concentration = self$solute_transport_df$concentration[rowNum],
            load = self$solute_transport_df$load[rowNum]
          )
        }
      )
      # add the  u/s and d/s cells from the linked cells table
      plyr::llply(
        1:nrow(self$solute_transport_df),
        function(b) {
          b$upstreamCell <- b$linkedBound$upstreamCell$linkedSoluteCell[[b$linkedBound$upstreamCell$linkedSoluteCell$currency == b$currency]]
          b$downstreamCell <- b$linkedBound$downstreamCell$linkedSoluteCell[[b$linkedBound$downstreamCell$linkedSoluteCell$currency == b$currency]]
        }
      )
    }
)

#' @method initializeSoluteReactionBoundaries
#'
#' @description Instantiate the solute reaction boundaries
#'
#' @export
#'
WQModel$set(
  which = "public",
  name = "initializeSoluteReactionBoundaries",
  value =
    function(){
      plyr::llply(
        1:nrow(self$boundsTableList[["bounds_react_solute"]]),
        function(rowNum) {
          Boundary$new(
            boundaryIdx = boundsTable$boundaryIdx[rowNum],
            currency = boundsTable$currency[rowNum],
            boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
            upstreamCell = self$cells[[ boundsTable$upstreamCell[rowNum] ]],
            pcntToRemove = boundsTable$pcntToRemove[rowNum],
            qStorage = boundsTable$qStorage[rowNum],
            alpha = boundsTable$alpha[rowNum],
            tauMin = boundsTable$tauMin[rowNum],
            tauMax = boundsTable$tauMax[rowNum],
            k = boundsTable$k[rowNum]
          )

        }
      )
    }
)
