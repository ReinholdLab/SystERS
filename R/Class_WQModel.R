#' @title Class WQModel (R6) A water quality model
#' @description Define and instantiate the WQ model and the network topology of
#'   cells and boundaries
#' @export
#'
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
        #' @field cells The model cells, stored in a list
        cells = NULL,
        #' @field bounds The model boundaries, stored in a list
        bounds = NULL,

        timeInterval = NULL,

        boundsTransportTable_water_int = NULL,
        boundsTransportTable_water_ext = NULL,

        boundsTransportTable_solute_int = NULL,
        boundsTransportTable_solute_us = NULL,
        boundsTransportTable_solute_ds = NULL,

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

        #' @param boundsTransportTable_water_int Table with the names of the
        #'   water boundaries and their attributes for water boundaries that are
        #'   internal to the model, i.e., those with exactly one upstream and
        #'   one downstream cell
        #' @param boundsTransportTable_water_ext Table with the names of the
        #'   water boundaries and their attributes for water boundaries at the
        #'   most upstream and most downstream extent of the model topology
        #' @param boundsTransportTable_solute_int Table with the names of the
        #'   solute boundaries and their attributes for solute boundaries that
        #'   are internal to the model, i.e., those with exactly one upstream
        #'   and one downstream cell
        #' @param boundsTransportTable_solute_us Table with the names of the
        #'   solute boundaries and their attributes for solute boundaries at the
        #'   most upstream extent of the model topology
        #' @param boundsTransportTable_solute_ds Table with the names of the
        #'   solute boundaries and their attributes for solute boundaries at the
        #'   most downstream extent of the model topology
        #' @param boundsReactionTable_solute_int Table with the names of the
        #'   solute reaction boundaries and their attributes
        #' @param cellsTable_water_stream Table with the names of the water
        #'   cells and their attributes in the stream processing domain
        #' @param cellsTable_solute_stream Table with the names of the solute
        #'   cells and their attributes in the stream processing domain
        #' @param cellsTable_water_soil Table with the names of the water
        #'   cells and their attributes in the soil processing domain
        #' @param cellsTable_solute_soil Table with the names of the solute
        #'   cells and their attributes in the soil processing domain
        #' @param cellsTable_water_groundwater Table with the names of the water
        #'   cells and their attributes in the groundwater processing domain
        #' @param cellsTable_solute_groundwater Table with the names of the solute
        #'   cells and their attributes in the groundwater processing domain
        #' @param unitsTable Table of units for model parameters and outputs
        #' @param timeInterval Model time step
        #' @return The ojbect of class \code{WQModel}.  Boundaries and cells are
        #'   stored in lists called \code{bounds} and \code{cells},
        #'   respectively.
        initialize =
          function(
            boundsTransportTable_water_int,
            boundsTransportTable_water_ext,
            boundsTransportTable_solute_int,
            boundsTransportTable_solute_us,
            boundsTransportTable_solute_ds,
            boundsReactionTable_solute_int,
            cellsTable_water_stream = NULL,
            cellsTable_solute_stream = NULL,
            cellsTable_water_soil = NULL,
            cellsTable_solute_soil = NULL,
            cellsTable_water_groundwater = NULL,
            cellsTable_solute_groundwater = NULL,
            unitsTable,
            timeInterval
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

            # populate dependencies
            bounds_transport_water <- c(bounds_transport_water_ext, bounds_transport_water_int)
            bounds_transport_water <- lapply(bounds_transport_water, function(b) b$populateDependencies())

            # Add the solute transport and solute reaction boundaries to the bounds list
            self$bounds <- c(bounds_transport_water, bounds_transport_solute, bounds_react_solute)

            # initialize store info
            # self$storeData <- self$initializeStores()

          }, # closes initialize method

        #' @method Method WQModel$errorCheckCellInputs
        #' @description Error check to see if any cell specifications are
        #'   duplicated
        #' @return Nothing if no error is detected.  An error message is
        #'   returned if an error is detected.
        errorCheckCellInputs = function(){
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
        }, # close method


        #' @method Method WQModel$initializeWaterCells_stream
        #' @description Instantiate the water cells in the stream process domain
        #' @return List of stream water cells
        initializeWaterCells_stream = function(){
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
            ) # close llply
          } # close if
        }, # close method

        #' @method Method WQModel$initializeSoluteCells_stream
        #' @description Instantiate the solute cells in the stream process domain
        #' @return List of stream solute cells
        initializeSoluteCells_stream = function(){
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
            ) # close llply
          } # close if
        }, # close method


        #' @method Method WQModel$linkSoluteCellsToWaterCells_stream
        #' @description Link the solute cells to the water cells in the stream process
        #'   domain
        #' @return Water cells with their \code{linkedSoluteCells} attribute populated
        #'   with a list of solute cells that are linked to the water cell
        linkSoluteCellsToWaterCells_stream =
          function(cells_water_stream = cells_water_stream){
            lapply(
              cells_water_stream,
              function(c) {
                # identify the water cells to which the solute cells are connected
                cellIdxs <- self$cellsTable_solute_stream$cellIdx[self$cellsTable_solute_stream$linkedCell == c$cellIdx]
                theCells <- cells_solute_stream[ names(cells_solute_stream) == cellIdxs]
                names(theCells) <- cellIdxs
                # link the solute cells to the water cells
                c$linkedSoluteCells <- theCells
              }
            ) # close lapply
          }, # close method


        #' @method Method WQModel$errorCheckBoundaryInputs
        #' @description Error check to see if any boundary specifications are duplicated
        #'   or have references to cells that do not exist.
        #' @return Nothing if no error is detected.  If an error is detected, a message
        #'   is thrown.
        errorCheckBoundaryInputs=
          function(){
            lapply(
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
            ) # close lapply
          }, # close method



        #' @method WQModel$initializeExternalWaterTransportBoundaries
        #' @description Instantiate the transport boundaries that are on the physical
        #'   edges of the model
        #' @return Water transport boundaries at the upstream and downstream extents of
        #'   the model topology
        #'
        initializeExternalWaterTransportBoundaries = function(){
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

              Boundary_Transport_Water$new(
                boundaryIdx = self$boundsTableList[["bounds_transport_water_ext"]]$boundaryIdx[rowNum],
                currency = self$boundsTableList[["bounds_transport_water_ext"]]$currency[rowNum],
                boundarySuperClass = self$boundsTableList[["bounds_transport_water_ext"]]$boundarySuperClass[rowNum],
                upstreamCell = self$cells[[upstreamCell]],
                downstreamCell = self$cells[[downstreamCell]],
                discharge = self$boundsTableList[["bounds_transport_water_ext"]]$discharge[rowNum]
              )

            }
          ) # close llply
        }, # close method



        #' @method WQModel$initializeInternalWaterTransportBoundaries
        #' @description Instantiate the transport boundaries that are internal to the
        #'   physical edges of the model
        #' @return Water transport boundaries within the upstream and downstream extents
        #'   of the model topology; i.e., those having exactly one upstream and one
        #'   downstream cell
        initializeInternalWaterTransportBoundaries = function(){
          plyr::llply(
            1:nrow(self$boundsTableList[["bounds_transport_water_int"]]),
            function(rowNum) {
              Boundary_Transport_Water$new(
                boundaryIdx = self$boundsTableList[["bounds_transport_water_int"]]$boundaryIdx[rowNum],
                currency = self$boundsTableList[["bounds_transport_water_int"]]$currency[rowNum],
                boundarySuperClass = self$boundsTableList[["bounds_transport_water_int"]]$boundarySuperClass[rowNum],
                upstreamCell = self$cells[[  self$boundsTableList[["bounds_transport_water_int"]]$upstreamCell[rowNum] ]],
                downstreamCell = self$cells[[ self$boundsTableList[["bounds_transport_water_int"]]$downstreamCell[rowNum] ]],
                discharge = self$boundsTableList[["bounds_transport_water_int"]]$discharge[rowNum]
              )
            }
          ) # close llply
        }, # close method



        #' @method WQModel$initializeSoluteTransportBoundaries
        #' @description Instantiate the solute transport boundaries
        #' @return Solute transport boundaries
        #'
        initializeSoluteTransportBoundaries=
          function( ){
            plyr::llply(
              1:nrow(self$solute_transport_df),
              function(rowNum) {
                Boundary_Transport_Solute$new(
                  boundaryIdx = self$solute_transport_df$boundaryIdx[rowNum],
                  currency = self$solute_transport_df$currency[rowNum],
                  boundarySuperClass = self$solute_transport_df$boundarySuperClass[rowNum],
                  linkedBound = c(bounds_transport_water_ext, bounds_transport_water_int)[[ self$solute_transport_df$linkedBound[rowNum] ]],
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
          }, # close method



        #' @method WQModel$initializeSoluteReactionBoundaries
        #' @description Instantiate the solute reaction boundaries
        #' @return A list of solute reaction boundaries
        initializeSoluteReactionBoundaries = function(){
          plyr::llply(
            1:nrow(self$boundsTableList[["bounds_react_solute"]]),
            function(rowNum) {
              Boundary_Reaction_Solute$new(
                boundaryIdx = boundsTable$boundaryIdx[rowNum],
                currency = boundsTable$currency[rowNum],
                boundarySuperClass = boundsTable$boundarySuperClass[rowNum],
                upstreamCell = self$cells[[ boundsTable$upstreamCell[rowNum] ]],
                timeInterval = self$timeInterval,
                pcntToRemove = boundsTable$pcntToRemove[rowNum],
                qStorage = boundsTable$qStorage[rowNum],
                alpha = boundsTable$alpha[rowNum],
                tauMin = boundsTable$tauMin[rowNum],
                tauMax = boundsTable$tauMax[rowNum],
                k = boundsTable$k[rowNum],
                processMethod = boundsTable$processMethod[rowNum]
              )
            }
          ) # close llply
        } # close method

      ) # closes public list
  ) # closes WQ model
