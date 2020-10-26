#' @title Class WQModel (R6)
#' A water quality model
#' @description Define and instantiate the WQ model and the network topology of
#'   cells and boundaries
#' @export

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
        #' @return The ojbect of class \code{WQModel}
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

            #### BOUNDARIES

            # Generate the boundaries from the three tables with boundaries info
            self$boundsTransportTable_water_int <- boundsTransportTable_water_int
            self$boundsTransportTable_water_ext <- boundsTransportTable_water_ext

            self$boundsTransportTable_solute_int <- boundsTransportTable_solute_int
            self$boundsTransportTable_solute_us <- boundsTransportTable_solute_us
            self$boundsTransportTable_solute_ds <- boundsTransportTable_solute_ds

            self$boundsReactionTable_solute_int <- boundsReactionTable_solute_int

            self$boundsTableList <-
              list(
                bounds_transport_water_int = self$boundsTransportTable_water_int,
                bounds_transport_water_ext = self$boundsTransportTable_water_ext,
                bounds_transport_solute_int = self$boundsTransportTable_solute_int,
                bounds_transport_solute_us = self$boundsTransportTable_solute_us,
                bounds_transport_solute_ds = self$boundsTransportTable_solute_ds,
                bounds_reaction_solute_int = self$boundsReactionTable_solute_int
              )

            self$solute_transport_df <-
              rbind(
                self$boundsTableList[["bounds_transport_solute_int"]],
                self$boundsTableList[["bounds_transport_solute_us"]],
                self$boundsTableList[["bounds_transport_solute_ds"]]
              )
          },


        #' @method Method WQModel$cellFactory
        #' @description Create a list of the model cells
        #' @return A list of model cells
        cellFactory = function(){

          # Error check to see if any cell specifications are duplicated
          self$errorCheckCellInputs()

          # generate the cells from the cells tables
          cells_water_stream <- self$initializeWaterCells_stream()
          names(cells_water_stream) <- self$cellsTable_water_stream$cellIdx
          self$cells <- cells_water_stream

          cells_solute_stream <- self$initializeSoluteCells_stream()
          names(cells_solute_stream) <- self$cellsTable_solute_stream$cellIdx
          self$cells <- c(self$cells, cells_solute_stream)

          self$linkSoluteCellsToWaterCells_stream()
          return()

        }, # closes cellFactory


        #' @method Method WQModel$boundaryFactory
        #' @description Create a list of the model boundaries
        #' @return A list of model boundaries
        boundaryFactory = function(){

          # Run a few checks on the boundary inputs
          self$errorCheckBoundaryInputs()

          # First, create the water transport boundaries
          bounds_transport_water_ext <- self$initializeExternalWaterTransportBoundaries()
          names(bounds_transport_water_ext) <- self$boundsTableList[["bounds_transport_water_ext"]]$boundaryIdx

          bounds_transport_water_ext <-
            lapply(
              bounds_transport_water_ext,
              function(b) {
                if("Boundary_Transport_Water_Stream" %in% class(b)) b$populateDependenciesExternalBound()
                return(b)
              }
            )

          bounds_transport_water_int <- self$initializeInternalWaterTransportBoundaries()
          names(bounds_transport_water_int) <- self$boundsTableList[["bounds_transport_water_int"]]$boundaryIdx

          bounds_transport_water_int <-
            lapply(
              bounds_transport_water_int,
              function(b) {
                if("Boundary_Transport_Water_Stream" %in% class(b)) b$populateDependenciesInternalBound()
                return(b)
              }
            )

          # populate dependencies
          bounds_transport_water <- c(bounds_transport_water_ext, bounds_transport_water_int)
          # bounds_transport_water <-
          #   lapply(
          #     bounds_transport_water,
          #     function(b) {
          #       if("Boundary_Transport_Water_Stream" %in% class(b)) b$populateDependencies()
          #     }
          #   )

          # Add the water bounds to the bounds list
          self$bounds <- bounds_transport_water

          # create the solute transport bounds (references self$bounds)
          bounds_transport_solute <- self$initializeSoluteTransportBoundaries()
          names(bounds_transport_solute) <- self$solute_transport_df$boundaryIdx

          # Add the solute transport bounds to the bounds list
          self$bounds <- c(self$bounds, bounds_transport_solute)

          # Create solute reaction boundaries
          bounds_react_solute <- self$initializeSoluteReactionBoundaries()
          names(bounds_react_solute) <- self$boundsTableList[["bounds_reaction_solute_int"]]$boundaryIdx

          # Add the solute reaction boundaries to the bounds list
          self$bounds <- c(self$bounds, bounds_react_solute)

        }, # closes boundaryFactory


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
            tbl <- self$cellsTable_water_stream
            plyr::llply(
              1:nrow(tbl),
              function(rowNum){
                Cell_Water_Stream$new(
                  cellIdx = tbl$cellIdx[rowNum],
                  currency = tbl$currency[rowNum],
                  processDomain = tbl$processDomain[rowNum],

                  channelWidth = tbl$channelWidth[rowNum],
                  channelLength = tbl$channelLength[rowNum],
                  channelDepth = tbl$channelDepth[rowNum]
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
            tbl <- self$cellsTable_solute_stream
            plyr::llply(
              1:nrow(tbl),
              function(rowNum){
                Cell_Solute$new(
                  cellIdx = tbl$cellIdx[rowNum],
                  processDomain = tbl$processDomain[rowNum],
                  currency = tbl$currency[rowNum],

                  concentration = tbl$concentration[rowNum],
                  linkedCell = self$cells[[ tbl$linkedCell[rowNum] ]]
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
          function(){
            streamWaterCells <- self$cells[sapply(self$cells, function(c) "Cell_Water_Stream" %in% class(c))]
            lapply(
              streamWaterCells,
              function(c) {
                # identify the water cells to which the solute cells are connected
                cellIdxs <- self$cellsTable_solute_stream$cellIdx[self$cellsTable_solute_stream$linkedCell == c$cellIdx]
                soluteCells <- self$cells[cellIdxs]
                names(soluteCells) <- cellIdxs
                # link the solute cells to the water cells
                c$linkedSoluteCells <- soluteCells
                return(c)
              }
            ) # close lapply
          }, # close method


        #' @method Method WQModel$errorCheckBoundaryInputs
        #' @description Error check to see if any boundary specifications are duplicated
        #'   or have references to cells that do not exist.
        #' @return Nothing if no error is detected.  If an error is detected, a message
        #'   is thrown.
        errorCheckBoundaryInputs =
          function(){
            lapply(
              1:length(self$boundsTableList),
              function(i) {
                tblName <- names(self$boundsTableList)[i]
                tbl <- self$boundsTableList[[i]]
                if( any( duplicated(tbl$boundsTable) ) ){
                  stop(paste0("At least one specification for a boundary is duplicated in the table", tblName, "."))
                }
                if(length(tbl$boundaryIdx) != length(unique(tbl$boundaryIdx))) {
                  stop(paste0("A boundary name was duplicated in the table:", tblName, ".  All boundary names must be unique."))
                }
                if( any(!(unique(tbl$downstreamCellIdx[!is.na(tbl$downstreamCellIdx)]) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
                  stop(paste0("In the table ", tblName, ", a name of a downstream cell was provided that refers to a cell that has not been instantiated."))
                }
                if( any(!(unique(tbl$upstreamCellIdx[!is.na(tbl$upstreamCellIdx)]) %in% unique(sapply(self$cells, function(cell) cell$cellIdx)))) ){
                  stop(paste0("In the table ", tblName, ", a name of a upstream cell was provided that refers to a cell that has not been instantiated."))
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
          tbl <- self$boundsTableList[["bounds_transport_water_ext"]]
          plyr::llply(
            1:nrow(tbl),
            function(rowNum) {

              locationOfBoundInNetwork <- tbl$locationOfBoundInNetwork[rowNum]

              if(!(locationOfBoundInNetwork %in% c("upstream", "downstream"))) stop("External model boundaries must have a 'locationOfBoundInNetwork' with a value of either 'upstream' or 'downstream'.")
              if(locationOfBoundInNetwork == "upstream") {
                upstreamCell <- NA
                downstreamCell <- tbl$cellIdx[rowNum]
              }
              if(locationOfBoundInNetwork == "downstream") {
                upstreamCell <- tbl$cellIdx[rowNum]
                downstreamCell <- NA
              }

              if(tbl$processDomain[rowNum] == "stream"){
                b <-
                  Boundary_Transport_Water_Stream$new(
                    boundaryIdx = tbl$boundaryIdx[rowNum],
                    currency = tbl$currency[rowNum],
                    upstreamCell = self$cells[[upstreamCell]],
                    downstreamCell = self$cells[[downstreamCell]],
                    discharge = tbl$discharge[rowNum],
                    timeInterval = self$timeInterval
                )
              } else {
                b <-
                  Boundary_Transport_Water$new(
                    boundaryIdx = tbl$boundaryIdx[rowNum],
                    currency = tbl$currency[rowNum],
                    upstreamCell = self$cells[[upstreamCell]],
                    downstreamCell = self$cells[[downstreamCell]],
                    discharge = tbl$discharge[rowNum],
                    timeInterval = self$timeInterval
                  )
              }
              return(b)

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
          tbl <- self$boundsTableList[["bounds_transport_water_int"]]
          plyr::llply(
            1:nrow(tbl),
            function(rowNum) {

              if(tbl$processDomainName[rowNum] == "stream"){
                Boundary_Transport_Water_Stream$new(
                  boundaryIdx = tbl$boundaryIdx[rowNum],
                  currency = tbl$currency[rowNum],
                  upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                  downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                  discharge = tbl$discharge[rowNum],
                  timeInterval = self$timeInterval
                )
              } else {
                Boundary_Transport_Water$new(
                  boundaryIdx = tbl$boundaryIdx[rowNum],
                  currency = tbl$currency[rowNum],
                  upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                  downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                  discharge = tbl$discharge[rowNum],
                  timeInterval = self$timeInterval
                )
              } # close else
            }
          ) # close llply
        }, # close method



        #' @method WQModel$initializeSoluteTransportBoundaries
        #' @description Instantiate the solute transport boundaries
        #' @return Solute transport boundaries
        #'
        initializeSoluteTransportBoundaries=
          function( ){
            tbl <- self$solute_transport_df
            plyr::llply(
              1:nrow(tbl),
              function(rowNum) {
                Boundary_Transport_Solute$new(
                  boundaryIdx = tbl$boundaryIdx[rowNum],
                  currency = tbl$currency[rowNum],
                  linkedBound = self$bounds[[ tbl$linkedBound[rowNum] ]],
                  # concentration = tbl$concentration[rowNum],
                  load = tbl$load[rowNum],
                  upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                  downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                  timeInterval = self$timeInterval
                )
              }
            )
          }, # close method



        #' @method WQModel$initializeSoluteReactionBoundaries
        #' @description Instantiate the solute reaction boundaries
        #' @return A list of solute reaction boundaries
        initializeSoluteReactionBoundaries = function(){
          tbl <- self$boundsTableList[["bounds_reaction_solute_int"]]
          plyr::llply(
            1:nrow(tbl),
            function(rowNum) {
              Boundary_Reaction_Solute$new(
                boundaryIdx = tbl$boundaryIdx[rowNum],
                currency = tbl$currency[rowNum],
                upstreamCell = self$cells[[ tbl$upstreamCellIdx[rowNum] ]],
                downstreamCell = NULL,
                timeInterval = self$timeInterval,
                pcntToRemove = tbl$pcntToRemove[rowNum],
                qStorage = tbl$qStorage[rowNum],
                alpha = tbl$alpha[rowNum],
                tauMin = tbl$tauMin[rowNum],
                tauMax = tbl$tauMax[rowNum],
                k = tbl$k[rowNum],
                processMethodName = tbl$processMethodName[rowNum]
              )
            }
          ) # close llply
        } # close method

      ) # closes public list
  ) # closes WQ model
