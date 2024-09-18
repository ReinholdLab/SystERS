#' @title Class systERSModel (R6)
#' A water quality model
#' @description Define and instantiate the systERS model and the network topology of
#'   cells and boundaries
#' @importFrom R6 R6Class
#' @export

systERSModel <-
  R6::R6Class(
    classname = "systERSModel",

    public =
      list(
        #' @field cells The model cells, stored in a list
        cells = NULL,
        #' @field bounds The model boundaries, stored in a list
        bounds = NULL,
        #' @field timeInterval The model time step
        timeInterval = NULL,
        #' @field boundsTransportTable_water_int Table of internal water
        #'   boundary specifications
        boundsTransportTable_water_int = NULL,
        #' @field boundsTransportTable_water_ext Table of external water
        #'   boundary specifications
        boundsTransportTable_water_ext = NULL,
        #' @field boundsTransportTable_solute_int Table of internal solute
        #'   boundary specifications
        boundsTransportTable_solute_int = NULL,
        #' @field boundsTransportTable_solute_us Table of external, upstream
        #'   solute boundary specifications
        boundsTransportTable_solute_us = NULL,
        #' @field boundsTransportTable_solute_ds Table of external, downstream
        #'   solute boundary specifications
        boundsTransportTable_solute_ds = NULL,
        #' @field boundsReactionTable_solute_int Table of internal solute
        #'   boundary specifications
        boundsReactionTable_solute_int = NULL,
        #' @field cellsTable_water_stream Table with the names of the water
        #'   cells and their attributes in the stream processing domain
        cellsTable_water_stream = NULL,
        #' @field cellsTable_solute_stream Table with the names of the solute
        #'   cells and their attributes in the stream processing domain
        cellsTable_solute_stream = NULL,
        #' @field cellsTable_water_soil Table with the names of the water
        #'   cells and their attributes in the soil processing domain
        cellsTable_water_soil = NULL,
        #' @field cellsTable_solute_soil Table with the names of the solute
        #'   cells and their attributes in the soil processing domain
        cellsTable_solute_soil = NULL,
        #' @field cellsTable_water_groundwater Table with the names of the water
        #'   cells and their attributes in the groundwater processing domain
        cellsTable_water_groundwater = NULL,
        #' @field cellsTable_solute_groundwater Table with the names of the
        #'   solute cells and their attributes in the groundwater processing
        #'   domain
        cellsTable_solute_groundwater = NULL,
        #' @field cellsTableList List storing all of the cell tables
        cellsTableList = NULL,
        #' @field boundsTableList List storing all of the cell tables
        boundsTableList = NULL,
        #' @field solute_transport_df A data frame created from the solute
        #'   transport boundary specifications
        solute_transport_df = NULL,
        #' @field unitsTable Table of units for model parameters and outputs
        unitsTable = NULL,
        #' @field iterationNum How many times the model has iterated
        iterationNum = NULL,

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
        #' @return The object of class \code{systERSModel}
        initialize =
          function(
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
              unitsTable = NULL,
              timeInterval
           ) {
            #### GENERAL

            # set duration of each time step
            self$timeInterval <- timeInterval

            # store the units table
            self$unitsTable <- unitsTable

            self$iterationNum <- 0


            #### CELLS

            # Create objects with specifications for each of the different types of cells
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
            # Tables with boundary specifications
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
            boundsTablesToKeep <- sapply(self$boundsTableList, function(df) !is.null(df))
            self$boundsTableList <- self$boundsTableList[boundsTablesToKeep]


            self$solute_transport_df <-
              rbind(
                self$boundsTableList[["bounds_transport_solute_int"]],
                self$boundsTableList[["bounds_transport_solute_us"]],
                self$boundsTableList[["bounds_transport_solute_ds"]]
              )

            #### INSTANTIATE THE CELLS & BOUNDARIES

            self$cellFactory()
            self$boundaryFactory()

            self$linkBoundsToCells() #must do this after the cells AND boundaries are already instantiated
            lapply(self$cells, function(c) c$populateDependencies()) # populate dependencies of cells that require bounds to be instantiated first


            ## Order the boundaries according to the order in which the trades
            ## should be executed.  For transport boundaries, these just get
            ## ordered according to their names; their order doesn't matter for
            ## the current version of the model.  It does, however, matter that
            ## water transport happens first followed by solute transport.  The
            ## reaction boundary order is preserved from the input table to
            ## allow the end user to specify which solutes "react" first.
            waterTransportBounds <- self$bounds[sapply(self$bounds, function(b) any(class(b) %in% "Boundary_Transport_Water"))]
            waterTransportBounds <- waterTransportBounds[sort(sapply(waterTransportBounds, "[[", "boundaryIdx"))]

            if(!is.null(self$solute_transport_df)) {
              soluteTransportBounds <- self$bounds[sapply(self$bounds, function(b) any(class(b) %in% "Boundary_Transport_Solute"))]
              soluteTransportBounds <- soluteTransportBounds[sort(sapply(soluteTransportBounds, "[[", "boundaryIdx"))]
            }

            if(!is.null(self$boundsReactionTable_solute_int)) {
              soluteReactionBounds <- self$bounds[sapply(self$bounds, function(b) any(class(b) %in% "Boundary_Reaction_Solute"))]
            }

            if(!is.null(self$solute_transport_df) && !is.null(self$boundsReactionTable_solute_int)) {
              self$bounds <- c(waterTransportBounds, soluteTransportBounds, soluteReactionBounds)
            }

          },


    #' @method Method systERSModel$cellFactory
    #' @description Create a list of the model cells
    #' @return A list of model cells
    cellFactory = function(){

      # Error check to see if any cell specifications are duplicated
      self$errorCheckCellInputs()

      # generate the cells from the cells tables
      if(!is.null(self$cellsTable_water_stream)){
        cells_water_stream <- self$initializeWaterCells_stream()
        names(cells_water_stream) <- self$cellsTable_water_stream$cellIdx
        self$cells <- cells_water_stream
      }

      if(!is.null(self$cellsTable_solute_stream)) {
        cells_solute_stream <- self$initializeSoluteCells_stream()
        names(cells_solute_stream) <- self$cellsTable_solute_stream$cellIdx
        self$cells <- c(self$cells, cells_solute_stream)
      }

      if(!is.null(self$cellsTable_water_soil)) {
        cells_water_soil <- self$initializeWaterCells_soil()
        names(cells_water_soil) <- self$cellsTable_water_soil$cellIdx
        self$cells <- cells_water_soil
      }

      if(!is.null(self$cellsTable_solute_soil)) {
        cells_solute_soil <- self$initializeSoluteCells_soil()
        names(cells_solute_soil) <- self$cellsTable_solute_soil$cellIdx
        self$cells <- c(self$cells, cells_solute_soil)
      }



      if(!is.null(self$cellsTable_solute_stream)) {
        self$linkSoluteCellsToWaterCells_stream()
      } else if (!is.null(self$cellsTable_solute_soil)) {
        self$linkSoluteCellsToWaterCells_soil()
      }
      return()


    }, # closes cellFactory


    #' @method Method systERSModel$boundaryFactory
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
            if("Boundary_Transport_Water_Stream" %in% class(b)) { b$populateDependenciesExternalBound()
              return(b)
            } else if("Boundary_Transport_Water_Soil" %in% class(b)) { b$populateDependenciesExternalBound()
              return(b)
            }
          }
        )

      # note that a single cell model has no internal boundaries, so create
      # internal boundaries if they are specified, but if they are NOT
      # specified then the only water boundaries are the external ones
      if(!is.null(self$boundsTableList[["bounds_transport_water_int"]]$boundaryIdx)){
        bounds_transport_water_int <- self$initializeInternalWaterTransportBoundaries()
        names(bounds_transport_water_int) <- self$boundsTableList[["bounds_transport_water_int"]]$boundaryIdx

        bounds_transport_water_int <-
          lapply(
            bounds_transport_water_int,
            function(b) {
              if("Boundary_Transport_Water_Stream" %in% class(b)) { b$populateDependenciesInternalBound()
                return(b)
              } else if("Boundary_Transport_Water_Soil" %in% class(b)) { b$populateDependenciesInternalBound()
                return(b)
              }
            }
          )
        bounds_transport_water <- c(bounds_transport_water_ext, bounds_transport_water_int)

      } else {
        bounds_transport_water <- bounds_transport_water_ext
      }

      # Add the water bounds to the bounds list
      self$bounds <- bounds_transport_water

      # create the solute transport bounds (references self$bounds)
      if (!is.null(self$solute_transport_df)) {
        bounds_transport_solute <- self$initializeSoluteTransportBoundaries()
        names(bounds_transport_solute) <- self$solute_transport_df$boundaryIdx
      }

      # Add the solute transport bounds to the bounds list
      if (!is.null(self$solute_transport_df)) {
        self$bounds <- c(self$bounds, bounds_transport_solute)
      }

      # Create solute reaction boundaries
      if (!is.null(self$boundsReactionTable_solute_int)) {
        bounds_react_solute <- self$initializeSoluteReactionBoundaries()
        names(bounds_react_solute) <- self$boundsTableList[["bounds_reaction_solute_int"]]$boundaryIdx
      }
      # Add the solute reaction boundaries to the bounds list
      if (!is.null(self$boundsReactionTable_solute_int)) {
        self$bounds <- c(self$bounds, bounds_react_solute)
      }

    }, # closes boundaryFactory


    #' @method Method systERSModel$errorCheckCellInputs
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


    #' @method Method systERSModel$initializeWaterCells_stream
    #' @description Instantiate the water cells in the stream process domain
    #' @importFrom plyr llply
    #' @return List of stream water cells
    initializeWaterCells_stream = function(){
      tbl <- self$cellsTableList$cells_water_stream
      if(!is.null(tbl)){
        return(
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
        ) # close return
      } else {return(NULL)} # close if
    }, # close method

    #' @method Method systERSModel$initializeSoluteCells_stream
    #' @description Instantiate the solute cells in the stream process domain
    #' @importFrom plyr llply
    #' @return List of stream solute cells
    initializeSoluteCells_stream = function(){
      tbl <- self$cellsTable_solute_stream
      if(!is.null(tbl)){
        return(
          plyr::llply(
            1:nrow(tbl),
            function(rowNum){

              Cell_Solute_Stream$new(
                cellIdx = tbl$cellIdx[rowNum],
                processDomain = tbl$processDomain[rowNum],
                currency = tbl$currency[rowNum],

                concentration = tbl$concentration[rowNum],
                linkedCell = self$cells[[ tbl$linkedCell[rowNum] ]]
              )
            }
          ) # close llply
        ) # close return
      } else{ return(NULL) } # close if
    }, # close method

    #' @method Method systERSModel$initializeWaterCells_soil
    #' @description Instantiate the soil water cells in the stream process domain
    #' @importFrom plyr llply
    #' @return List of soil water cells
    initializeWaterCells_soil = function(){
      tbl <- self$cellsTableList$cells_water_soil
      if(!is.null(tbl)){
        return(
          plyr::llply(
            1:nrow(tbl),
            function(rowNum){
              Cell_Water_Soil$new(
                cellIdx = tbl$cellIdx[rowNum],
                currency = tbl$currency[rowNum],
                processDomain = tbl$processDomain[rowNum],

                cellLength = tbl$cellLength[rowNum],
                cellHeight = tbl$cellHeight[rowNum],
                cellWidth = tbl$cellWidth[rowNum],
                cellDepth = tbl$cellDepth[rowNum],
                cellSoilType = tbl$cellSoilType[rowNum],
                waterVolume = tbl$waterVolume[rowNum],
                cellHydraulicConductivity = tbl$cellHydraulicConductivity[rowNum],
                cellMaxTemp = tbl$cellMaxTemp[rowNum],
                cellMinTemp = tbl$cellMinTemp[rowNum],
                cellSolarRadiation = tbl$cellSolarRadiation[rowNum],
                rootDepth = tbl$rootDepth[rowNum]
              )
            }
          ) # close llply
        ) # close return
      } else {return(NULL)} # close if
    }, # close method

    #' @method Method systERSModel$initializeSoluteCells_soil
    #' @description Instantiate the solute cells in the soil process domain
    #' @importFrom plyr llply
    #' @return List of soil solute cells
    initializeSoluteCells_soil = function(){
      tbl <- self$cellsTable_solute_soil
      if(!is.null(tbl)){
        return(
          plyr::llply(
            1:nrow(tbl),
            function(rowNum){
              Cell_Solute_Soil$new(
                cellIdx = tbl$cellIdx[rowNum],
                processDomain = tbl$processDomain[rowNum],
                currency = tbl$currency[rowNum],

                concentration = tbl$concentration[rowNum],
                linkedCell = self$cells[[ tbl$linkedCell[rowNum] ]],
                reactionVolume = tbl$reactionVolume[rowNum]
              )
            }
          ) # close llply
        ) # close return
      } else{ return(NULL) } # close if
    }, # close method



    #' @method Method systERSModel$linkSoluteCellsToWaterCells_stream
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

    #' @method Method systERSModel$linkSoluteCellsToWaterCells_soil
    #' @description Link the solute cells to the soil water cells in the stream process
    #'   domain
    #' @return Soil water cells with their \code{linkedSoluteCells} attribute populated
    #'   with a list of solute cells that are linked to the soil water cell
    linkSoluteCellsToWaterCells_soil =
      function(){
        soilWaterCells <- self$cells[sapply(self$cells, function(c) "Cell_Water_Soil" %in% class(c))]
        lapply(
          soilWaterCells,
          function(c) {
            # identify the water cells to which the solute cells are connected
            cellIdxs <- self$cellsTable_solute_soil$cellIdx[self$cellsTable_solute_soil$linkedCell == c$cellIdx]
            soluteCells <- self$cells[cellIdxs]
            names(soluteCells) <- cellIdxs
            # link the solute cells to the water cells
            c$linkedSoluteCells <- soluteCells
            return(c)
          }
        ) # close lapply
      }, # close method


    #' @method Method systERSModel$errorCheckBoundaryInputs
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
            if( any( duplicated(tbl) ) ){
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



    #' @method Method systERSModel$initializeExternalWaterTransportBoundaries
    #' @description Instantiate the transport boundaries that are on the physical
    #'   edges of the model
    #' @importFrom plyr llply
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
          } else if (tbl$processDomain[rowNum] == "soil"){
            b <-
              Boundary_Transport_Water_Soil$new(
                boundaryIdx = tbl$boundaryIdx[rowNum],
                currency = tbl$currency[rowNum],
                upstreamCell = self$cells[[upstreamCell]],
                downstreamCell = self$cells[[downstreamCell]],
                discharge = tbl$discharge[rowNum],
                timeInterval = self$timeInterval,
                tradeType = tbl$tradeType[rowNum]
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



    #' @method Method systERSModel$initializeInternalWaterTransportBoundaries
    #' @description Instantiate the transport boundaries that are internal to the
    #'   physical edges of the model
    #' @importFrom plyr llply
    #' @return Water transport boundaries within the upstream and downstream extents
    #'   of the model topology
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
          } else if(tbl$processDomainName[rowNum] == "soil"){
            Boundary_Transport_Water_Soil$new(
              boundaryIdx = tbl$boundaryIdx[rowNum],
              currency = tbl$currency[rowNum],
              upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
              downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
              discharge = tbl$discharge[rowNum],
              timeInterval = self$timeInterval,
              tradeType = tbl$tradeType[rowNum]
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



    #' @method Method systERSModel$initializeSoluteTransportBoundaries
    #' @description Instantiate the solute transport boundaries
    #' @importFrom plyr llply
    #' @return Solute transport boundaries
    #'
    initializeSoluteTransportBoundaries =
      function( ){
        tbl <- self$solute_transport_df
        plyr::llply(
          1:nrow(tbl),
          function(rowNum) {
            if(tbl$processDomain[rowNum] == "stream"){
              Boundary_Transport_Solute_Stream$new(
                boundaryIdx = tbl$boundaryIdx[rowNum],
                currency = tbl$currency[rowNum],
                linkedBound = self$bounds[[ tbl$linkedBound[rowNum] ]],
                # concentration = tbl$concentration[rowNum],
                load = tbl$load[rowNum],
                upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                timeInterval = self$timeInterval,
                processDomain = tbl$processDomain[rowNum]
              )
            } else if (tbl$processDomain[rowNum] == "soil"){
              Boundary_Transport_Solute_Soil$new(
                boundaryIdx = tbl$boundaryIdx[rowNum],
                currency = tbl$currency[rowNum],
                linkedBound = self$bounds[[ tbl$linkedBound[rowNum] ]],
                upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                timeInterval = self$timeInterval,
                processDomain = tbl$processDomain[rowNum]
              )
            } else {
              Boundary_Transport_Solute$new(
                boundaryIdx = tbl$boundaryIdx[rowNum],
                currency = tbl$currency[rowNum],
                linkedBound = self$bounds[[ tbl$linkedBound[rowNum] ]],
                # concentration = tbl$concentration[rowNum],
                load = tbl$load[rowNum],
                upstreamCell = self$cells[[  tbl$upstreamCellIdx[rowNum] ]],
                downstreamCell = self$cells[[ tbl$downstreamCellIdx[rowNum] ]],
                timeInterval = self$timeInterval,
                processDomain = tbl$processDomain[rowNum]
              )
            }
          }
        )
      }, # close method



    #' @method Method systERSModel$initializeSoluteReactionBoundaries
    #' @description Instantiate the solute reaction boundaries.  Current
    #'   model version only supports stream reaction boundaries.
    #' @importFrom plyr llply
    #' @return A list of solute reaction boundaries
    initializeSoluteReactionBoundaries = function(){
      tbl <- self$boundsTableList[["bounds_reaction_solute_int"]]
      plyr::llply(
        1:nrow(tbl),
        function(rowNum) {
          if(tbl$processDomain[rowNum] == "stream"){
            Boundary_Reaction_Solute_Stream$new(
              processDomain = tbl$processDomain[rowNum],
              boundaryIdx = tbl$boundaryIdx[rowNum],
              currency = tbl$currency[rowNum],
              upstreamCell = self$cells[[ tbl$upstreamCellIdx[rowNum] ]],
              downstreamCell = NULL,
              timeInterval = self$timeInterval,
              pcntToRemove = tbl$pcntToRemove[rowNum],
              qStorage = tbl$qStorage[rowNum],
              volWaterInStorage = tbl$volWaterInStorage[rowNum],
              alpha = tbl$alpha[rowNum],
              tauMin = tbl$tauMin[rowNum],
              tauMax = tbl$tauMax[rowNum],
              tauRxn = tbl$tauRxn[rowNum],
              k = tbl$k[rowNum],
              processMethodName = tbl$processMethodName[rowNum]
            )
          } else if(tbl$processDomain[rowNum] == "soil"){
            Boundary_Reaction_Solute_Soil$new(
              processDomain = tbl$processDomain[rowNum],
              boundaryIdx = tbl$boundaryIdx[rowNum],
              currency = tbl$currency[rowNum],
              upstreamCell = self$cells[[ tbl$upstreamCellIdx[rowNum] ]],
              downstreamCell = NULL,
              timeInterval = self$timeInterval,
              qStorage = tbl$qStorage[rowNum],
              reactionConstant = tbl$reactionConstant[rowNum]
            )
          } else {
            Boundary_Reaction_Solute$new(
              boundaryIdx = tbl$boundaryIdx[rowNum],
              currency = tbl$currency[rowNum],
              upstreamCell = self$cells[[ tbl$upstreamCellIdx[rowNum] ]],
              downstreamCell = NULL,
              timeInterval = self$timeInterval,
              pcntToRemove = tbl$pcntToRemove[rowNum],
              qStorage = tbl$qStorage[rowNum],
              volWaterInStorage = tbl$volWaterInStorage[rowNum],
              alpha = tbl$alpha[rowNum],
              tauMin = tbl$tauMin[rowNum],
              tauMax = tbl$tauMax[rowNum],
              tauRxn = tbl$tauRxn[rowNum],
              k = tbl$k[rowNum],
              processMethodName = tbl$processMethodName[rowNum]
            )
          }
        }
      ) # close llply
    }, # close method



    #' @method Method systERSModel$linkBoundsToCells
    #' @description Creates a list of boundaries attached to each cell and
    #'   then adds the list of bounds connected to each cell as an attribute
    #'   of the cell.
    #' @return Cells with \code{linkedBoundsList} attribute populated
    linkBoundsToCells = function(){


      lapply(
        self$bounds, function(bound) {
          bound$upstreamCell$linkedBoundsList$downstreamBounds <- c(bound$upstreamCell$linkedBoundsList$downstreamBounds, bound)
          bound$downstreamCell$linkedBoundsList$upstreamBounds <- c(bound$downstreamCell$linkedBoundsList$upstreamBounds, bound)
        }
      )

      return()
    },



    #' @method Method systERSModel$trade
    #' @description Runs the trade method on all boundaries in the model in
    #'   the order in which they occur in the \code{bounds} list.
    #' @return Updated boundary values.
    trade = function(){
      lapply(self$bounds, function(bound) bound$trade())
      return()
    },


    #' @method Method systERSModel$store
    #'
    #' @description Runs the store method on all cells in the model.
    #' @return Updated store values.
    store = function(){
      lapply(self$bounds, function(bound) bound$store())
      return()
    },

    #' @method Method systERSModel$update
    #'
    #' @description Runs the update method on all cells and boundaries in the model.
    #' @return Updates all values in cells and boundaries based on trades and stores.
    update = function(){
      lapply(self$cells, function(c) c$update())
      lapply(
        self$bounds[sapply(self$bounds, function(b) any(class(b) %in% "Boundary_Transport_Water"))],
        function(b) b$populateDependencies()
      )
      return()
    },

    #' @method Method systERSModel$iterate
    #' @description Iterates the model by calling all trades, stores, and
    #'   updates.
    #' @return All cells and boundaries will values updated to reflect the
    #'   trades, stores, and updates that occurred during the time step.
    iterate = function(){
      self$trade()
      self$store()
      self$update()
      self$iterationNum <- self$iterationNum + 1

    }

      ) # closes public list
  ) # closes systERS model

