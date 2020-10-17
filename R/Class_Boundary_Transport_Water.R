#' @title Class Boundary_Transport_Water (R6)
#' A model boundary that transports water to and from cells
#' @description Transport boundary for water between cells
#' @export

Boundary_Transport_Water <-
  R6::R6Class(
    classname = "Boundary_Transport_Water",

    #' @inherit Boundary
    inherit = Boundary,
    public =
      list(
        #' @field discharge Water discharge rate through the boundary in user specified units (volume/time)
        discharge = NULL,
        #' @field volume Water volume passed through the boundary
        volume = NULL,

        #' @description Instantiate a water transport boundary
        #' @param discharge Rate of water discharge (a.k.a. Q)
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param boundarySuperClass String indicating the super class of the boundary, e.g., \code{transport} or \code{reaction}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that transports water
        initialize =
          function(
            ...,
            discharge
          ){
            super$initialize(...)

            self$discharge <- as.numeric(discharge) # as.numeric is here in case reading from sparse table
            self$volume <- self$discharge * self$timeInterval
          } # close initialize
      ) # close public
  ) # close R6 class




########  STREAM-SPECIFIC BOUNDARIES   ########

#' @title Class Boundary_Transport_Water_Stream (R6)
#' A model boundary that transports water to and from stream cells
#' @description Transport boundary for water between stream cells
#' @export
Boundary_Transport_Water_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Water_Stream",

    #' @inherit Boundary_Transport_Water
    inherit = Boundary_Transport_Water,

    public =
      list(

        #' @field channelVelocity Mean velocity of water in the channel compartment
        channelVelocity = NULL,
        #' @field channelResidenceTime Mean residence time of water in the channel compartment
        channelResidenceTime = NULL,
        #' @field hydraulicLoad The hydraulic load of water in the channel compartment
        hydraulicLoad = NULL,


        #' @description Instantiate a water transport boundary in the stream processing domain
        #' @param discharge Rate of water discharge (a.k.a. Q)
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param boundarySuperClass String indicating the super class of the boundary, e.g., \code{transport} or \code{reaction}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that transports water in the stream processing domain
        initialize =
          function(...){
            super$initialize(...)
          }, # close initialize



        #' @description Populate boundary dependencies.  Sets the following
        #'   \code{channelVelocity, channelResidenceTime, hydraulicLoad} based
        #'   on the \code{discharge}.
        #' @method Method Boundary_Transport_Water_Stream$populateDependencies
        #' @return Populates boundary dependencies
        populateDependencies = function(){
          # To get velocity, divide Q by the mean of x-sec area of u/s and d/s
          # cell.  Upstream model boundaries (ie, most upstream) will thus
          # have a velocity equal only to the d/s cell (because there is no
          # u/s cell).  The opposite is true for the most d/s boundaries.
          # Likewise, to get residence time, divide by the mean of u/s and d/s
          # channel lengths.  Upstream model boundaries will thus have the
          # channel length defined by the d/s cell because no u/s cell exists
          # and vice versa for d/s model boundaries. Same pattern applies to
          # hydraulic load...
          if(!any(c(self$usModBound, self$dsModBound))) {
            depth <- mean(c(self$upstreamCell$channelDepth, self$downstreamCell$channelDepth))
            widthXdepth <- mean(c(self$upstreamCell$channelWidth * self$upstreamCell$channelDepth, self$downstreamCell$channelWidth * self$downstreamCell$channelDepth))
            len <- mean(c(self$upstreamCell$channelLength, self$downstreamCell$channelLength))
          }else{
            if(self$usModBound) {
              depth <- self$downstreamCell$channelDepth
              widthXdepth <- self$downstreamCell$channelWidth * depth
              len <-self$downstreamCell$channelLength
            }
            if(self$dsModBound){
              depth <- self$upstreamCell$channelDepth
              widthXdepth <- self$upstreamCell$channelWidth * depth
              len <-self$upstreamCell$channelLength
            }
          }
          self$channelVelocity <- self$discharge / widthXdepth
          self$channelResidenceTime <- len / self$channelVelocity
          self$hydraulicLoad <- depth / self$channelResidenceTime

          return()
        },



        #' @description Calculates the trades between the stream water cells
        #'   using the values provided for \code{discharge}.
        #' @method Method Boundary_Transport_Water_Stream$trade_static
        #' @return Updates the \code{volume} in the cell based on
        #'   \code{discharge}. Returns a list with two elements
        #'   (\code{discharge, volume}).

        trade_static   = function(){
          # volume of water to trade
        self$volume <- self$discharge * self$timeInterval #L

        if(!self$usModBound){
          # volume of water to remain
          volumeToRemain <- self$upstreamCell$waterVolume - self$volume
          if(volumeToRemain < 0) warning(
            paste(
              "You are about to remove more water volume from a cell than it held at the start of the timestep.
             Boundary is", self$boundaryIdx,
              "Cell is", self$upstreamCell$cellIdx
            ) # close paste
          ) # close warning
        } # close if statement

        return(list(discharge = self$discharge, volume = self$volume))
        } # close function def
      ) # close public
  ) # close R6 class

