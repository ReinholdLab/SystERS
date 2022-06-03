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
        #' @param ... Parameters inherit from Class \code{\link{Boundary}}
        #' @param discharge Rate of water discharge (a.k.a. Q)
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
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
          }, # close initialize



        #' @method Method Boundary_Transport_Water$store
        #' @description Runs the store method on water cells in the model.
        #' @return Updated store values.
        store = function(){
          self$upstreamCell$waterVolume <- self$upstreamCell$waterVolume - self$volume
          self$downstreamCell$waterVolume <- self$downstreamCell$waterVolume + self$volume
          return(c(self$upstreamCell$waterVolume, self$downstreamCell$waterVolume))
        }
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

        #' @field populateDependencies Updates channel velocity, residence time, and hydraulic load.
        populateDependencies = NULL,


        #' @description Instantiate a water transport boundary in the stream processing domain
        #' @param ... Parameters inherit from Class \code{\link{Boundary_Transport_Water}} and thus \code{\link{Boundary}}
        #' @param discharge Rate of water discharge (a.k.a. Q)
        #' @param boundaryIdx String indexing the boundary
        #' @param currency String naming the currency handled by the boundary as a character e.g., \code{water, NO3}
        #' @param upstreamCell  Cell (if one exists) upstream of the boundary
        #' @param downstreamCell Cell (if one exists) downstream of the boundary
        #' @param timeInterval  Model time step
        #' @return A model boundary that transports water in the stream processing domain
        initialize =
          function(...){
            super$initialize(...)

            if(any(self$usModBound, self$dsModBound )) {
              self$populateDependencies <- self$populateDependenciesExternalBound
            } else{
                self$populateDependencies <- self$populateDependenciesInternalBound
              }

          }, # close initialize



        #' @description Populate boundary dependencies for boundaries with
        #'   exactly one upstream and one downstream cell.  Sets the
        #'   \code{channelVelocity} based on the \code{discharge} and the cross
        #'   sectional area of the boundary.
        #' @method Method
        #'   Boundary_Transport_Water_Stream$populateDependenciesInternalBound
        #' @return Populates boundary dependencies
        populateDependenciesInternalBound = function(){
          # To get velocity, divide Q by the mean of x-sec area of u/s and d/s
          # cell.  Upstream model boundaries (ie, most upstream) will thus
          # have a velocity equal only to the d/s cell (because there is no
          # u/s cell).  The opposite is true for the most d/s boundaries.
          # Likewise, to get residence time, divide by the mean of u/s and d/s
          # channel lengths.  Upstream model boundaries will thus have the
          # channel length defined by the d/s cell because no u/s cell exists
          # and vice versa for d/s model boundaries. Same pattern applies to
          # hydraulic load...

          depth <- mean(c(self$upstreamCell$channelDepth, self$downstreamCell$channelDepth))
          widthXdepth <- mean(c(self$upstreamCell$channelWidth * self$upstreamCell$channelDepth, self$downstreamCell$channelWidth * self$downstreamCell$channelDepth))

          self$channelVelocity <- self$discharge / widthXdepth


        },



        #' @description Populate boundary dependencies for boundaries at the
        #'   edge of the topology (i.e., with either one upstream or one
        #'   downstream cell).  Sets the \code{channelVelocity} based on the
        #'   \code{discharge} and the cross sectional area of the boundary.
        #' @method Method
        #'   Boundary_Transport_Water_Stream$populateDependenciesExternalBound
        #' @return Populates boundary dependencies
        populateDependenciesExternalBound = function(){
          # To get velocity, divide Q by the mean of x-sec area of u/s and d/s
          # cell.  Upstream model boundaries (ie, most upstream) will thus
          # have a velocity equal only to the d/s cell (because there is no
          # u/s cell).  The opposite is true for the most d/s boundaries.
          # Likewise, to get residence time, divide by the mean of u/s and d/s
          # channel lengths.  Upstream model boundaries will thus have the
          # channel length defined by the d/s cell because no u/s cell exists
          # and vice versa for d/s model boundaries. Same pattern applies to
          # hydraulic load...

          if(self$usModBound) {
            connectedCell <- self$downstreamCell
          } else if(self$dsModBound){
            connectedCell <- self$upstreamCell
          }

          depth <- connectedCell$channelDepth
          widthXdepth <- connectedCell$channelWidth * depth

          self$channelVelocity <- self$discharge / widthXdepth

        },


        #' @description Calculates the trades between the stream water cells
        #'   using the values provided for \code{discharge}.
        #' @method Method Boundary_Transport_Water_Stream$trade
        #' @return Updates the \code{volume} in the cell based on
        #'   \code{discharge}. Returns a list with two elements
        #'   (\code{discharge, volume}).

        trade   = function(){
          # volume of water to trade
        self$volume <- self$discharge * self$timeInterval #L

        if(!self$usModBound){
          # volume of water to remain
          volumeToRemain <- self$upstreamCell$waterVolume - self$volume
          if(volumeToRemain < 0) stop(
            paste(
              "You are about to remove more water volume from a cell than it held at the start of the timestep.
             Boundary is ", print(self$boundaryIdx)
            ) # close paste
          ) # close warning
        } # close if statement

        return(list(discharge = self$discharge, volume = self$volume))
        } # close function def
      ) # close public


  ) # close R6 class

