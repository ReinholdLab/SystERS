#' @title Boundary_Transport_Water
#'
#' @description Transport boundary for water between cells
#'
#' @param discharge mean discharge
#'
#' @export
#'
Boundary_Transport_Water <-
  R6::R6Class(
    classname = "Boundary_Transport_Water",
    inherit = Boundary,
    public =
      list(
        discharge = NULL,
        initialize =
          function(
            ...,
            discharge
          ){
            super$initialize(...)

            self$discharge <- as.numeric(discharge) # as.numeric is here in case reading from sparse table
          } # close initialize
      ) # close public
  ) # close R6 class




########  STREAM-SPECIFIC BOUNDARIES   ########


#' @title Boundary_Transport_Water_Stream
#'
#' @description Transport boundary for water between stream cells
#'
#' @param channelVelocity mean water velocity
#' @param channelResidenceTime mean residence time of the surface water
#' @param hydraulicLoad hydraulic load of the cell
#'
#' @export
#'
Boundary_Transport_Water_Stream <-
  R6::R6Class(
    classname = "Boundary_Transport_Water_Stream",
    inherit = Boundary_Transport_Water,
    public =
      list(
        channelVelocity = NULL,
        channelResidenceTime = NULL,
        hydraulicLoad = NULL,
        initialize =
          function(...){
            super$initialize(...)
          } # close initialize
      ) # close public
  ) # close R6 class


#' @method populateDependencies
#'
#' @description Populate boundary dependencies
#'
#' @export
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "populateDependencies",
  value = function(){
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
  }
)

#' @method trade_static
#'
#' @description Calculates the trades between the water cells using static
#'   values for the boundary conditions; these values are provided by the user.
#'
#' @export
#'
Boundary_Transport_Water_Stream$set(
  which = "public",
  name = "trade_static",
  value =
    function(){

      # volume of water to trade
      volume <- self$discharge * timeInterval #L

      if(!self$usModBound){
        # volume of water to remain
        volumeToRemain <- self$upstreamCell$channelVolume - volume
        if(volumeToRemain < 0) warning(
          paste(
            "You are about to remove more water volume from a cell than it held at the start of the timestep.
             Boundary is", self$boundaryIdx,
            "Cell is", self$upstreamCell$upstreamCellIdx
          ) # close paste
        ) # close warning
      } # close if statement

      return(list(discharge = self$discharge, volume = volume))
    } # close function def
)
