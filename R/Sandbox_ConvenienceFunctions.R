
#' @title Quickly create a list of values to instantiate new boundaries from an
#'   existing \code{topoInputList}
#'
#' @param topoInputList must be a list with sublists of length three with the
#'   following names: \code{cellIdx, inputBoundsIdx, outputBoundsIdx}.
#'   \code{cellIdx} must be of length == 1. \code{inputBoundsIdx} and
#'   \code{outputBoundsIdx} can be vectors of length => 1.
#'
#' @return Returns a list that is of the correct dimensions to easily
#'   instantiate new boundaries efficiently via \code{Boundary$new()}.
#'
boundsInputFromTopoInput <-
  function(topoInputList, format = "list"){

    # First, create a vector of each of the boundary indicies in the network
    boundIdxs <-
      unique(
        unlist(sapply(topoInputList, '[[', "inputBoundsIdx")),
        unlist(sapply(topoInputList, '[[', "outputBoundsIdx"))
      )
    names(boundIdxs) <- boundIdxs

    # Next, figure out the cells that are upstream and downstream of each boundary
    cellsByBounds <-
      plyr::llply(
        # The first sublist returned will identify the cells upstream  of each
        # boundary.  The second sublist will identify the cells downstream of each
        # boundary.
        c("outputBoundsIdx", "inputBoundsIdx"),
        function(idx){
          # Each sublist will have the name of the boundaryIdx in the network
          plyr::llply(
            boundIdxs,
            function(b) {
              # this next bit gives the cellIdx associated with the boundary index
              unlist(
                sapply(
                  topoInputList,
                  function(subList) {
                    subList[["cellIdx"]][ b %in% subList[[idx]] ]
                  }
                )
              )
            }
          )
        }
      )
    names(cellsByBounds) <- c("upstreamCellIdx", "downstreamCellIdx")

    # generate the first iteration of the boundsDf
    boundsDf <-
      data.frame(
        boundIdx = boundIdxs,
        upstreamCellIdx = unlist(cellsByBounds[["upstreamCellIdx"]]),
        downstreamCellIdx = unlist(cellsByBounds[["downstreamCellIdx"]]),
        stringsAsFactors = FALSE
      )

    # quick error check
    if(
      any(mapply(function(sublist) {length(sublist[["outputFlowProportions"]]) != length(sublist[["outputBoundsIdx"]])}, topoInputList))
    ){
      stop("The dimensions of the outputFlowProportions do not match the outputBoundsIdx for each sublist in the input list.
           There must be a 1:1 relationship between outputBoundsIdx and outputFlowProportions in each sublist in the input list.")
    }

    # generate a vector for the flow multiplier and name it by output bounds
    proportionOfInputCellFlow <- unlist(sapply(topoInputList, "[[", "outputFlowProportions"))
    names(proportionOfInputCellFlow) <- unlist(sapply(topoInputList, "[[", "outputBoundsIdx"))

    # Add a column to the boundsDf for the proportion of input cell flow, making sure order is correct
    boundsDf$proportionOfInputCellFlow <-
      proportionOfInputCellFlow[order(match(names(proportionOfInputCellFlow), boundsDf$boundIdx))]

    boundsList <-
      purrr::transpose(boundsDf)
    if(format == "list"){
      return(boundsList)
    }
    if(format == "df"){
      return(boundsDf)
    }
  }
