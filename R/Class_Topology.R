#' @title Class Topology (R6)
#'
#' @description Define the network topology of cells and boundaries
#'
#' @export
#'
#' @return The ojbect of class \code{Topology}.
#'
#' @param topoList At this point, \code{topoList} needs to be a list with
#'   sublists wherein the first item is the \code{Cell} index, the second item
#'   is the input/upstream/upnetwork \code{Boundary} index, and the third item
#'   is the output/downstream/downnetwork \code{Boundary} index.

Topology <-
  R6::R6Class(
    classname = "Topology",

    public =
      list(
        topoList = NULL,

        initialize =
          function(topoList) {

            self$topoList <- topoList
          }
      )
  )


## Need to add error checks to ensure that sublist names and structures are
## appropriate and that the network structure is appropriate
