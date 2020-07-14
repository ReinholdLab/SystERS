#' @title CalcStores
#'
#' @description Calculate the mass balances for water and solute.
#'
#' @param cells A list of cells in the model.
#' @param tradeTable A table with the neccessary trade information to calculate
#'   the stores.
#'
#' @return A list containing 2 tables--one corresponding to H2O and the other
#'   NO3--with mass balances for each cell for each currency.
#'

CalcStores <-
  R6::R6Class(
    classname = "CalcStores",
    public =
      list(
        stores = NULL,
        cells = NULL,
        bounds = NULL,
        tradeTable = NULL,
        currencies = NULL,
        initialize =
          function(cells, bounds){

            self$cells <- cells
            self$bounds <- bounds

            self$currencies <- unique(sapply(bounds, function(b) b$currency))

            self$stores <- vector("list", length = length(self$currencies))
            names(self$stores) <- self$currencies

          } # end initialize
      ) # close public
  ) # close class def

CalcStores$set(
  which = "public",
  name = "doCalc",
  value = function(tradeTable, initStores){

    masterTable <- tradeTable
    masterTable$cellAttrToCalc <- ifelse(masterTable$tradeCurrency == "H2O", "channelVolume_L", "soluteMass")

    plyr::llply(1:nrow(masterTable), function(rw) {
      # if there is an upstream cell (i.e., not an upstream model boundary, then do the calc for that cell
      if(is.environment(masterTable[rw,]$usCell[[1]]) ){
        startVal <- masterTable[rw,]$usCell[[1]][[masterTable[rw,]$cellAttrToCalc]]
        masterTable[rw,]$usCell[[1]][[masterTable[rw,]$cellAttrToCalc]] <- startVal - masterTable[rw,]$tradeVals
      }
      # if there is downstream cell (i.e., not an d/s model boundary, then do the calc for that cell
      if(is.environment(masterTable[rw,]$usCell[[1]]) ){
        startVal <- masterTable[rw,]$usCell[[1]][[masterTable[rw,]$cellAttrToCalc]]
        masterTable[rw,]$usCell[[1]][[masterTable[rw,]$cellAttrToCalc]] <- startVal + masterTable[rw,]$tradeVals
      }
    }
    )

    return()
  } # close function
) # close def

# Old doCalc code below:
# CalcStores$set(
#   which = "public",
#   name = "doCalc",
#   value = function(tradeTable, initStores){
#
#     masterTable <- tradeTable
#
#     for(curr in initStores$currencies){
#       tradeTable <- masterTable[masterTable$tradeCurrency == curr, ]
#
#       usCellIdxs <- unique(as.character(tradeTable$usCellIdx[!is.na(tradeTable$usCellIdx)]))
#       dsCellIdxs <- unique(as.character(tradeTable$dsCellIdx[!is.na(tradeTable$dsCellIdx)]))
#       cellIdxs <- sort(unique(c(usCellIdxs, dsCellIdxs)))
#       # orderingVect <- match(cellIdxs, unlist(plyr::llply(wq.pl$cells, function(cell) return(cell$cellIdx))))
#       outVal <- rep(0, length(cellIdxs)) #vect of zeros same length as cellIdxs
#       inVal <- outVal #vect of zeros same length as outVol and cellIdxs
#
#       valToGrab <- ifelse(curr == "H2O", "channelVolume_L", "soluteMass")
#       startVal <- sapply(initStores$cells, function(c) c[[valToGrab]])[match(cellIdxs, sapply(initStores$cells, function(c) c$cellIdx))]
#
#       for(i in 1:length(cellIdxs)){
#         if(any(cellIdxs[i] %in% tradeTable$usCellIdx)){
#           crit <- tradeTable$usCellIdx == cellIdxs[i]
#           crit[is.na(crit)] <- FALSE # get rid of the NAs so you can query
#           outVal[i] <- sum(tradeTable$tradeVals[crit])
#         }
#         if(any(cellIdxs[i] %in% tradeTable$dsCellIdx)){
#           crit <- tradeTable$dsCellIdx == cellIdxs[i]
#           crit[is.na(crit)] <- FALSE
#           inVal[i] <- sum(tradeTable$tradeVals[crit & tradeTable$tradeType != "remove" ])
#         }
#       }
#       endVal <- startVal - outVal + inVal
#
#       storeTable <- cbind(startVal, outVal, inVal, endVal)
#       row.names(storeTable) <- cellIdxs
#
#       #populate stores list
#       initStores$stores[[curr]] <- storeTable
#     } # end currency for loop
#     return(initStores$stores)
#   } # close function
# ) # close def
