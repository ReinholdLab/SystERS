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
        tradeTable = NULL,
        initialize =
          function(cells, tradeTable){
            masterTable <- tradeTable

            currencies <- c("H2O", "NO3")

            stores <- vector("list", length = 2)
            names(stores) <- currencies

            for(curr in currencies){

              tradeTable <- masterTable[masterTable$tradeCurrency == curr, ]

              usCellIdxs <- unique(as.character(tradeTable$usCellIdx))
              dsCellIdxs <- unique(as.character(tradeTable$dsCellIdx))
              cellIdxs <- sort(unique(c(usCellIdxs, dsCellIdxs)))
              # orderingVect <- match(cellIdxs, unlist(plyr::llply(wq.pl$cells, function(cell) return(cell$cellIdx))))
              outVal <- rep(0, length(cellIdxs)) #vect of zeros same length as cellIdxs
              inVal <- outVal #vect of zeros same length as outVol and cellIdxs

              valToGrab <- ifelse(curr == "H2O", "channelVolume_L", "soluteMass")
              startVal <- sapply(cells, function(c) c[[valToGrab]])[match(cellIdxs, sapply(cells, function(c) c$cellIdx))]

              for(i in 1:length(cellIdxs)){
                if(any(cellIdxs[i] %in% tradeTable$usCellIdx)){
                  outVal[i] <- sum(tradeTable$tradeVals[tradeTable$usCellIdx == cellIdxs[i]])
                }
                if(any(cellIdxs[i] %in% tradeTable$dsCellIdx)){
                  inVal[i] <- sum(tradeTable$tradeVals[tradeTable$dsCellIdx == cellIdxs[i] & tradeTable$tradeType != "remove" ])
                }
              }
              endVal <- startVal - outVal + inVal

              storeTable <- cbind(startVal, outVal, inVal, endVal)
              row.names(storeTable) <- cellIdxs

              stores[[curr]] <- storeTable
            }
            self$stores <- stores
          }
      )
  )
