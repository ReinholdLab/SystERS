#' @title Class Outputter (R6)
#' Outputs values from systERS Models
#' @description Outputs values from systERS Models
#' @export
#'
#'
Outputter <-
  R6::R6Class(
    classname = "Outputter",

    public =
      list(
        #' @field model The systERS model object
        model = NULL,
        #' @field objectClassName The class of systERS model object to extract
        #'   (either Cells or Bounds)
        objectClassName = NULL,
        #' @field attributesToReport The names of objects associated with the
        #'   systERS cells or boundaries to output
        attributesToReport = NULL,
        #' @field objectsToReport The values of the systERS cells or
        #'   boundaries to output
        objectsToReport = NULL,
        #' @field reportingInterval A number indicating the reporting interval.
        #'   A value of \code{1} will report every timestep whereas a value of
        #'   \code{10} will report every 10th timestep.
        reportingInterval = NULL,
        #' @field filePath The string pointing to the place where the output
        #'   files will be written.
        filePath = NULL,
        #' @field destination The string with the full file path and file name.
        destination = NULL,

        #' @description Create an outputter
        #' @param model The systERS model object
        #' @param objectClassName String with the class name of either the cells
        #'   or boundaries on which to report
        #' @param attributesToReport Vector of strings containing the names of
        #'   the attributes to report
        #' @param reportingInterval Numeric indicating how often model should report values
        #' @param filePath String with the location for the output file to be stored
        #' @return NULL
        initialize =
          function(
            model,
            objectClassName,
            attributesToReport,
            reportingInterval,
            filePath
          ){

            self$model <- model
            self$reportingInterval <- reportingInterval

            self$objectClassName <- objectClassName
            self$attributesToReport <- attributesToReport

            self$filePath <- filePath

            self$destination <- paste0(filePath, "/", objectClassName, "_", reportingInterval, "int", ".csv")

            if(grepl("Cell", objectClassName)){
              systERSObjects <- model$cells
            } else if(grepl("Boundary", objectClassName)){
              systERSObjects <- model$bounds
            }

            self$objectsToReport <- self$getCellsOrBoundsByClass(systERSObjects = systERSObjects, objectClassName = self$objectClassName)
          },


        #' @method Method Outputter$report
        #' @description Report the output
        #' @return Write output to file
        report = function(){

          # Should this iteration be reported?  Check by dividing the
          # iteration number by the reporting interval.  If the remainder is
          # 0, then we want to output it.  If not, then return NULL.
          if(self$model$iterationNum %% self$reportingInterval == 0){

            outList <-
              lapply(self$attributesToReport, function(attribute) self$getCellOrBoundAttributes(self$objectsToReport, attribute) )
            names(outList) <- self$attributesToReport

            outDf <- as.data.frame(outList)
            outDf$objectName <- row.names(outDf)

            outDf$timeStepIdx <- self$model$iterationNum
            outDf$modelTime <- outDf$timeStepIdx * self$model$timeInterval

            writeHeadersBoolean <- ifelse(!file.exists(self$destination), TRUE, FALSE)
            write.table(outDf, file = self$destination, sep = ",", append = TRUE, quote = FALSE,
                        col.names = writeHeadersBoolean, row.names = FALSE)
          } else{
            NULL
          }
        },


        #' @method Method Outputter$getCellsOrBoundsByClass
        #' @description Gets cells or boundaries in the model by their class
        #' @return Selected cells or boundaries
        getCellsOrBoundsByClass = function(systERSObjects, objectClassName){
          matches <- sapply(systERSObjects, is, class2 = objectClassName)
          return(systERSObjects[matches])
        },
        #' @method Method Outputter$getCellOrBoundAttributes
        #' @description Gets attributes of cells or boundaries based on the boundary name
        #' @return Selected attributes of either cells or boundaries
        getCellOrBoundAttributes = function(systERSObjects, attributeName){
          attribs <- sapply(systERSObjects, "[[", attributeName)
          return(attribs)
        },

        #' @method Method Outputter$timeSeriesGraph
        #' @description Creates a of outputs through time.
        #' @return Graph
        timeSeriesGraph = function(systERSobject, attributeName, outputFilePathAndName){
          browser()
           plotDat <- read.csv(outputFilePathAndName, head = TRUE)
          theField <- objects(systERSobject)[grepl("(boundaryIdx|cellIdx)", objects(systERSobject))]
          theFieldName <- systERSobject[[theField]]
          plotDat <- plotDat[plotDat$objectName == theFieldName, ]
          yVals <- plotDat[attributeName]
          xVals <- plotDat["modelTime"]
          return(plot(x = xVals[,1], y = yVals[,1], type = "l", ylab = attributeName, xlab = "Model time", main = theFieldName))
        }

      )
  )







