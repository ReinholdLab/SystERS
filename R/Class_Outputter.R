#' @title Class Outputter (R6)
#' Outputs values from S2S Models
#' @description Outputs values from S2S Models
#' @export
#'
#'
Outputter <-
  R6::R6Class(
    classname = "Outputter",

    public =
      list(
        #' @field model The S2S model object
        model = NULL,
        #' @field objectClassName The class of S2S model object to extract
        #'   (either Cells or Bounds)
        objectClassName = NULL,
        #' @field attributesToReport The attributes of the S2S cells or
        #'   boundaries to output
        attributesToReport = NULL,
        #' @field reportingInterval A number indicating the reporting interval.
        #'   A value of \code{1} will report every timestep whereas a value of
        #'   \code{10} will report every 10th timestep.
        reportingInterval = NULL,
        #' @field filePath The string pointing to the place where the output
        #'   files will be written.
        filePath = NULL,

        #' @description Create an outputter
        #' @param model The S2S model object
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

            browser()
            self$model <- model
            self$objectClassName <- objectClassName
            self$attributesToReport <- attributesToReport
            self$reportingInterval <- reportingInterval
            self$filePath <- filePath

            destination <- paste0(filePath, "/", objectClassName, "_", reportingInterval, "int", ".csv")

            if(grepl("Cell", objectClassName)){
              s2sObjects <- model$cells
            } else if(grepl("Boundary", objectClassName)){
              s2sObjects <- model$bounds
            }

            objectsToReport <- self$getCellsOrBoundsByClass(s2sObjects = s2sObjects, objectClassName = self$objectClassName)
            outList <-
              lapply(attributesToReport, function(attribute) self$getCellOrBoundAttributes(objectsToReport, attribute) )
            names(outList) <- attributesToReport

            ############## Stopped here 11/19/2020


          },
        #' @method Method Outputter$getCellsOrBoundsByClass
        #' @description Gets cells or boundaries in the model by their class
        #' @return Selected cells or boundaries
        getCellsOrBoundsByClass = function(s2sObjects, objectClassName){
          matches <- sapply(s2sObjects, is, class2 = objectClassName)
          return(s2sObjects[matches])
        },
        #' @method Method Outputter$getCellOrBoundAttributes
        #' @description Gets attributes of cells or boundaries based on the boundary name
        #' @return Selected attributes of either cells or boundaries
        getCellOrBoundAttributes = function(s2sObjects, attributeName){
          attribs <- sapply(s2sObjects, "[[", attributeName)
          return(attribs)
        }
      )
  )







