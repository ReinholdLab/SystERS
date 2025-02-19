% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_systERSModel.R
\name{systERSModel}
\alias{systERSModel}
\title{Class systERSModel (R6)
A water quality model}
\description{
Define and instantiate the systERS model and the network topology of
  cells and boundaries
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{cells}}{The model cells, stored in a list}

\item{\code{bounds}}{The model boundaries, stored in a list}

\item{\code{timeInterval}}{The model time step}

\item{\code{boundsTransportTable_water_int}}{Table of internal water
boundary specifications}

\item{\code{boundsTransportTable_water_ext}}{Table of external water
boundary specifications}

\item{\code{boundsTransportTable_solute_int}}{Table of internal solute
boundary specifications}

\item{\code{boundsTransportTable_solute_us}}{Table of external, upstream
solute boundary specifications}

\item{\code{boundsTransportTable_solute_ds}}{Table of external, downstream
solute boundary specifications}

\item{\code{boundsReactionTable_solute_int}}{Table of internal solute
boundary specifications}

\item{\code{cellsTable_water_stream}}{Table with the names of the water
cells and their attributes in the stream processing domain}

\item{\code{cellsTable_solute_stream}}{Table with the names of the solute
cells and their attributes in the stream processing domain}

\item{\code{cellsTable_water_soil}}{Table with the names of the water
cells and their attributes in the soil processing domain}

\item{\code{cellsTable_solute_soil}}{Table with the names of the solute
cells and their attributes in the soil processing domain}

\item{\code{cellsTable_water_groundwater}}{Table with the names of the water
cells and their attributes in the groundwater processing domain}

\item{\code{cellsTable_solute_groundwater}}{Table with the names of the
solute cells and their attributes in the groundwater processing
domain}

\item{\code{cellsTableList}}{List storing all of the cell tables}

\item{\code{boundsTableList}}{List storing all of the cell tables}

\item{\code{solute_transport_df}}{A data frame created from the solute
transport boundary specifications}

\item{\code{unitsTable}}{Table of units for model parameters and outputs}

\item{\code{iterationNum}}{How many times the model has iterated}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-systERSModel-new}{\code{systERSModel$new()}}
\item \href{#method-systERSModel-cellFactory}{\code{systERSModel$cellFactory()}}
\item \href{#method-systERSModel-boundaryFactory}{\code{systERSModel$boundaryFactory()}}
\item \href{#method-systERSModel-errorCheckCellInputs}{\code{systERSModel$errorCheckCellInputs()}}
\item \href{#method-systERSModel-initializeWaterCells_stream}{\code{systERSModel$initializeWaterCells_stream()}}
\item \href{#method-systERSModel-initializeSoluteCells_stream}{\code{systERSModel$initializeSoluteCells_stream()}}
\item \href{#method-systERSModel-linkSoluteCellsToWaterCells_stream}{\code{systERSModel$linkSoluteCellsToWaterCells_stream()}}
\item \href{#method-systERSModel-errorCheckBoundaryInputs}{\code{systERSModel$errorCheckBoundaryInputs()}}
\item \href{#method-systERSModel-initializeExternalWaterTransportBoundaries}{\code{systERSModel$initializeExternalWaterTransportBoundaries()}}
\item \href{#method-systERSModel-initializeInternalWaterTransportBoundaries}{\code{systERSModel$initializeInternalWaterTransportBoundaries()}}
\item \href{#method-systERSModel-initializeSoluteTransportBoundaries}{\code{systERSModel$initializeSoluteTransportBoundaries()}}
\item \href{#method-systERSModel-initializeSoluteReactionBoundaries}{\code{systERSModel$initializeSoluteReactionBoundaries()}}
\item \href{#method-systERSModel-linkBoundsToCells}{\code{systERSModel$linkBoundsToCells()}}
\item \href{#method-systERSModel-trade}{\code{systERSModel$trade()}}
\item \href{#method-systERSModel-store}{\code{systERSModel$store()}}
\item \href{#method-systERSModel-update}{\code{systERSModel$update()}}
\item \href{#method-systERSModel-iterate}{\code{systERSModel$iterate()}}
\item \href{#method-systERSModel-clone}{\code{systERSModel$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-new"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$new(
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
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{boundsTransportTable_water_int}}{Table with the names of the
water boundaries and their attributes for water boundaries that are
internal to the model, i.e., those with exactly one upstream and
one downstream cell}

\item{\code{boundsTransportTable_water_ext}}{Table with the names of the
water boundaries and their attributes for water boundaries at the
most upstream and most downstream extent of the model topology}

\item{\code{boundsTransportTable_solute_int}}{Table with the names of the
solute boundaries and their attributes for solute boundaries that
are internal to the model, i.e., those with exactly one upstream
and one downstream cell}

\item{\code{boundsTransportTable_solute_us}}{Table with the names of the
solute boundaries and their attributes for solute boundaries at the
most upstream extent of the model topology}

\item{\code{boundsTransportTable_solute_ds}}{Table with the names of the
solute boundaries and their attributes for solute boundaries at the
most downstream extent of the model topology}

\item{\code{boundsReactionTable_solute_int}}{Table with the names of the
solute reaction boundaries and their attributes}

\item{\code{cellsTable_water_stream}}{Table with the names of the water
cells and their attributes in the stream processing domain}

\item{\code{cellsTable_solute_stream}}{Table with the names of the solute
cells and their attributes in the stream processing domain}

\item{\code{cellsTable_water_soil}}{Table with the names of the water
cells and their attributes in the soil processing domain}

\item{\code{cellsTable_solute_soil}}{Table with the names of the solute
cells and their attributes in the soil processing domain}

\item{\code{cellsTable_water_groundwater}}{Table with the names of the water
cells and their attributes in the groundwater processing domain}

\item{\code{cellsTable_solute_groundwater}}{Table with the names of the solute
cells and their attributes in the groundwater processing domain}

\item{\code{unitsTable}}{Table of units for model parameters and outputs}

\item{\code{timeInterval}}{Model time step}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The object of class \code{systERSModel}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-cellFactory"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-cellFactory}{}}}
\subsection{Method \code{cellFactory()}}{
Create a list of the model cells
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$cellFactory()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of model cells
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-boundaryFactory"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-boundaryFactory}{}}}
\subsection{Method \code{boundaryFactory()}}{
Create a list of the model boundaries
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$boundaryFactory()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of model boundaries
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-errorCheckCellInputs"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-errorCheckCellInputs}{}}}
\subsection{Method \code{errorCheckCellInputs()}}{
Error check to see if any cell specifications are
  duplicated
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$errorCheckCellInputs()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Nothing if no error is detected.  An error message is
  returned if an error is detected.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeWaterCells_stream"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeWaterCells_stream}{}}}
\subsection{Method \code{initializeWaterCells_stream()}}{
Instantiate the water cells in the stream process domain
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeWaterCells_stream()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
List of stream water cells
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeSoluteCells_stream"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeSoluteCells_stream}{}}}
\subsection{Method \code{initializeSoluteCells_stream()}}{
Instantiate the solute cells in the stream process domain
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeSoluteCells_stream()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
List of stream solute cells
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-linkSoluteCellsToWaterCells_stream"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-linkSoluteCellsToWaterCells_stream}{}}}
\subsection{Method \code{linkSoluteCellsToWaterCells_stream()}}{
Link the solute cells to the water cells in the stream process
  domain
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$linkSoluteCellsToWaterCells_stream()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Water cells with their \code{linkedSoluteCells} attribute populated
  with a list of solute cells that are linked to the water cell
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-errorCheckBoundaryInputs"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-errorCheckBoundaryInputs}{}}}
\subsection{Method \code{errorCheckBoundaryInputs()}}{
Error check to see if any boundary specifications are duplicated
  or have references to cells that do not exist.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$errorCheckBoundaryInputs()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Nothing if no error is detected.  If an error is detected, a message
  is thrown.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeExternalWaterTransportBoundaries"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeExternalWaterTransportBoundaries}{}}}
\subsection{Method \code{initializeExternalWaterTransportBoundaries()}}{
Instantiate the transport boundaries that are on the physical
  edges of the model
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeExternalWaterTransportBoundaries()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Water transport boundaries at the upstream and downstream extents of
  the model topology
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeInternalWaterTransportBoundaries"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeInternalWaterTransportBoundaries}{}}}
\subsection{Method \code{initializeInternalWaterTransportBoundaries()}}{
Instantiate the transport boundaries that are internal to the
  physical edges of the model
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeInternalWaterTransportBoundaries()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Water transport boundaries within the upstream and downstream extents
  of the model topology
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeSoluteTransportBoundaries"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeSoluteTransportBoundaries}{}}}
\subsection{Method \code{initializeSoluteTransportBoundaries()}}{
Instantiate the solute transport boundaries
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeSoluteTransportBoundaries()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Solute transport boundaries
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-initializeSoluteReactionBoundaries"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-initializeSoluteReactionBoundaries}{}}}
\subsection{Method \code{initializeSoluteReactionBoundaries()}}{
Instantiate the solute reaction boundaries.  Current
  model version only supports stream reaction boundaries.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$initializeSoluteReactionBoundaries()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of solute reaction boundaries
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-linkBoundsToCells"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-linkBoundsToCells}{}}}
\subsection{Method \code{linkBoundsToCells()}}{
Creates a list of boundaries attached to each cell and
  then adds the list of bounds connected to each cell as an attribute
  of the cell.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$linkBoundsToCells()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Cells with \code{linkedBoundsList} attribute populated
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-trade"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-trade}{}}}
\subsection{Method \code{trade()}}{
Runs the trade method on all boundaries in the model in
  the order in which they occur in the \code{bounds} list.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$trade()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Updated boundary values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-store"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-store}{}}}
\subsection{Method \code{store()}}{
Runs the store method on all cells in the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$store()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Updated store values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-update"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-update}{}}}
\subsection{Method \code{update()}}{
Runs the update method on all cells and boundaries in the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$update()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Updates all values in cells and boundaries based on trades and stores.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-iterate"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-iterate}{}}}
\subsection{Method \code{iterate()}}{
Iterates the model by calling all trades, stores, and
  updates.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$iterate()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
All cells and boundaries will values updated to reflect the
  trades, stores, and updates that occurred during the time step.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-systERSModel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-systERSModel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{systERSModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
