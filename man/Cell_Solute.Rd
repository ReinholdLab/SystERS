% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_Cell.R
\name{Cell_Solute}
\alias{Cell_Solute}
\title{Class Cell_Solute (R6)
A cell containing a solute.  Must be linked to a water cell.}
\description{
Instantiate a \code{Cell_Solute} object. Class
  \code{Cell_Solute} inherits from class \code{Cell}.
}
\section{Super class}{
\code{\link[systERS:Cell]{systERS::Cell}} -> \code{Cell_Solute}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{concentration}}{Solute concentration in user specified units;
user must ensure consistency in units}

\item{\code{amount}}{Solute amount in user specified units (mass or mols)}

\item{\code{linkedCell}}{The water cell to which the solute cell is linked}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Cell_Solute-new}{\code{Cell_Solute$new()}}
\item \href{#method-Cell_Solute-update}{\code{Cell_Solute$update()}}
\item \href{#method-Cell_Solute-clone}{\code{Cell_Solute$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="systERS" data-topic="Cell" data-id="populateDependencies"><a href='../../systERS/html/Cell.html#method-Cell-populateDependencies'><code>systERS::Cell$populateDependencies()</code></a></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cell_Solute-new"></a>}}
\if{latex}{\out{\hypertarget{method-Cell_Solute-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cell_Solute$new(..., linkedCell, concentration)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Parameters inherit from Class \code{\link{Cell}}}

\item{\code{linkedCell}}{the cell containing the water in which this solute is located}

\item{\code{concentration}}{the concentration of the solute in user specified units
(mass or mols per unit volume)}

\item{\code{cellIdx}}{Character string denoting the index for the cell}

\item{\code{processDomain}}{Character string indicating process domain of cell (soil, groundwater, or stream)}

\item{\code{currency}}{Character string with either water or name of solute}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The object of class \code{Cell_Solute}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cell_Solute-update"></a>}}
\if{latex}{\out{\hypertarget{method-Cell_Solute-update}{}}}
\subsection{Method \code{update()}}{
Runs the update method on all cells of class
  \code{Cell_Solute}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cell_Solute$update()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Updates cell values based on trades and stores.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cell_Solute-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Cell_Solute-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cell_Solute$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
