% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_Boundary_Transport_Water.R
\name{Boundary_Transport_Water}
\alias{Boundary_Transport_Water}
\title{Class Boundary_Transport_Water (R6)
A model boundary that transports water to and from cells}
\description{
Transport boundary for water between cells
}
\section{Super class}{
\code{\link[systERS:Boundary]{systERS::Boundary}} -> \code{Boundary_Transport_Water}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{discharge}}{Water discharge rate through the boundary in user specified units (volume/time)}

\item{\code{volume}}{Water volume passed through the boundary}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Boundary_Transport_Water-new}{\code{Boundary_Transport_Water$new()}}
\item \href{#method-Boundary_Transport_Water-store}{\code{Boundary_Transport_Water$store()}}
\item \href{#method-Boundary_Transport_Water-clone}{\code{Boundary_Transport_Water$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boundary_Transport_Water-new"></a>}}
\if{latex}{\out{\hypertarget{method-Boundary_Transport_Water-new}{}}}
\subsection{Method \code{new()}}{
Instantiate a water transport boundary
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Boundary_Transport_Water$new(..., discharge)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Parameters inherit from Class \code{\link{Boundary}}}

\item{\code{discharge}}{Rate of water discharge (a.k.a. Q)}

\item{\code{boundaryIdx}}{String indexing the boundary}

\item{\code{currency}}{String naming the currency handled by the boundary as a character e.g., \code{water, NO3}}

\item{\code{upstreamCell}}{Cell (if one exists) upstream of the boundary}

\item{\code{downstreamCell}}{Cell (if one exists) downstream of the boundary}

\item{\code{timeInterval}}{Model time step}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A model boundary that transports water
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boundary_Transport_Water-store"></a>}}
\if{latex}{\out{\hypertarget{method-Boundary_Transport_Water-store}{}}}
\subsection{Method \code{store()}}{
Runs the store method on water cells in the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Boundary_Transport_Water$store()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Updated store values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Boundary_Transport_Water-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Boundary_Transport_Water-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Boundary_Transport_Water$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
