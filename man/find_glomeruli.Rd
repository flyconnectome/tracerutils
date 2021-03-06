% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_glomeruli}
\alias{find_glomeruli}
\title{Finds the glomerulus associated with a PN}
\usage{
find_glomeruli(skids, exclude = NULL, method = c("annotation", "name"),
  ...)
}
\arguments{
\item{skids}{Required; an \code{integer} or \code{character} vector of skeleton IDs}

\item{exclude}{Optional; a vector of glomeruli to exclude (in the format \bold{\emph{X}} for \bold{glomerulus \emph{X}}, or the full annotation for \bold{unknown glomerulus \emph{N}})}

\item{method}{Optional; indicating whether to use the neuron 'name' or glomerulus 'annotation's.}

\item{...}{Additional arguments passed to catmaid_* functions. Use this to specify \code{conn}, \code{pid} or similar low level arguments for the catmaid_query.}
}
\value{
A \code{character} vector of glomeruli names
}
\description{
Given a vector of skeleton IDs, this will find the glomerului associated with PNs, based on annotations in CATMAID.
}
\details{
Requires the PN to be annotated with \bold{glomerulus \emph{X}} or \bold{unknown glomerulus \emph{N}} in CATMAID.  Annotations of the form \bold{glomerulus \emph{X}}
will be prioritised over \bold{unknown glomerulus \emph{N}}, and if there are multiples they will be joined together with a forward slash.  If there are
no results with either of these annotations, the string \code{"unknown"} will be returned in place of a glomerulus.
Note that annotations of the form \bold{glomerulus \emph{X} right|left} are not considered.
}
\examples{
\donttest{
find_glomeruli('WTPN2017_uPN_right')
find_glomeruli('WTPN2017_uPN_right', exclude=c('VCx','VCy', 'VP1l'))
find_glomeruli('WTPN2017_uPN_right', method = 'name')
}

}
