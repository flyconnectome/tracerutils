#-----REMOVED FUNCTIONS-----
#' Removed functions
#' @name tracerutils-defunct
NULL


#' NBLAST a CATMAID neuron against the flycircuit data set
#'
#' DEFUNCT - use \code{\link{nblast_fafb}}
#'
#' Requires the \code{flycircuit.datadir} option to be set and the \code{nat.default.neuronlist} option to be set to "dps" in .Rprofile
#'
#' @rdname tracerutils-defunct
#'
#' @param skid The skeleton ID of the neuron in CATMAID
#' @return The full NBLAST results object
#'
#' @export
quick_nblast <- function(skid){
  .Defunct("elmr::nblast_fafb", package = "tracerutils", "This method has been removed; please use elmr::nblast_fafb instead.")
}
