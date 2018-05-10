#' Count the number of synapses one or more neurons make in each neuropil volume
#'
#' @param skids A list of skeleton IDs; required if \code{neurons} is not provided.  Will only be used if \code{neurons} is not provided.
#' @param neurons A neuron list; required if \code{skids} is not provided.
#' @param reference The name of a reference brain to use.  Currently accepts \code{"FAFB"}, or \code{"FCWB"}; defaults to \code{"FAFB"}.
#'
#' @return Returns a list containing one data frame for each neuron/skeleton ID provided.
#' Each data frame will contain a row for each neuropil, with two columns indicating the number of outgoing and incoming synapses that the neuron has in that region.
#'
#' @export
#'
#' @importFrom elmr fetchn_fafb
#' @importFrom catmaid read.neurons.catmaid
#' @importFrom nat pointsinside
synapses_per_neuropil <- function(skids = NULL, neurons = NULL, reference = c("FAFB", "FCWB")){#TODO - automatic skid/neuron detection, expand to any template brain with neuropil segmentation

  if(missing(skids) & missing(neurons)){ stop("At least one skeleton ID or neuron must be provided.") }
  reference = match.arg(reference)

  tb = if(reference == "FCWB"){ nat.flybrains::FCWB } else{ elmr::FAFB }
  surf = if(reference == "FCWB"){ nat.flybrains::FCWBNP.surf } else{ elmr::FAFBNP.surf }
  neuropils = surf$RegionList

  if (missing(neurons))
    neurons = fetchn_fafb(skids, mirror = FALSE, reference = tb)

  sapply(neurons, function(neuron){
    neuron.outgoing = neuron$connectors[neuron$connectors$prepost == 0,]
    neuron.incoming = neuron$connectors[neuron$connectors$prepost == 1,]

    outgoing = sapply(neuropils, function(x){ if(!is.null(neuron.outgoing)){ INTERNAL_count_synapses_in_mesh(neuron.outgoing, x, surf)} else{ 0 } })
    incoming = sapply(neuropils, function(x){ if(!is.null(neuron.incoming)){ INTERNAL_count_synapses_in_mesh(neuron.incoming, x, surf)} else{ 0 } })

    summary = data.frame(outgoing = outgoing, incoming = incoming)#neuropils given as row names from sapply
   }, simplify = F)
  }

INTERNAL_count_synapses_in_mesh <- function(connectors, neuropil, surf){
  tf = pointsinside(connectors[,c("x", "y", "z")], subset(surf, neuropil))
  n = sum(tf, na.rm = TRUE)
  return(n)
}
