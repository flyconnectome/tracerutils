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

  if(reference == "FCWB"){ neuropils = elmr::FCWBNP.surf$RegionList }
  else{ neuropils = elmr::FAFBNP.surf$RegionList }

  if (missing(neurons)){
    if(reference == "FCWB"){ neurons = fetchn_fafb(skids, mirror = FALSE, reference = elmr::FCWB) }
    else{ neurons = read.neurons.catmaid(skids) }
  }

  summaries = list()

  for (n in 1:length(neurons)){
    neuron = neurons[[n]]

    neuron.outgoing = neuron$connectors[neuron$connectors$prepost == 0,]
    neuron.incoming = neuron$connectors[neuron$connectors$prepost == 1,]


    outgoing = sapply(neuropils, function(x){if(!is.null(neuron.outgoing)){ INTERNAL_count_synapses_in_mesh(neuron.outgoing, x, reference)} else{ 0 }})
    incoming = sapply(neuropils, function(x){if(!is.null(neuron.incoming)){ INTERNAL_count_synapses_in_mesh(neuron.incoming, x, reference)} else{ 0 }})

    summary = data.frame(outgoing = outgoing, incoming = incoming)#neuropils given as row names from sapply
    summaries[[n]] = summary
  }

  names(summaries) = names(neurons)
  return(summaries)

}

INTERNAL_count_synapses_in_mesh <- function(connectors, neuropil, reference){
  if(reference == "FCWB"){ ref.brain = elmr::FCWBNP.surf }#doesn't work as an ifelse?
  else{ ref.brain = elmr::FAFBNP.surf }
  tf = pointsinside(connectors[,c("x", "y", "z")], subset(ref.brain, neuropil))
  n = sum(tf, na.rm = TRUE)
  invisible(n)
}
