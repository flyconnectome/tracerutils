#-----UTILITIES-----

#' NBLAST a CATMAID neuron against the flycircuit data set
#'
#' NBLAST a neuron in CATMAID (FAFB) against the full flycircuit neuron list
#'
#' Requires the \code{flycircuit.datadir} option to be set and the \code{nat.default.neuronlist} option to be set to "dps" in .Rprofile
#'
#' @rdname tracerutils-deprecated
#'
#' @param skid The skeleton ID of the neuron in CATMAID
#' @return The full NBLAST results object
#'
#' @export
#'
quick_nblast <- function(skid){
  .Deprecated("nblast_fafb", "elmr", "quick_nblast is deprecated, please use elmr::nblast_fafb instead")

  if(!requireNamespace("doMC", quietly = TRUE)){
    warning("The NBLAST search will not be parallelised without the 'doMC' package installed.  This might take a long time to complete.")
  }
  else{
    doMC::registerDoMC(4)
  }

  dps <<- nat::read.neuronlistfh(getOption("flycircuit.datadir"))

  library(nat.nblast)# workaround to nat.nblast issue #31
  results = elmr::nblast_fafb(skid)
}



#' Split a neuron and return 'downstream' section
#'
#' Split a neuron at a particular node and return the 'downstream' section as a new neuron object
#'
#' This does not actually split the neuron in CATMAID; split is only performed on the local neuron object.
#'
#' @param skid Required; the skeleton ID of the neuron to split
#' @param node Required; the ID of the node where the neuron should be split
#' @return A \code{neuron} object representing the 'downstream' portion of the split neuron
#'
#' @export
#'
split_neuron_local <- function(skid, node, return = "child"){#split local copy of a neuron at a particular node, and return result ('child' by default) as a neuron object
  neuron = catmaid::read.neuron.catmaid(skid)
  index = match(node, neuron$d$PointNo)#add error handling
  neuron.distal = elmr::distal_to(neuron, index)
  neuron.distal.points = neuron$d[neuron.distal,]

  #child neuron
  segs = sapply(neuron$SegList, function(x) all(x %in% neuron.distal))#add truncated partial segments
  segs.old = segs.old = neuron$SegList[segs]
  segs.new = segs.new = sapply(segs.old, function(x) sapply(x, function(y) y = match(neuron$d$PointNo[y], neuron.distal.points$PointNo)))
  #tags = sapply(neuron$tags, function(x) x %in% neuron.distal.points$PointNo)
  new = nat::neuron(d = neuron.distal.points,
               NumPoints = nrow(neuron.distal.points),
               StartPoint = 1,#check this
               BranchPoints = neuron$BranchPoints[neuron$BranchPoints %in% neuron.distal],
               EndPoints = neuron$EndPoints[neuron$EndPoints %in% neuron.distal],
               SegList = segs.new,
               NeuronName = paste0("SKID ", skid, " downstream of node ", node),
               #...
               connectors = neuron$connectors[neuron$connectors$treenode_id %in% neuron.distal.points$PointNo,]
               #tags =
               )

}



#' Generate a CATMAID URL for a node or connector
#'
#' Simple method for generating a CATMAID URL pointing to a particular skeleton node or connector
#'
#' Requires the \code{catmaid.server} option to be set in .Rprofile
#'
#' @param dfrow Required; a single data frame row containing columns \code{x}, \code{y}, \code{z}, and (optionally) \code{connector_id} with CATMAID coordinates and the connector ID
#' @param skid Required; the skid of the active skeleton
#' @param sid0 The ID of the desired stack mirror on your CATMAID server.  Found by generating a URL within CATMAID and picking out the \code{sid0} param.  Defaults to 5.
#' @param zoom The desired zoom level (\code{s0} parameter) of the URL.  Defaults to 0.
#' @param conn Whether or not to specify an active connector ID in the URL.  Defaults to \code{FALSE}.
#' @return A \code{character} string with the CATMAID URL.
#'
#' @export
#'
simple_catmaid_url <- function(dfrow, skid, sid0 = 5, zoom = 0, conn = FALSE){ #takes row of a data frame with columns for x, y, z, and (optionally) connector_id; skid set for neuron and sid0 for stack mirror
  base = getOption('catmaid.server')

  catmaid_url = paste0(base, "?pid=1")
  catmaid_url = paste0(catmaid_url, "&zp=", dfrow["z"])
  catmaid_url = paste0(catmaid_url, "&yp=", dfrow["y"])
  catmaid_url = paste0(catmaid_url, "&xp=", dfrow["x"])
  catmaid_url = paste0(catmaid_url, "&tool=tracingtool")
  catmaid_url = paste0(catmaid_url, "&active_skeleton_id=", skid)
  if(conn == TRUE){ catmaid_url = paste0(catmaid_url, "&active_node_id=", dfrow["connector_id"]) }
  catmaid_url = paste0(catmaid_url, "&sid0=", sid0, "&s0=", zoom)

  invisible(catmaid_url)
}
