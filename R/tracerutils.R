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



#' Quickly plot neurons from CATMAID
#'
#' Retrieve one or more neurons from CATMAID and plot them, along with one or more CATMAID volumes if desired
#'
#' @param skid Required; the skeleton ID of the neuron in CATMAID. Can accept a vector with multiple SKIDs.
#' @param volumes The names of the volumes in CATMAID to be plotted.  Defaults to NULL.
#' @param ncol The colour(s) to use when plotting the specified neuron(s).
#'     If a vector of colour names is provided, the colours will be applied to the neurons in the order specified,
#'     repeating from the beginning if there are more neurons than colours.
#'     Uses default from NAT package if unspecified.
#' @param vcol The colour(s) to use when plotting the specified volume(s).
#'     Behaves the same way as ncol.  Defaults to gray.
#' @param valpha The alpha value(s) to use when plotting volume(s).
#'     Behaves the same way as ncol and vcol.  Defaults to 0.5.
#' @return Returns the neuron(s) retrieved from CATMAID
#'
#' @export
#'
#TODO - connectors option?
plot_catmaid <- function(skid, volumes = NULL, ncol = NULL, vcol = NULL, valpha = NULL){#single skid as numeric, multiples in character vector
  #packages()
  neurons = catmaid::read.neurons.catmaid(skid)
  rgl::plot3d(neurons, WithConnectors = F, soma = T, col = ncol)#ERROR - plotting root node as soma
  if (!is.null(volumes)){
    plotVolumes(volumes, vcol, valpha)
  }

  invisible(neurons)#returned in case you want to do anything else with them, but not printed to console
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






#-----INTERNAL METHODS-----
plotVolumes <- function(volumes, vcol, valpha, pid = 1){#plot multiple neuropil volumes at once in same space as CATMAID neurons - get volumes from catmaid sever?
  vols.df = catmaidVolsAsDF()
  volumes.ids = vols.df[match(volumes, vols.df$name), 'id']#preserves order, c.f. vols.df$name %in% volumes

  #fill out colour and alpha specifications
  if (!is.null(vcol)){
    if (length(vcol) < length(volumes)) vcol = rep(vcol, length.out = length(volumes))
  }
  else{
    vcol = rep("gray", length(volumes))
  }

  if (!is.null(valpha)){
    if (length(valpha) < length(volumes)) valpha = rep(valpha, length.out = length(volumes))
  }
  else{
    valpha = rep(0.5, length(volumes))
  }


  for (id in 1:length(volumes.ids)){
    if (is.na(volumes.ids[id])){
      warning(paste("Volume \"", volumes[id], "\" was not found.", sep = ""))
      next
    }
    urlstr = paste0(pid, URL.volumes, as.character(volumes.ids[id]))
    details = catmaid::catmaid_fetch(urlstr)
    #horrible string parsing
    v1 = details$mesh
    v2 = strsplit(v1, "<")
    v3 = strsplit(v2[[1]][3], "Coordinate point='", fixed=TRUE)
    v4 = strsplit(v3[[1]][2], " ")
    v5 = as.numeric(v4[[1]])
    #split into groups of 3
    seq = seq_along(v5)
    points = split(v5, ceiling(seq/3))

    len = length(points)
    x = numeric(len)
    y = numeric(len)
    z = numeric(len)
    for (i in 1:len){
      x[i] = points[[i]][1]
      y[i] = points[[i]][2]
      z[i] = points[[i]][3]
    }
    points.df = data.frame(x,y,z)
    points.matrix = data.matrix(points.df)#go straight from vectors instead?


    rgl::triangles3d(x = points.df[,'x'], y = points.df[, 'y'], z = points.df[, 'z'], col = vcol[id], alpha = valpha[id])

  }
}

catmaidVolsAsDF <- function(pid = 1){

  vols = catmaid::catmaid_fetch(paste0(pid, URL.volumes))
  l = length(vols)

  comment = character(l)
  name = character(l)
  creation_time = character(l)
  edition_time = character(l)
  project = integer(l)
  user = integer(l)
  id = integer(l)
  editor = integer(l)

  for (i in 1:l){#there has got to be a better way of doing this...
    row = vols[[i]]
    comment[i] = if (!is.null(row$comment)) row$comment else ""
    name[i] = if (!is.null(row$name)) row$name else ""
    creation_time[i] = if (!is.null(row$creation_time)) row$creation_time else ""
    edition_time[i] = if (!is.null(row$edition_time)) row$edition_time else ""
    project[i] = row$project
    user[i] = row$user
    id[i] = row$id
    editor[i] = row$editor
  }

  vols.df = data.frame(comment, name, creation_time, edition_time, project, user, id, editor, stringsAsFactors = FALSE)

}
