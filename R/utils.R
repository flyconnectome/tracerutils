#-----UTILITIES-----

#' Split a neuron and return 'downstream' section
#'
#' Split a neuron at a particular node and return the 'downstream' section as a
#' new neuron object
#'
#' This does not actually split the neuron in CATMAID; split is only performed
#' on the local neuron object. If a string is used to specify the split
#' \code{node} (rather than the CATMAID node id), it is matched against named
#' tags present in the input neuron. There will be an error if this does not
#' resolve to a single unique node.
#'
#' @param skid Required unless \code{neuron} is provided; the skeleton ID of the neuron to split (passed on to
#'   \code{\link{catmaid_skids}})
#' @param neuron Required unless \code{skid} is provided; a \code{neuron} object to split
#' @param node Required; the ID of the node where the neuron should be split (or
#'   a string naming a tag)
#' @param return Which part of the split neuron to return (\code{downstream} or \code{upstream}). Defaults to \code{downstream}.
#' @return A \code{neuron} object representing the specified portion of the
#'   split neuron
#'
#' @export
#' @examples
#' \donttest{
#' DL4.LH=split_neuron_local("glomerulus DL4 right", "SCHLEGEL_LH")
#' plot(DL4.LH)
#' }
#' @importFrom catmaid read.neuron.catmaid catmaid_skids
#' @importFrom nat prune_vertices
#' @importFrom elmr distal_to
split_neuron_local <- function(skid = NULL, neuron = NULL, node, return = c("downstream", "upstream")){#consider possibility of returning both as neuron list?
  if(missing(skid) & missing(neuron)){ stop("A skeleton ID or neuron must be provided.") }
  return = match.arg(return)

  if(!missing(skid)){ skid=catmaid_skids(skid, several.ok = FALSE) }
  if(missing(neuron)){ neuron = read.neuron.catmaid(skid) }
  if(missing(skid)){ skid = neuron$skid }
  #TODO - handle case if both skid and neuron are provided, whether or not they match

  if(is.character(node)) {
    tag=node
    node=neuron[['tags']][[tag]]
    if(!isTRUE(length(node)==1L)) {
      stop("Unable to find unique node with the tag: ", tag)
    }
  }
  neuron.distal = distal_to(neuron, node.pointno=node)

  invert = if(return == "downstream"){ TRUE } else{ FALSE }
  new=prune_vertices(neuron, neuron.distal, invert = invert)


  # give it a nice name and handle connectors and tags
  new$NeuronName = paste0("SKID ", skid, " ", return, " of node ", node)
  old_connectors = catmaid::connectors(neuron)
  new$connectors = old_connectors[old_connectors$treenode_id %in%
                                    new$d$PointNo, ]
  new$tags = lapply(neuron$tags, function(tag){
                                    nodes = unlist(sapply(tag, function(node){
                                                                  if(node %in% new$d$PointNo){ node }
                                                                  else{ NULL }
                                                                }
                                                   ))
                                    nodes = nodes[!is.null(nodes)]
                                    if(length(nodes) > 0){ nodes }
                                    else{ NULL }
                                  }
                    )
  new$tags = new$tags[sapply(new$tags, function(t){ length(t) > 0 })]
  class(new) = class(neuron)
  invisible(new)
}



#' Generate a CATMAID URL for a node or connector
#'
#' Simple method for generating a CATMAID URL pointing to a particular skeleton node or connector
#'
#' Requires the \code{catmaid.server} option to be set in .Rprofile
#'
#' @param dfrow Required; a single data frame row containing columns of xyz coordinates and (optionally) \code{connector_id} with CATMAID coordinates and the connector ID
#' @param skid Required; the skid of the active skeleton
#' @param sid0 The ID of the desired stack mirror on your CATMAID server.  Found by generating a URL within CATMAID and picking out the \code{sid0} param.  Defaults to 5.
#' @param zoom The desired zoom level (\code{s0} parameter) of the URL.  Defaults to 0.
#' @param conn Whether or not to specify an active connector ID in the URL.  Defaults to \code{FALSE}.
#' @param treenode_id A specific treenode ID to set as the active node.  Useful if you are generating URLs for nodes postsynaptic to a particular skeleton.  Will be ignored if \code{conn} is set to \code{TRUE}.
#' @param xyz_columns A character vector specifying the names of the columns containing x, y, and z coordinates.  Defaults to \code{c("x", "y", "z")}.
#' @return A \code{character} string with the CATMAID URL.
#'
#' @export
#'
simple_catmaid_url <- function(dfrow, skid, sid0 = 5, zoom = 0, conn = FALSE, treenode_id = NULL, xyz_columns = c("x", "y", "z")){ #takes row of a data frame with columns for x, y, z, and (optionally) connector_id; skid set for neuron and sid0 for stack mirror
  base = getOption('catmaid.server')

  catmaid_url = paste0(base, "?pid=1")
  catmaid_url = paste0(catmaid_url, "&xp=", dfrow[xyz_columns[1]])
  catmaid_url = paste0(catmaid_url, "&yp=", dfrow[xyz_columns[2]])
  catmaid_url = paste0(catmaid_url, "&zp=", dfrow[xyz_columns[3]])
  catmaid_url = paste0(catmaid_url, "&tool=tracingtool")
  catmaid_url = paste0(catmaid_url, "&active_skeleton_id=", skid)
  if(conn == TRUE){ catmaid_url = paste0(catmaid_url, "&active_node_id=", dfrow["connector_id"]) }
  else if(!is.null(treenode_id)){ catmaid_url = paste0(catmaid_url, "&active_node_id=", treenode_id) }
  catmaid_url = paste0(catmaid_url, "&sid0=", sid0, "&s0=", zoom)

  invisible(catmaid_url)
}


#' Finds the glomerulus associated with a PN
#'
#' Given a vector of skeleton IDs, this will find the glomerului associated with PNs, based on annotations in CATMAID.
#'
#' Requires the PN to be annotated with \bold{glomerulus \emph{X}} or \bold{unknown glomerulus \emph{N}} in CATMAID.  Annotations of the form \bold{glomerulus \emph{X}}
#' will be prioritised over \bold{unknown glomerulus \emph{N}}, and if there are multiples they will be joined together with a forward slash.  If there are
#' no results with either of these annotations, the string \code{"unknown"} will be returned in place of a glomerulus.
#' Note that annotations of the form \bold{glomerulus \emph{X} right|left} are not considered.
#'
#' @param skids Required; an \code{integer} or \code{character} vector of skeleton IDs
#' @param method Optional; indicating whether to use the neuron 'name' or glomerulus 'annotation's.
#' @param exclude Optional; a vector of glomeruli to exclude (in the format \bold{\emph{X}} for \bold{glomerulus \emph{X}}, or the full annotation for \bold{unknown glomerulus \emph{N}})
#' @param ... Additional arguments passed to catmaid_* functions. Use this to specify \code{conn}, \code{pid} or similar low level arguments for the catmaid_query.

#' @return A \code{character} vector of glomeruli names
#'
#' @export
#'
#' @examples
#' \dontrun{
#'conn = catmaid::catmaid_login(server=Sys.getenv("catmaid_server"),
#'       authname=Sys.getenv("catmaid_authname"),authpassword=Sys.getenv("catmaid_authpassword"),
#'       token=Sys.getenv("catmaid_token"))
#' find_glomeruli('WTPN2017_uPN_right')
#' find_glomeruli('WTPN2017_uPN_right', method = 'annotation')
#' }
#'
find_glomeruli <- function(skids, exclude = NULL, method=c("annotation", "name"), ...){
  skids=catmaid::catmaid_skids(skids, ...)
  method=match.arg(method)

  if(method=="name") {
    glomeruli=stringr::str_match(catmaid::catmaid_get_neuronnames(skids, ...),'glomerulus [lv]{0,2}(\\S+)')[,2]
    return(glomeruli)
  }

  annotations = catmaid::catmaid_get_annotations_for_skeletons(unique(skids), ...)
  annotations.glom = annotations[grepl("^glomerulus [A-Za-z0-9]+$", annotations$annotation),]
  annotations.glom$glom = sapply(annotations.glom$annotation, function(a){ sub("glomerulus ", "", a) })
  annotations.unknown_glom = annotations[grepl("^unknown glomerulus \\d+$", annotations$annotation),]

  annotations.glom = annotations.glom[!annotations.glom$glom %in% exclude,]
  annotations.unknown_glom = annotations.unknown_glom[!annotations.unknown_glom$annotation %in% exclude,]

  glomeruli = sapply(skids, function(s){
    s.glom = paste(sort(unlist(annotations.glom[annotations.glom$skid == s, "glom"])), sep = "", collapse = "/")
    s.unknown_glom = paste(sort(unlist(annotations.unknown_glom[annotations.unknown_glom$skid == s, "annotation"])), sep = "", collapse = "/")
    if(nchar(s.glom) > 0){
      s.glom
    }
    else if(nchar(s.unknown_glom) > 0){
      s.unknown_glom
    }
    else{
      "unknown"
    }
  })

  return(glomeruli)
}






#' Check a neuron for potential duplicate synapses
#'
#' Given a neuron or skeleton ID, this function will look for outgoing connectors that are close enough to represent the same synapse.
#' The function will return a data frame (optionally written to a CSV) with IDs of the identified connector pairs, their distances, and URLs to the connectors in CATMAID.
#'
#' Requires the \code{catmaid.server} option to be set in .Rprofile
#'
#' @param neuron A CATMAID \code{neuron} object; required if \code{skid} is not provided.
#' @param skid The skeleton ID of a neuron in CATMAID; required if \code{neuron} is not provided.  Will only be used if \code{neuron} is not provided.
#' @param xy_threshold The maximum X-Y distance (in the same units as your neuron) between two connectors for them to be considered potential duplicates.  Defaults to 200.
#' @param z_threshold The maximum Z distance (in the same units as your neuron) between two connectors for them to be considered potential duplicates.  Defaults to 200.
#' @param fileout Optional; the path to a CSV file where the result should be written.
#'
#' @return A data frame containing the connector ID of each connector in a pair, a CATMAID URL for each, the X-Y and Z distances between the connectors, and the skeleton ID of the neuron.
#'
#' @export
#'
#' @importFrom catmaid read.neuron.catmaid
#' @importFrom stats dist
check_duplicate_synapses <- function(neuron = NULL, skid = NULL, xy_threshold = 200, z_threshold = 200, fileout = NULL){

  if(is.null(skid) & is.null(neuron)){ stop("A skeleton ID or neuron must be provided.") }
  if(is.numeric(neuron)){ skid = neuron; neuron = NULL }#skid provided as first argument
  if(is.null(neuron)){ neuron = read.neuron.catmaid(skid) }

  conn = neuron$connectors[neuron$connectors$prepost == 0,]
  synapse_distances = dist(conn[, c("x", "y")], method = "euclidean")
  synapse.distances.df = as.data.frame(as.matrix(synapse_distances))

  if(nrow(synapse.distances.df) == 0 & ncol(synapse.distances.df) == 0){ potential_duplicates = data.frame(row = 0,col = 0) }
  else{ potential_duplicates = as.data.frame(which(synapse.distances.df <= xy_threshold, arr.ind = TRUE)) }

  potential_duplicates$diagonal = sapply(seq_len(nrow(potential_duplicates)), function(r){ potential_duplicates$row[r] == potential_duplicates$col[r] })
  potential_duplicates = potential_duplicates[potential_duplicates$diagonal == FALSE, c("row", "col")]

  potential_duplicates$within_z_threshold = sapply(seq_len(nrow(potential_duplicates)), function(r){ abs(conn[potential_duplicates$row[r], "z"] - conn[potential_duplicates$col[r], "z"]) <= z_threshold })
  potential_duplicates = potential_duplicates[potential_duplicates$within_z_threshold == TRUE, c("row", "col")]

  potential_duplicates = potential_duplicates[!duplicated(t(apply(potential_duplicates, 1, sort))), ] #deduplicates

  potential_duplicates$URL_1 = sapply(potential_duplicates$row, function(r){simple_catmaid_url(conn[r,], neuron$skid, conn = TRUE)})
  potential_duplicates$URL_2 = sapply(potential_duplicates$col, function(c){simple_catmaid_url(conn[c,], neuron$skid, conn = TRUE)})

  potential_duplicates$conn_1 = sapply(potential_duplicates$row, function(r){conn[r, "connector_id"]})
  potential_duplicates$conn_2 = sapply(potential_duplicates$col, function(c){conn[c, "connector_id"]})

  potential_duplicates$xy_distance = sapply(seq_len(nrow(potential_duplicates)), function(r){ synapse.distances.df[potential_duplicates$row[r], potential_duplicates$col[r]] })
  potential_duplicates$z_distance = sapply(seq_len(nrow(potential_duplicates)), function(r){ abs(conn[potential_duplicates$row[r], "z"] - conn[potential_duplicates$col[r], "z"]) })

  potential_duplicates$skid = rep(neuron$skid, nrow(potential_duplicates))
  potential_duplicates = potential_duplicates[,c("conn_1", "URL_1", "conn_2", "URL_2", "xy_distance", "z_distance", "skid")]

  if(!missing(fileout)){ write.csv(potential_duplicates, file = fileout) }

  return(potential_duplicates)
}





#' Generate URLs for specifically tagged nodes on a neuron
#'
#' Given a neuron and a particular tag, this will return a data frame (optionally written to a CSV) with the node IDs, coordinates, and CATMAID URLs.
#'
#' Requires the \code{catmaid.server} option to be set in .Rprofile
#'
#' @param neuron Required; a A CATMAID \code{neuron} object or skeleton ID.
#' @param tag Required; a string specifying a tag.
#' @param node.id Optional; the ID of a node at which to cut the neuron, if only a portion of the neuron should be considered.
#' @param node.direction If a cut node is provided, which portion of the cut neuron to retain.  Options are \code{downstream} (default) or \code{upstream}.
#' @param volume Optional; a string specifying a FAFB neuropil to filter the tagged nodes.
#' @param fileout Optional; the path to a CSV file where the result should be written.
#'
#' @return A subset of the neuron's treenode data frame (\code{d}) including only tagges nodes, with a CATMAID URL for each.
#'
#' @export
#'
#' @importFrom catmaid read.neuron.catmaid
#' @importFrom nat pointsinside xyzmatrix
#' @importFrom utils write.csv
tagged_nodes <- function(neuron = NULL, tag, node.id = NULL, node.direction = c("downstream", "upstream"), volume = NULL, fileout = NULL){

  if(missing(neuron)){ stop("A skeleton ID or neuron must be provided.") }
  if(is.numeric(neuron)){ neuron = read.neuron.catmaid(neuron) }#skid provided instead of neuron object

  #filter by cut node
  if(!is.null(node.id)){
    node.direction = match.arg(node.direction)
    neuron = split_neuron_local(neuron = neuron, node = node.id, return = node.direction)
  }

  tag.i = neuron$tags[[tag]]
  tag.nodes = neuron$d[neuron$d$PointNo %in% tag.i,]

  #filter by volume
  if(!is.null(volume)){
    tag.nodes = tag.nodes[pointsinside(xyzmatrix(tag.nodes), subset(elmr::FAFBNP.surf, volume)),]
  }

  tag.nodes$URL = sapply(seq_len(nrow(tag.nodes)), function(i){ simple_catmaid_url(dfrow = tag.nodes[i,], skid = neuron$skid, xyz_columns = c("X", "Y", "Z"), treenode_id = tag.nodes[i,"PointNo"]) })

  if(!missing(fileout)){ write.csv(tag.nodes, file = fileout) }
  if(nrow(tag.nodes) == 0){ message("There are no nodes matching your criteria.") }

  return(tag.nodes)
}
