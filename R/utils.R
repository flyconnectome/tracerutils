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
#' @param skid Required; the skeleton ID of the neuron to split (passed on to
#'   \code{\link{catmaid_skids}})
#' @param node Required; the ID of the node where the neuron should be split (or
#'   a string naming a tag)
#' @param return FIXME: currently ignored
#' @return A \code{neuron} object representing the 'downstream' portion of the
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
split_neuron_local <- function(skid, node, return = "child"){#split local copy of a neuron at a particular node, and return result ('child' by default) as a neuron object
  skid=catmaid_skids(skid, several.ok = FALSE)
  neuron = read.neuron.catmaid(skid)
  if(is.character('node')) {
    tag=node
    node=neuron[['tags']][[tag]]
    if(!isTRUE(length(node)==1L)) {
      stop("Unable to find unique node with the tag: ", tag)
    }
  }
  neuron.distal = distal_to(neuron, node.pointno=node)
  new=prune_vertices(neuron, neuron.distal, invert = T)

  # give it a nice name and handle connectors
  new$NeuronName = paste0("SKID ", skid, " downstream of node ", node)
  old_connectors = catmaid::connectors(neuron)
  new$connectors = old_connectors[old_connectors$treenode_id %in%
                                    neuron$d[neuron.distal, "PointNo"], ]
  new
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
#' @return A \code{character} vector of glomeruli names
#'
#' @export
#'
find_glomeruli <- function(skids){
  annotations = catmaid::catmaid_get_annotations_for_skeletons(unique(skids))
  annotations.glom = annotations[grepl("^glomerulus [A-Za-z0-9]+$", annotations$annotation),]
  annotations.glom$glom = sapply(annotations.glom$annotation, function(a){ sub("glomerulus ", "", a) })
  annotations.unknown_glom = annotations[grepl("^unknown glomerulus \\d+$", annotations$annotation),]
  glomeruli = sapply(skids, function(s){
    s.glom = paste(annotations.glom[annotations.glom$skid == s, "glom"], sep = "", collapse = "/")
    s.unknown_glom = paste(annotations.unknown_glom[annotations.unknown_glom$skid == s, "annotation"], sep = "", collapse = "/")
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
