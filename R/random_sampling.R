#' Randomly sample synapses or connections of a neuron
#'
#' Assemble a data frame/csv for random sampling of a neuron's synapses or connections.
#' The units sampled can be outgoing connections ("downstream), incoming connections ("upstream"), or outgoing synapses (connectors in CATMAID).
#' Optionally, the sample can be limited to a certain number of connections or synapses, or constrained to the section of a neuron either within a
#' neuropil or up-/downstream of a particular node.
#'
#' @param neuron Required; a CATMAID neuron object.
#' @param number Optional; the number of units (connections or synapses) to return.  Will return all if not specified.
#' @param type The type of unit to sample. Options are \code{"downstream"} (samples outgoing connections), \code{"upstream"} (samples incoming connections), and \code{"connector"} (samples outgoing synapses).
#' Defaults to \code{"downstream"}.
#' @param fileout Optional; the path to a CSV file where the result should be written.
#' @param node.id Optional; the id of a treenode where the neuron should be cut, if limiting sample to up-/downstream portion of the neuron.
#' @param node.direction If \code{node.id} is specfied, the section of cut neuron to retain.  Options are \code{"downstream"} (i.e. distal to the cut point) or \code{"upstream"} (proximal to the cut point).
#' Defaults to \code{"downstream"}.
#' @param volume Optional; the name of a neuropil (in the FAFB brain, see \code{\link{FAFBNP.surf}}) that the sample should be constrained to.
#' @param plot Optional; whether or not to generate a 3D plot of the neuron highlighting the portion retained and which connectors are included in the sample.
#' Defaults to \code{FALSE}.
#'
#' @return A \code{data frame} containing the randomised connections or synapses, with a column of URLs to the items in CATMAID.
#' Additional columns in the data frame will vary by \code{type}, but will include further information about the units sampled (e.g. CATMAID IDs, coordinates, etc.).
#' If \code{fileout} is specified, this data frame will also be written to a CSV.
#'
#' @export
#'
#' @importFrom nat pointsinside nopen3d xyzmatrix
#' @importFrom catmaid catmaid_get_connectors_between
#' @importFrom rgl points3d
#' @importFrom utils write.csv
sample_connections <- function(neuron, number = NULL, type = c("downstream", "upstream", "connector"), fileout = NULL, node.id = NULL, node.direction = c("downstream", "upstream"), volume = NULL, plot = FALSE){
  type = match.arg(type)

  if(!is.null(node.id)){
    node.direction = match.arg(node.direction)
    neuro = subset(neuron, distal_to(neuron, node.pointno = node.id), invert = (node.direction == "upstream"))
    neuro$connectors = neuron$connectors[neuron$connectors$treenode_id %in% neuro$d$PointNo,]
  }
  else{
    neuro = neuron
  }

  if(!is.null(volume)){
    neuro = subset(neuro, nat::pointsinside(xyzmatrix(neuro$d), subset(elmr::FAFBNP.surf, volume)))
    neuro$connectors = neuron$connectors[neuron$connectors$treenode_id %in% neuro$d$PointNo,]
  }

  #conn = neuro$connectors#check pruning
  if(plot){
    nat::nopen3d()
    plot3d(neuron, col = "gray", soma = T)
    plot3d(neuro, col = "cyan", lwd = 3, WithNodes = F, WithConnectors = F)
    #rgl::points3d(xyzmatrix(conn), col = "red")
  }


  conn = NULL
  switch(type,
         downstream = {
            conn = catmaid::catmaid_get_connectors_between(pre_skids = neuron$skid)
            conn = conn[conn$connector_id %in% neuro$connectors$connector_id,]
            rgl::points3d(conn[, c("connector_x", "connector_y", "connector_z")], col = "red")
           }
         ,upstream = {
            conn = catmaid::catmaid_get_connectors_between(post_skids = neuron$skid)
            conn = conn[conn$connector_id %in% neuro$connectors$connector_id,]
            rgl::points3d(conn[, c("connector_x", "connector_y", "connector_z")], col = "red")
           }
         ,connector = {
            conn = neuro$connectors[neuro$connectors$prepost == 0,]
            rgl::points3d(xyzmatrix(conn), col = "red")
           }
         )
  if (!is.null(number)){
    n = number
  }
  else{
    n = nrow(conn)
  }




  sample = conn[sample.int(nrow(conn), n),]


  sample$URL = sapply(1:nrow(sample), function(s){
    switch(type,
           downstream = {simple_catmaid_url(sample[s,], skid = sample$post_skid[s], conn = FALSE, treenode_id = sample$post_node_id[s], xyz_columns = c("post_node_x", "post_node_y", "post_node_z"))}
           ,upstream = {simple_catmaid_url(sample[s,], skid = sample$pre_skid[s], conn = FALSE, treenode_id = sample$pre_node_id[s], xyz_columns = c("pre_node_x", "pre_node_y", "pre_node_z"))}
           ,connector = {simple_catmaid_url(sample[s,], skid = neuron$skid, conn = TRUE)}
    )
  })

  if(!is.null(fileout)){
    utils::write.csv(sample, file = fileout)
  }
  else{
    return(sample)
  }
}
