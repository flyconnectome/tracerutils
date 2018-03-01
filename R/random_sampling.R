#' Randomly sample synapses or connections of a neuron
#'
#'
#'
#'
#'
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
    neuro = subset(neuro, pointsinside(xyzmatrix(neuro$d), subset(FAFBNP.surf, volume)))
    neuro$connectors = neuron$connectors[neuron$connectors$treenode_id %in% neuro$d$PointNo,]
  }

  conn = neuro$connectors#check pruning
  if(plot){
    nat::nopen3d()
    plot3d(neuron, col = "gray", soma = T)
    plot3d(neuro, col = "cyan", lwd = 3, WithNodes = F, WithConnectors = F)
    points3d(nat::xyzmatrix(conn), col = "red")
  }


  #catmaid API is actually returning treenode_id on query skeleton rather than partner; use of catmaid_get_connectors_between is a workaround
  switch(type,
         downstream = {conn = catmaid::catmaid_get_connectors_between(pre_skids = neuron$skid)}
         ,upstream = {conn = catmaid::catmaid_get_connectors_between(post_skids = neuron$skid)}
         ,connector = {conn = conn[conn$prepost == 0,]}
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
           downstream = {simple_catmaid_url(conn[s,], skid = conn$post_skid[s], conn = FALSE, treenode_id = conn$post_node_id[s], xyz_columns = c("post_node_x", "post_node_y", "post_node_z"))}
           ,upstream = {simple_catmaid_url(conn[s,], skid = conn$pre_skid[s], conn = FALSE, treenode_id = conn$pre_node_id[s], xyz_columns = c("pre_node_x", "pre_node_y", "pre_node_z"))}
           ,connector = {simple_catmaid_url(conn[s,], skid = neuron$skid, conn = TRUE)}
    )
  })

  if(!is.null(fileout)){
    write.csv(sample, file = fileout)
  }
  else{
    return(sample)
  }
}
