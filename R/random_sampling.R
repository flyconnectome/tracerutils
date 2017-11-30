sample_connections(neuron, number = NULL, type = c("downstream", "upstream", "connector"), fileout = NULL, node.id = NULL, node.direction = c("downstream", "upstream"), volume = NULL){
  type = match.arg(type)

  if(!is.null(node.id)){
    node.direction = match.arg(node.direction)
    neuro = subset(neuron, distal_to(neuron, node.idx = node.id), invert = (node.direction == "upstream"))
    catmaid::copy_tags_connectors(old = neuron, new = neuro)
  }
  else{
    neuro = neuron
  }

  if(!is.null(volume)){
    neuro = subset(neuro, pointsinside(xyzmatrix(neuro$d), subset(FAFBNP.surf, volume)))
    catmaid::copy_tags_connectors(old = neuron, new = neuro)
  }

  conn = catmaid::connectors(neuro)#check pruning

  #clean up this section
  switch(type,
         downstream = {conn = catmaid::catmaid_get_connector_table(neuron$skid, direction = "outgoing")}
         ,upstream = {conn = catmaid::catmaid_get_connector_table(neuron$skid, direction = "incoming")}
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
           downstream = {simple_catmaid_url(conn[s,], skid = conn[s, "partner_skid"], conn = FALSE)}
           ,upstream = {simple_catmaid_url(conn[s,], skid = conn[s, "partner_skid"], conn = FALSE)}
           ,connector = { simple_catmaid_url(conn[s,], skid = neuron$skid, conn = TRUE) }
    )
  })

  if(!is.null(fileout)){
    write.csv(sample, file = fileout)
  }
  else{
    return(sample)
  }
}
