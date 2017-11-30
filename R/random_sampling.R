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

  conn = catmaid::connectors(neuro)

  switch(type,
         downstream = {conn = conn[conn$prepost == 0,]}#NEED T) GET POSTSYNAPTIC IDS
         ,upstream = {conn = conn[conn$prepost == 1,]}
         ,connector = {conn = conn}
         )
  if (!is.null(number)){
    n = number
  }
  else{
    n = nrow(conn)
  }

  sample = conn[sample.int(nrow(conn), n),]

  sample$URL = sapply(1:nrow(sample), function(s){ simple_catmaid_url(conn[s,], skid = neuron$skid, conn = TRUE) })#ONLY NEURON SKID IF SAMPLING CONNECTORS

  if(!is.null(fileout)){
    write.csv(sample, file = fileout)
  }
  else{
    return(sample)
  }
}
