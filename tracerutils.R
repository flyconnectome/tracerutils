libraries <- function(){
  library(elmr); library(catmaid)
}

quickNBLAST <- function(skid){
  libraries(); library(doMC)
  registerDoMC(4)
  dps = read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds", localdir = getOption('flycircuit.datadir'))
  
  results = nblast_fafb(skid)
}

catmaidPlot <- function(skid, volumes){
  neuron = read.neurons.catmaid(skid)[1] #read.neuron.catmaid(skid) returns same, but with branches/endpoints highlighted by default
  plot3d(neuron)
  if (!is.null(volumes)){
    #plotVolumes(volumes)
  }
}

#TODO
plotVolumes <- function(volumes){#plot multiple neuropil volumes at once in same space as CATMAID neurons - get volumes from catmaid sever?
  vols.df = catmaidVolsAsDF()
  volumes.ids = vols.df[vols.df$name %in% volumes, 'id']
  
}

catmaidVolsAsDF <- function(){
  
  vols = catmaid_fetch("/1/volumes")
  l = length(vols)
  
  comment = character(l)
  name = character(l)
  creation_time = character(l)
  edition_time = character(l)
  project = integer(l)
  user = integer(l)
  id = integer(l)
  editor = integer(l)
  
  for (i in 1:l){
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
