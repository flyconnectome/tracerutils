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
  
}
