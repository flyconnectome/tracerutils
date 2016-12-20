libraries <- function(){
  library(elmr); library(catmaid)
}

quickNBLAST <- function(skid){
  libraries(); library(doMC)
  registerDoMC(4)
  dps = read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds", localdir = getOption('flycircuit.datadir'))
  
  results = nblast_fafb(skid)
}

catmaidPlot <- function(skid){
  neuron = nread.neurons.catmaid(skid)[1]
  plot3d(neuron)
}

