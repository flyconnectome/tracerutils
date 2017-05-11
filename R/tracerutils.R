#-----SETUP-----
packages <- function(){
  if (!require("devtools")) install.packages("devtools")
  if(!require("elmr")) devtools::install_github("jefferis/elmr", dependencies=TRUE)
  if(!require("catmaid")) devtools::install_github("jefferis/rcatmaid")
}
catmaidURLs <- function(){
  URLs = character(0)
  names(URLs) = character(0)
  
  URLs["volumes"] = "/1/volumes/"
  URLs["skeletons"] = "/1/skeletons/"
  URLs["analytics"] = "/1/analytics/skeletons"
  
  return(URLs)
}
getEndpoint <- function(name){
  return(catmaidURLs()[name])
  
}

#-----UTILITIES-----
quickNBLAST <- function(skid){
  packages()
  if(!require("doMC")) install.packages("doMC")
  registerDoMC(4)
  dps = read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds", localdir = getOption('flycircuit.datadir'))
  
  results = nblast_fafb(skid)
}

#TODO - connectors option?
catmaidPlot <- function(skid, volumes = NULL, ncol = NULL, vcol = NULL, valpha = NULL){#single skid as numeric, multiples in character vector
  packages()
  neurons = read.neurons.catmaid(skid)
  plot3d(neurons, WithConnectors = F, soma = 2000, col = ncol)
  if (!is.null(volumes)){
    plotVolumes(volumes, vcol, valpha)
  }
  
  invisible(neurons)#returned in case you want to do anything else with them, but not printed to console
}



#-----INTERNAL METHODS-----
#TODO - add colour/alpha specification
plotVolumes <- function(volumes, vcol, valpha){#plot multiple neuropil volumes at once in same space as CATMAID neurons - get volumes from catmaid sever?
  vols.df = catmaidVolsAsDF()
  volumes.ids = vols.df[match(volumes, vols.df$name), 'id']#preserves order, c.f. vols.df$name %in% volumes
  
  #fil out colour and alpha specifications
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
    urlstr = paste("/1/volumes/", as.character(volumes.ids[id]), sep="")
    details = catmaid_fetch(urlstr)
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
    
    
    triangles3d(x = points.df[,'x'], y = points.df[, 'y'], z = points.df[, 'z'], col = vcol[id], alpha = valpha[id])
    
  }
  

}

catmaidVolsAsDF <- function(){
  
  vols = catmaid_fetch(getEndpoint("volumes"))
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
