#' Quickly plot neurons from CATMAID
#'
#' Retrieve one or more neurons from CATMAID and plot them, along with one or more CATMAID volumes if desired
#'
#' @param skid Required; the skeleton ID of the neuron in CATMAID. Can accept a vector with multiple SKIDs.
#' @param volumes The names of the volumes in CATMAID to be plotted.  Defaults to NULL.
#' @param ncol The colour(s) to use when plotting the specified neuron(s).
#'     If a vector of colour names is provided, the colours will be applied to the neurons in the order specified,
#'     repeating from the beginning if there are more neurons than colours.
#'     Uses default from NAT package if unspecified.
#' @param vcol The colour(s) to use when plotting the specified volume(s).
#'     Behaves the same way as ncol.  Defaults to gray.
#' @param valpha The alpha value(s) to use when plotting volume(s).
#'     Behaves the same way as ncol and vcol.  Defaults to 0.5.
#' @return Returns the neuron(s) retrieved from CATMAID
#'
#' @export
#' @importFrom rgl plot3d
#' @examples
#' plot_catmaid(skid = 'WTPN2017_uPN_right', volumes = 'v14.neuropil')
#TODO - connectors option?
plot_catmaid <- function(skid, volumes = NULL, ncol = NULL, vcol = NULL, valpha = NULL){#single skid as numeric, multiples in character vector
  #packages()
  neurons = catmaid::read.neurons.catmaid(skid)
  rgl::plot3d(neurons, WithConnectors = F, soma = T, col = ncol)#ERROR - plotting root node as soma
  if (!is.null(volumes)){
    plotVolumes(volumes, vcol, valpha)
  }
  invisible(neurons)#returned in case you want to do anything else with them, but not printed to console
}



#' Open a new 3D plot and plot a brain surface
#'
#' Opens a new 3D viewer window (with pan), plots a brain volume, and adjusts the view angle.
#'
#' @param volume The brain or other surface object to plot.  Defaults to \code{FAFB.surf} from the \code{elmr} package
#' @param col The colour to use for plotting the surface.  Default is gray.
#' @param alpha The alpha value of the plotted surface.  Defaults to 0.1.
#'
#' @export
#' @importFrom nat nopen3d nview3d
new_3d_plot <- function(volume = elmr::FAFB.surf, col = "gray", alpha = 0.1){
  nat::nopen3d()
  if(!is.null(volume)) plot3d(volume, col = col, alpha = alpha)
  nat::nview3d("frontal")
}





#-----INTERNAL METHODS-----
#' @importFrom rgl triangles3d
plotVolumes <- function(volumes, vcol, valpha, pid = 1){#plot multiple neuropil volumes at once in same space as CATMAID neurons - get volumes from catmaid sever?
  df_vols = catmaidVolsAsDF()
  volumes.ids = df_vols[match(volumes, df_vols$name), 'id']#preserves order, c.f. df_vols$name %in% volumes

  #fill out colour and alpha specifications
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
    valpha = rep(0.1, length(volumes))
  }


  for (id in 1:length(volumes.ids)){
    if (is.na(volumes.ids[id])){
      warning(paste("Volume \"", volumes[id], "\" was not found.", sep = ""))
      next
    }
    urlstr = paste0(pid, URL.volumes, as.character(volumes.ids[id]))
    details = catmaid::catmaid_fetch(urlstr)
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


    rgl::triangles3d(x = points.df[,'x'], y = points.df[, 'y'], z = points.df[, 'z'], col = vcol[id], alpha = valpha[id])

  }
}

catmaidVolsAsDF <- function(pid = 1){

  vols = catmaid::catmaid_fetch(paste0(pid, URL.volumes))
  #convert the vols$data (that contains data fields) into a column list, and then transpose to get nObs x nVariables
  vols_df <- data.frame(t(sapply(vols$data,c)))
  #now name the dataframe with the fields from header vols$columns
  names(vols_df) <- vols$columns

  return(vols_df)
}
