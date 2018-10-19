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
  vols.df = catmaidVolsAsDF()
  volumes.ids = vols.df[match(volumes, vols.df$name), 'id']#preserves order, c.f. vols.df$name %in% volumes

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
