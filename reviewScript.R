checkMine <- function(dateSince, user){#date format as YYYMMDD string, user as login name
  mySkeletons = getSince(dateSince, user = user)
  
  for (i in 1:length(mySkeletons)){
    neuron = read.neuron.catmaid(mySkeletons[i])

    #check for duplicate soma tags
    if (!is.null(neuron$tags$soma) && length(neuron$tags$soma > 1)){
      print(paste("Duplicate soma in neuron ", mySkeletons[i], "!", sep = ""))#or, you know, actually do something useful like generate a URL
    }
    
    #check for radius without a soma tag
    radpoints = neuron$d[neuron$d$W > -2, 'PointNo']
    if (length(radpoints) > 0){
      notsoma = numeric(0)
      if (!is.null(neuron$tags$soma)){
        for (i in 1:length(radpoints)){
          if (radpoints[i] %in% neuron$tags$soma) next
          notsoma = c(notsoma, radpoints[i])
        }
      }
      else{
        notsoma = radpoints
      }
      print(paste("Point(s) ", notsoma, " on neuron ", mySkeletons[i], " have a radius but no soma tag", sep = ""))
    }
    
    #check skeleton analytics
    
  }
  
  
  
}

getSince <- function(dateSince, user = 'all'){#date format as YYYMMDD string, user as login name
  #TODO: add to date param, default to today as format(Sys.Date(), '%Y%m%d')
  
  if (user != 'all'){
    ul = catmaid_get_user_list()
    uid = ul[ul$login == user,]$id #there's probably a much better way of doing this...
    userparam = paste("&created_by=", uid, sep = '')
  }
  else{
    userparam = ''
  }
  
  skeletons = as.integer(catmaid_fetch(paste("/1/skeletons/?nodecount_gt=1&from=", dateSince, userparam, sep = '')))
  
  
}


#TODO
checkDuplicateSomas <- function(){
  
}