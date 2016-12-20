checkMine <- function(dateSince, user){#date format as YYYMMDD string, user as login name
  mySkeletons = getSince(dateSince, user = user)
  
  #check for duplicate soma tags
  
  #check for large radius without a soma tag
  
  #check skeleton analytics
  
  
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