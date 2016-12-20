checkMine <- function(dateSince, user = 'all'){#date format as YYYMMDD string, user as login name
  
  if (user != 'all'){
    ul = catmaid_get_user_list()
    uid = ul[ul$login == user,]$id #there's probably a much better way of doing this...
    userparam = paste("&created_by=", uid, sep = '')
  }
  else{
    userparam = ''
  }
  
  mySkeletons = as.integer(catmaid_fetch(paste("/1/skeletons/?nodecount_gt=1&from=", dateSince, userparam, sep = '')))
  
  
}

duplicateSomas <- function(){
  
}