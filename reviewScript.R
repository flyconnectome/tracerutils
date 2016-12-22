checkMine <- function(dateSince, user){#date format as YYYMMDD string, user as login name
  mySkeletons = getSince(dateSince, user = user)
  
  for (i in 1:length(mySkeletons)){
    neuron = read.neuron.catmaid(mySkeletons[i])#TODO- move this call outside loop - retrieve all skeletons simultaneously
    issuesfound = FALSE
    #check for duplicate soma tags
    if ((!is.null(neuron$tags$soma)) & length(neuron$tags$soma) > 1){
      print(paste("Duplicate somas in neuron ", mySkeletons[i], " at nodes ", paste(neuron$tags$soma, collapse = ", "), sep = ""))#or, you know, actually do something useful like generate a URL
      issuesfound = TRUE
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
      if (length(notsoma) > 0){
        print(paste("Point(s) ", notsoma, " on neuron ", mySkeletons[i], " have a radius but no soma tag", sep = ""))
        issuesfound = TRUE
      } 
    }
    
    #check skeleton analytics
    #analytics checks for eight possible issues, but we only care about a few of them for now
    #0 - autapse
    #1 - Two or more times postsynaptic to the same connector
    #2 - Connector without postsynaptic targets
    #3 - Connector without presynaptic skeleton
    #4 - Duplicated synapse?
    #5 - End node without end tag
    #6 - TODO tag
    #7 - End-node tag in a non-end node
    #for now, we'll check for 0, 1, 3, 4, and 7
    
    #body = list(0)
    for (skid in 1:length(mySkeletons)){
      #nothing for now  add loop to assemble body string once multiple neurons are handled at once
    }
    
    analytics = catmaid_fetch("/1/skeleton/analytics", body = list("skeleton_ids[0]"=mySkeletons[i]))
    issues = analytics$issues[[1]][[2]]#modify to handle for each neuron
    issue_code = numeric(length(issues))
    node = numeric(length(issues))
    for (n in 1:length(issues)){#check that this order is consistent
      issue_code[n] = issues[[n]][[1]]
      node[n] = issues[[n]][[2]]
    }
    issues.df = data.frame(issue_code, node)
    
    problems = issues.df[issues.df$issue_code %in% c(0, 1, 3, 4, 7),]
    
    if (nrow(problems) > 0){
      print(problems)
      issuesfound = TRUE
    }
    
    if (issuesfound == FALSE) print(paste("Neuron ", mySkeletons[i], " looks good!", sep = ""))
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


#TODO - checking for duplicate somas across all skeletons
checkDuplicateSomas <- function(){
  
}