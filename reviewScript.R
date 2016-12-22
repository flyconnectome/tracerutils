checkMine <- function(dateSince, user){#date format as YYYMMDD string, user as login name
  mySkeletons = getSince(dateSince, user = user)
  neurons = read.neurons.catmaid(mySkeletons)
  
  for (i in 1:length(neurons)){
    print(paste("Neuron ", names(neurons)[i], sep  = ""))
    
    neuron = neurons[i]
    issuesfound = FALSE
    #check for duplicate soma tags and radius set on a node without soma tag
    issuesfound = checkDuplicateSomas(neuron) & checkRadiusWithoutSoma(neuron)
    
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
      #nothing for now - move outside loop and assemble body string to minimise server calls
    }
    
    analytics = catmaid_fetch("/1/skeleton/analytics", body = list("skeleton_ids[0]"=names(neurons)[i]))
    issues = analytics$issues[[1]][[2]]#modify to handle for each neuron
    
    issue_code = numeric(length(issues))
    node = numeric(length(issues))
    for (n in 1:length(issues)){#check that this order is consistent
      issue_code[n] = issues[[n]][[1]]
      node[n] = issues[[n]][[2]]
    }
    issues.df = data.frame(issue_code, node)
    
    problems = issues.df[issues.df$issue_code %in% c(0, 1, 3, 4, 7),]
    autapse = issues.df[issues.df$issue_code == 0,]
    double_post = issues.df[issues.df$issue_code == 1,]
    no_pre = issues.df[issues.df$issue_code == 3,]
    dup_syn = issues.df[issues.df$issue_code == 4,]
    end_in_non_end = issues.df[issues.df$issue_code == 7,]
    
    if (nrow(problems) > 0){
      #print(problems)
      if(nrow(autapse) > 0) print(paste("Autapse at node(s) ", paste(autapse[,'node'], collapse = ", "), sep = ""))
      if(nrow(double_post) > 0) print(paste("Two or more nodes postsynaptic to the same connector at node(s) ", paste(double_post[,'node'], collapse = ", "), sep = ""))
      if(nrow(no_pre) > 0) print(paste("Connector without presynaptic skeleton at node(s) ", paste(no_pre[,'node'], collapse = ", "), sep = ""))
      if(nrow(dup_syn) > 0) print(paste("Possible duplicate synapse at node(s) ", paste(dup_syn[,'node'], collapse = ", "), sep = ""))
      if(nrow(end_in_non_end) > 0) print(paste("End tag in non-end node at node(s) ", paste(end_in_non_end[,'node'], collapse = ", "), sep = ""))
      issuesfound = TRUE
    }
    
    if (issuesfound == FALSE) print(" looks good!")
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
  
  skeletons = catmaid_fetch(paste("/1/skeletons/?nodecount_gt=1&from=", dateSince, userparam, sep = ''))
  
  
}


#-----INTERNALS-----
checkDuplicateSomas <- function(neuron){
  issuesfound = FALSE
  if ((!is.null(neuron$tags$soma)) & length(neuron$tags$soma) > 1){
    print(paste("Duplicate somas in neuron ", names(neurons)[i], " at nodes ", paste(neuron$tags$soma, collapse = ", "), sep = ""))#or, you know, actually do something useful like generate a URL
    issuesfound = TRUE
  }
  return(issuesfound)
}

checkRadiusWithoutSoma <- function(neuron){
  issuesfound = FALSE
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
      print(paste("Point(s) ", notsoma, " on neuron ", names(neurons)[i], " have a radius but no soma tag", sep = ""))
      issuesfound = TRUE
    } 
  }
  return(issuesfound)
}
