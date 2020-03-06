# ================ [1.0] get the n most common c, u, and e according to the number of interactions ================ 
# get the 20 most frequent c, u, and e items according to one parameter
most_frequent <- function(session, type){
  
  # check session 
  check <- newArgCheck()  # initalize an argument check 
  
  sessions = c("reg", "new", "both")  # vector with all possible valid session inputs
  
  if (session %in% sessions == FALSE){
    addError(msg = "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)", check)
  }
  else if(type %in% c("c", "e", "u") == FALSE){
    addError(msg = "type should be equal to either \"c\", \"u\" or \"e\"", check)
  }
  # stop function if necessary 
  finishArgCheck(check)
  
  ### function starts here
  
  # set items to search
  n.items <- 20
  
  # call "item_analysis" function to get the interactions for all items
  items.df <- items_analysis(session)
  
  # set column names 
  columns <- c("item", "int_total", "type")
  # get length of columns
  len.columns <- length(columns)
  
  # filter items.df by type and sort descending 
  this.df <- items.df[items.df$type == type, ] # select a type
  
  # sort type by interactions total column 
  this.sort <- this.df[order(-this.df$'int_total'), ]
  
  # keep from data frame only the columns "items" and "int_total"
  this.selc <- data.frame(this.sort$item, this.sort$int_total, this.sort$type)[1:20,]
  
  # change columns names 
  names(this.selc)[1:3] <- c("item", "int_total", "type")
  
  # return data frames 
  return(this.selc)
}

# ================ [2.0] get n instances of a specific item  ================ 
get_instances <- function(session, item){
  ### check arguments and either break function or assign data set to variables 
  
  # initalize an argument check 
  check <- newArgCheck()
  # select data and check "session" argument is valid
  if (session == "reg"){
    session.list <- reg.list.concat
  }
  else if (session == "new"){
    session.list <- new.list.concat
  }
  else if (session == "both"){
    session.list <- reg.new.concat
  }
  else{
    addError(msg = "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)", check)
  }
  # check if "item" argument is valid
  if (item %in% items.names == FALSE){
    addError(msg = " \"item\" should be a valid item name in item.names", check)
  }
  # stop function if necessary 
  finishArgCheck(check)
  
  ### function starts here

  #### create data frame to store the data
  columns <- c("participant", "session", "start", "end", "duration","start_hms", "end_hms", "order")
  
  #### set items to search 
  
  # get type of item
  type.index <- items.list$unique  %>% {which(. == item)}
  type.item <- items.list$type[type.index]
  
  # get n percent of items
  frequent.df <- most_frequent(session, type.item)
  
  # get index within most_frequent return data fram of item of interest 
  inst.indx <- frequent.df$item %>% {which(. == item)}
  
  # get number of instance of this item
  total.inst <- frequent.df$int_total[inst.indx]
  # get length of indxs                           #same number 
  len.indxs <- length(indxs)
  
  # initialize data frame
  instance.df <- data.frame()
  for (col in columns){instance.df[[col]] <- as.numeric()}
  instance.df[nrow(instance.df) + total.inst, ] <- NA # add empty NAs

  ### get the indexes of item of interest 
  indxs <- session.list$items  %>% {which(. == item)}
  
  # create data frame with only indxs
  
  # modify names columns 
  
  # add star and end_hms using apply function
  
  # stop if item is not in session.list
  if (len.indxs < 1){stop(sQuote(session), "item was not used in this set of/individual cooking session")
  }
  
  ### add data to data frame 
  for (i in 1:total.inst){
    
    browser()
    
    # get a random number in the range of len.indxs
    random.index <- runif(1, 1, len.indxs)
    
    # get random item instance
    item.inst <- session.list[indxs[random.index],]
    
    # add participant
    instance.df$participant[i] <- item.inst$participant
    
    # add session
    instance.df$session[i] <- item.inst$session
    
    # add time start 
    start <- item.inst$start
    instance.df$start[i] <- start
    
    # add time end 
    end <- item.inst$end
    instance.df$end[i] <- end
    
    # add time duration
    instance.df$duration[i] <- item.inst$duration
    
    # add transformed start
    start.hms <- seconds_to_period(start) 
    instance.df$start_hms[i] <- as.character(start.hms)
    
    # add transformed end
    end.hms <- seconds_to_period(end) # transform time.start and end from seconds to h s m 
    instance.df$end_hms[i] <- as.character(end.hms)
    
    # add instance number
    instance.df$order[i] <- item.inst$order
  }
  
  # save data frame as .csv
  fileName = paste("/", item, "_", session, ".csv", sep = '')
  fileOutput = paste(path_output, fileName, sep = '')
  write.csv(instance.df, fileOutput)
  
  # return data frame
  return(instance.df)
}
# ================ [2.1] get n instances of a specific item  ================ 
get_instances <- function(session, item){
  ### check arguments and either break function or assign data set to variables 
  
  # initalize an argument check 
  check <- newArgCheck()
  # select data and check "session" argument is valid
  if (session == "reg"){
    session.list <- reg.list.concat
  }
  else if (session == "new"){
    session.list <- new.list.concat
  }
  else if (session == "both"){
    session.list <- reg.new.concat
  }
  else{
    addError(msg = "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)", check)
  }
  # check if "item" argument is valid
  if (item %in% items.names == FALSE){
    addError(msg = " \"item\" should be a valid item name in item.names", check)
  }
  # stop function if necessary 
  finishArgCheck(check)
  
  ### function starts here
  
  #### create data frame to store the data
  columns <- c("participant", "session", "start", "end", "duration","start_hms", "end_hms", "order")
  
  #### set items to search 
  
  # get type of item
  type.index <- items.list$unique  %>% {which(. == item)}
  type.item <- items.list$type[type.index]
  
  # get n percent of items
  frequent.df <- most_frequent(session, type.item)

  # get the index of item of interest in most_frequent df
  inst.indx <- frequent.df$item %>% {which(. == item)}
  
  # get number of instance of this item
  total.inst <- frequent.df$int_total[inst.indx]
  
  ### get the indexes of item of interest 
  indxs <- session.list$items  %>% {which(. == item)}
  
  # get length of indxs                           #same number 
  len.indxs <- length(indxs)
  
  # initialize data frame
  instance.df <- data.frame()
  for (col in columns){instance.df[[col]] <- as.numeric()}
  instance.df[nrow(instance.df) + total.inst, ] <- NA # add empty NAs
  
  # create data frame with only indxs
  item.list <- session.list[indxs, c(10:11, 1, 5:6, 7)] # participant # session # order # start # end # duration
  
  # add star and end_hms using apply function
  item.list$start_hms =  as.character(seconds_to_period(item.list$start))
  item.list$end_hms =  as.character(seconds_to_period(item.list$end))
  
  browser()
  
  
  # stop if item is not in session.list
  if (len.indxs < 1){stop(sQuote(session), "item was not used in this set of/individual cooking session")
  }
  
  ### add data to data frame 
  for (i in 1:total.inst){
    
    browser()
    
    # get a random number in the range of len.indxs
    random.index <- runif(1, 1, len.indxs)
    
    # get random item instance
    item.inst <- session.list[indxs[random.index],]
    
    # add participant
    instance.df$participant[i] <- item.inst$participant
    
    # add session
    instance.df$session[i] <- item.inst$session
    
    # add time start 
    start <- item.inst$start
    instance.df$start[i] <- start
    
    # add time end 
    end <- item.inst$end
    instance.df$end[i] <- end
    
    # add time duration
    instance.df$duration[i] <- item.inst$duration
    
    # add transformed start
    start.hms <- seconds_to_period(start) 
    instance.df$start_hms[i] <- as.character(start.hms)
    
    # add transformed end
    end.hms <- seconds_to_period(end) # transform time.start and end from seconds to h s m 
    instance.df$end_hms[i] <- as.character(end.hms)
    
    # add instance number
    instance.df$order[i] <- item.inst$order
  }
  
  # save data frame as .csv
  fileName = paste("/", item, "_", session, ".csv", sep = '')
  fileOutput = paste(path_output, fileName, sep = '')
  write.csv(instance.df, fileOutput)
  
  # return data frame
  return(instance.df)
}