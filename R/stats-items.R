# get the descriptive statistics of items. The descriptive stats include the following: 
# sessions used, interactions (total, and average), duration (total, and average), 
# distinct, and stage of used. Differentiate by regular, new and both sessions 

# ================ [1.0] counting interactions (1), duration (2) or distinct (unique) items (3) per session  ================ 
items_sessions <- function(session, method){
  # count how many times each times was used for every cooking session
  # returns a data frame object that is arranged like a matrix (e.g items x interactions per participant) 
  
    #   method 1 = interactions
    #   method 2 = duration
    #   method 3 = unique
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    len.session <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session <- 2
  }
  else{
    stop(sQuote(session), "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # set session factor to account for the number of participants
  participants.factor <- c(0, 20)
  
  # get numbber of cooking sessions (take into acount whether  list is simple or concatenated )
  cooking.sessions <- len.session * participants 
  
  # name of data frames columns
  cols.names <-  c("items", 1:cooking.sessions)
  len.cols.names <- length(cols.names)
  
  # create freqs data frame to store calculations 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+len.items.names,] <- NA #add empty NAs
  freqs.df[,1] <- items.names
  
  # iterate over session types (reg and/or new)
  for (s in 1:len.session){
    
    # assign the session type to the session list object
    if (len.session == 1){
      this.session <- session.list
    }
    else if (len.session == 2){
      this.session <- session.list[[s]]
    }
    
    #iterate over participants
    for (p in 1:participants){
      
      # get data for unique items
      if (method == 3){
        
        # select data for each participant (items and items_uniq)
        p.data_temp <- this.session[[p]][2:3] 
        
        # get unique items from selected data
        p.data <- unique(p.data_temp[1:2])
      }
      # get data for interactions and duration 
      else{
        # select data for each participant (items)
        p.data <- this.session[[p]][2] 
      }
      
      # count unique items
      freqs <- cbind(table(p.data$items))
      
      # get length of frequencies
      len.freqs <-  length(dimnames(freqs)[[1]])
      
      #add counting to data frame
      for (f in 1:len.freqs){
        
        #find name in data frame
        item.name <- dimnames(freqs)[[1]][f]
        
        #find indexes of item in participant
        item.indexes <- this.session[[p]] %>% {which(this.session[[p]][2] == item.name)}
        
        #find and column position of current item
        row.pos <- match(item.name, items.names)
        col.pos <- 1 + p + participants.factor[s]
        
        if (method == 2){
          #get duration of item
          item.data <- sum(this.session[[p]]$duration[item.indexes])
        }
        else{
          # get counting of items
          item.data <- freqs[f]
        }
        
        print(item.name)
        
        # add frequency to item or update value in data frame
        freqs.df[row.pos,  col.pos] <- item.data
      }
    }
  }
  return(freqs.df)
}

# ================ [2.0] summary counting interactions (1), duration (2) and distinct (unique) items (3) per session ================
items_analysis <- function(session){
  
  # select session 
  if ((session == "reg") || (session == "new")){
    len.session <- 1
  }
  else if (session == "both"){
    len.session <- 2
  }
  else{
    stop(sQuote(session), " Wrong input: session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  ### call function [1 ] to get data frames 
  interactions.df <- items_sessions(session, 1)
  durations.df <- items_sessions(session, 2)
  uniques.df <- items_sessions(session, 3)
  
  ### create data frame to store data, this data frame will be returned 
  
  # set cooking sessions
  sessions <- participants * len.session
  
  # names of data frame columns of values to compute
  columns <- c("item", "sessions", "int_total", "int_mean", "int_median", "dur_total", "dur_mean", "dur_median", "distinct_uniq", "section", "section_avg", "type")
  # get length of columns
  len.columns <- length(columns)
  
  # data frame is initialized
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df) + len.items.names,] <- NA # add empty NAs
  stats.df[, 1] <- items.names # add items names
  
  # corrected participants
  sessions_c <- sessions +1 
  
  # add values to data frame 
  for (i in 1:len.items.names){
  
    # "sessions" to df
    this.interactions <- na.omit(as.numeric(interactions.df[i, 2:sessions_c]))            # get row of items from data frame
    number.sessions <- length(this.interactions)
    stats.df[i, 2] <- number.sessions
    
    # "int_total" to df
    sum.interactions <- sum(this.interactions) 
    stats.df[i, 3] <- sum.interactions
    
    # "int_mean" to df
    stats.df[i, 4] <- round(sum.interactions / number.sessions, 1)
    
    # "int_median" to df
    stats.df[i, 5] <- round(median(this.interactions), 1)
    
    # "dur_total" to df
    this.durations <-  na.omit(as.numeric(durations.df[i, 2:sessions_c]))
    stats.df[i, 6] <- sum(this.durations)
    
    # "dur_mean" to df
    stats.df[i, 7] <- round(stats.df[i, 6] / sum.interactions, 1)
    
    # "dur_median" to df
    stats.df[i, 8] <- median(this.durations)
    
    # ""distinct_uniq" to df
    this.uniques <-  na.omit(as.numeric(uniques.df[i, 2:sessions_c]))
    stats.df[i, 9] <- round(sum(this.uniques) / number.sessions, 1)
    
    # call slice items function
    this.item <- items.list$unique[i]
    slices <- slices_item(this.item, session)

    # "section" to df
    max.section <-  slices$max.avg
    stats.df[i, 10] <- max.section[1]
    
    # "section.avg" to df
    stats.df[i, 11] <- max.section[2]
    
    # "type" to df
    stats.df[i, 12] <- items.list$type[i]
  }
  
  #return data frame 
  return(stats.df)
}