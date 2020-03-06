# ================ [1.0] session slices an item per section - summary ================ 
slices_item <- function(item, session){
  
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
    stop(sQuote(session), " Wrong input: session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # set sessions (slices)
  sections <- 3
  
  # set sessions
  sessions <- participants * len.session
  
  # set session factor to account for the number of participants
  participants.factor <- c(0, 20)
  
  ### create data frame to store calculations
  cols.names <-  c("1", "2", "3") # names of columns 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+ sessions,] <- NA #add empty NAs
  
  # iterate over session types (reg and/or new)
  for (l in 1:len.session){
    
    # assign the session type to the session list object
    if (len.session == 1){
      this.session <- session.list
    }
    else if (len.session == 2){
      this.session <- session.list[[l]]
    }
    
    #iterate over participants 
    for (p in 1:participants){
      
      # get total number of times item was used in the current session 
      total.sum <- sum(this.session[[p]]$items == item, na.rm = TRUE)
      
      if (total.sum > 0){
        #find the range duration
        start.time <- min(this.session[[p]]$`start`)
        end.time <- max(this.session[[p]]$`end`, n=1)
        range.time <- end.time - start.time
        
        # set time duration for section slice 
        sections.slices <- range.time/sections
        
        # get time ranges
        start.ranges <- c(start.time + sections.slices*(1:sections-1))
        end.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
        
        # list of start indexes
        start.indexes  <- c()
        
        # find start and end indexes and assign to variabes
        for (s in 1:sections){
          start.indexes [s] <- which.min(abs(start.ranges[s] -  this.session[[p]]$`start`))
        }
        #list of end indexes
        end.indexes  <- c((start.indexes -1)[2:sections], length(this.session[[p]]$`end`))
        
        # get the frequency on each range
        for (section in 1:sections){
          
          #get index per section
          start.index <- start.indexes [section]
          end.index <- (end.indexes[section])
          
          # select data 
          p.data <- this.session[[p]]$items[start.index:end.index]
          
          # get frequency of "item" in this section 
          item.freq <- sum(p.data == item, na.rm = TRUE)

          # add value to data frame 
          freqs.df[p + participants.factor[l], section] <-  item.freq
        }
      }
      # find if the item was used, if not skip the sections loop
      else if (total.sum == 0){
        freqs.df[p + participants.factor[l], 1:sections] <- 0
      }
    }
  }
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)

  # get sum per section and average
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i]], na.rm = TRUE)
  }
  
  # sum sections
  sum.sections <- sum(freqs.sum)
  
  # initalize vector to store sum data
  freqs.avg <- numeric(sections)

  # get sum per section and average
  freqs.avg <- round((freqs.sum / sum.sections)*100, 0)
  
  # position of larger average frequency
  max.sect <- match(max(freqs.avg), freqs.avg)
  max.avg <- c(max.sect, freqs.avg[max.sect])
  
  
  # merge items to return
  freq.item <- mget(c("freqs.df", "freqs.avg", "max.avg")) 
  return(freq.item)
}

# ================ [2.0] session slices an item per section - summary ================ 
slices_summary <- function(session){
  
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
  
  # create data frame
  columns <- c("item", "segment", "percentage")
  
  # get length of columns
  len.columns <- length(columns)
  
  # data frame is initialized
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df) + len.items.names,] <- NA # add empty NAs
  stats.df[, 1] <- items.names # add items namesit
  
  # iterate over list items
  # add values to data frame 
  for (i in 1:len.items.names){
    
    # get current item
    this.item <- items.list$unique[i]
    
    # call the slices_item function
    item.slices <- slices_item(this.item, session)
    
    # get max section and average value
    item.data <- item.slices$max.avg
    
    # add data to data frame
    stats.df[i, 2:3] <- item.data
  }
  
  # return data frame
  return(stats.df)
}