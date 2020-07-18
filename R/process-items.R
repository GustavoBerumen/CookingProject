# ================ [1.0] items concurrent around according to time method================ 
items_around <- function(item, session, search){
  # search "bef" = before
  # search "aft" = after
  # search "in" = in the range
  
  # select session
  if (session == "reg"){
    session.list <- reg.list
    sessions <- 1
  } else if (session == "new"){
    session.list <- new.list
    sessions <- 1
  } else if (session == "both"){
    session.lists <- c(reg.list, new.list)
    sessions <- 2
  }
  else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\" or \"both\" (reg and new")
  }
  
  # set range (seconds after and before to search for items)
  range <- 30 
  # range <- 10 # (it was 10  for the analysis in data-analysis.R)
  
  ### function starts here 
  
  # initialize variable to count the frequency of item of interest
  item.count <- 0
  
  # initialize variable to count the instance counter
  instance.counter <- 0
  
  # initialize counter
  counter <- 0 
  
  # initialize variable to count the row position
  row.pos <- 0
  
  # initialize
  counter <- 0 
  
  # create data frame to store calculations time interval
  cols.names <- c("distance", "item_n", "order", "start_time", "end_time", "participant", "session", "instance")
  items.df <- data.frame()
  for (col in cols.names){items.df[[col]] <- as.numeric()}
  # add rows -PROVISIONAL SOLUTION - find a better way
  if(session == "both"){items.df[nrow(items.df) + 3000, ] <- NA}
  items.df[nrow(items.df) + 2000, ] <- NA
  
  # set session when session is equal to both (sessions = 2)
  for (s in 1:sessions){
    
    # iterate over sessions
    if((sessions == 2) & (s == 1)){
      session.list <- reg.list
    }
    else if((sessions == 2) & (s == 2)){
      session.list <- new.list
    }
    # iterate over participants 
    for (p in 1:participants){
      
      # find indexes of item.interest
      item.indxs <- session.list[[p]]$`items` %>% {which(. == item)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      ### check item indexes are not empty and follow TIME interval approach
      if (len.indxs > 0){
        
        # iterate over indexs
        for(index in 1:len.indxs){
          
          # get the position of item of interest
          item.pos <- item.indxs[index]
          
          ### change to after
          
          #before range
          if (search == "bef"){
            
            # get times of the time window of the items that will not be excluded 
            item.end <- session.list[[p]]$`start`[item.pos]
            item.start <- item.end -  range
            
            # get items not in interval before of the concurrent item
            left.items.excl <- which((session.list[[p]]$`start`) <  item.start) # items left tail 
            right.items.excl <- which((session.list[[p]]$`start`) >  item.end) # items right tail 
            
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
            to.keep <- c(item.pos, to.keep)
          }
          
          #after range
          if (search == "aft"){
            # get times of the time window of the items that will not be excluded 
            item.start <- session.list[[p]]$`end`[item.pos]
            item.end <- item.start +  range
            
            # get items of items to exclude 
            left.items.excl <- which((session.list[[p]]$`start`) <  item.start) # items left tail 
            right.items.excl <- which((session.list[[p]]$`start`) >  item.end) # items right tail 
            
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
            to.keep <- c(item.pos, to.keep)
          }
          
          #after range
          if (search == "in"){
            
            # item interest
            item.start <- session.list[[p]]$`start`[item.pos] - range
            item.end <- session.list[[p]]$`end`[item.pos] + range 
            item.mid <- (item.start + item.end) / 2
            item.dur <- item.end - item.start
            
            # get items in interval 
            right.items.excl <- which((session.list[[p]]$`start`) >=  item.end)
            left.items.excl <- which((session.list[[p]]$`end`) <=  item.start)
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
          }
          
          ### remove duplicates from data frame
          items.intv <- session.list[[p]][session.list[[p]]$order %in% to.keep, ]
          
          ### get items distance
          
          # get position of item of interest within interval
          item.int.pos <- which(items.intv$`order` == item.pos)
          
          # length items interval
          len.intv <- length(items.intv$order)
          
          # vector with order positions for each item in items.intv
          order.vc <- c(1:len.intv)
          
          # vector with relative position of items with respect to item of interest
          rel.pos.vec <-  order.vc - item.int.pos
          
          # increase instance counter by 1
          instance.counter <- instance.counter + 1
          
          for (i in 1:len.intv){
            
            # increase counter 
            counter <- counter + 1
            
            #row position
            this.row <- row.pos + i
            
            #add distance
            items.df$distance[this.row] <- rel.pos.vec[i]
            
            #add order
            items.df$order[this.row] <- items.intv$`order`[i]
            
            # item name
            this.item.name <- items.intv$`items`[i]
            
            #add item (number)
            items.df$item_n[this.row] <-  items.list$unique  %>% {which(. == this.item.name)}
            
            #add start time
            items.df$start_time[this.row] <- items.intv$`start`[i]
            
            #add end time
            items.df$end_time[this.row] <- items.intv$`end`[i]
            
            #add participant
            items.df$participant[this.row] <- items.intv$`participant`[i]
            
            #add session
            items.df$session[this.row] <- items.intv$`session`[i]
            
            #add instance
            items.df$instance[this.row] <- instance.counter
            
            #add id
            items.df$id[this.row] <- counter
            
          }
          # update row position
          row.pos <- row.pos + len.intv
        }
      }
    }
    #remove NAs from data frame
    items.intv <- items.df[rowSums(is.na(items.df)) <7,]
    
    #filter data frame [remove item of interest]
    items.intv.filter <- items.intv[items.intv$distance != 0, ]
    
    #non.items.intv <- non.items.df
    itemsAround.ls <- mget(c("items.intv", "items.intv.filter"))
  }
  return(itemsAround.ls)
}


# ================ [1.0] items concurrent around according to number of items method================ 
items_around_n <- function(item, session, search){
  # search "bef" = before
  # search "aft" = after
  # search "in" in this one
  
  # select session
  if (session == "reg"){
    session.list <- reg.list
    sessions <- 1
  } else if (session == "new"){
    session.list <- new.list
    sessions <- 1
  } else if (session == "both"){
    session.lists <- c(reg.list, new.list)
    sessions <- 2
  }
  else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\" or \"both\" (reg and new")
  }
  
  ### function starts here 
  
  # initialize variable to count the frequency of item of interest
  item.count <- 0
  
  # set range (seconds after and before to search for items)
  # range <- 3
  
  # initialize variable to count the instance counter
  instance.counter <- 0
  
  # initialize counter
  counter <- 0 
  
  # initialize variable to count the row position
  row.pos <- 0
  
  # initialize
  counter <- 0 
  
  # create data frame to store calculations time interval
  cols.names <- c("distance", "item_n", "order", "start_time", "end_time", "participant", "session", "instance")
  items.df <- data.frame()
  for (col in cols.names){items.df[[col]] <- as.numeric()}
  # add rows -PROVISIONAL SOLUTION - find a better way
  if(session == "both"){items.df[nrow(items.df) + 12000, ] <- NA}
  items.df[nrow(items.df) + 4000, ] <- NA
  
  # set session when session is equal to both (sessions = 2)
  for (s in 1:sessions){
    
    # iterate over sessions
    if((sessions == 2) & (s == 1)){
      session.list <- reg.list
    }
    else if((sessions == 2) & (s == 2)){
      session.list <- new.list
    }
    # iterate over participants 
    for (p in 1:participants){
      
      # find indexes of item.interest
      item.indxs <- session.list[[p]]$`items` %>% {which(. == item)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # get length of this list
      len.list <- length(session.list[[p]]$`items`)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      ### check item indexes are not empty and follow TIME interval approach
      if (len.indxs > 0){
        
        # iterate over indexs
        for(index in 1:len.indxs){
          
          # get the position of item of interest
          item.pos <- item.indxs[index]
          
          ### change to after
          
          #before range
          if (search == "bef"){
            
            # get items n indexes before the item of interest
            incl.items <- c(item.pos-1, item.pos-2, item.pos-3)
            
            # remove indexes not in indexes list
            incl.clean <- incl.items[incl.items > 0]
            
            # merge item.pos and items befor 
            to.keep <- c(incl.clean, item.pos)
            to.keep <- sort(to.keep)
          }
          
          #after range
          if (search == "aft"){
            
            # get items n indexes before the item of interest
            incl.items <- c(item.pos+1, item.pos+2, item.pos+3)
            
            # remove indexes not in indexes list
            incl.clean <- incl.items[incl.items < len.list]
            
            # merge item.pos and items befor 
            to.keep <- c(incl.clean, item.pos)
            to.keep <- sort(to.keep)
          }
          
          #after range
          if (search == "in"){
            
            # item interest
            item.start <- session.list[[p]]$`start`[item.pos]
            item.end <- session.list[[p]]$`end`[item.pos]
            
            # get items in interval to exclude
            right.items.excl <- which((session.list[[p]]$`start`) >  item.end)
            left.items.excl <- which((session.list[[p]]$`start`) <  item.start)
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
          }
          
          ### remove duplicates from data frame
          items.intv <- session.list[[p]][session.list[[p]]$order %in% to.keep, ]
          
          ### get items distance
          
          # get position of item of interest within interval
          item.int.pos <- which(items.intv$`order` == item.pos)
          
          # length items interval
          len.intv <- length(items.intv$order)
          
          # vector with order positions for each item in items.intv
          order.vc <- c(1:len.intv)
          
          # vector with relative position of items with respect to item of interest
          rel.pos.vec <-  order.vc - item.int.pos
          
          # increase instance counter by 1
          instance.counter <- instance.counter + 1
          
          if (len.intv > 0){
            for (i in 1:len.intv){
              
              # increase counter 
              counter <- counter + 1
              
              #row position
              this.row <- row.pos + i
              
              #add distance
              items.df$distance[this.row] <- rel.pos.vec[i]
              
              #add order
              items.df$order[this.row] <- items.intv$`order`[i]
              
              # item name
              this.item.name <- items.intv$`items`[i]
              
              #add item (number)
              items.df$item_n[this.row] <-  items.list$unique  %>% {which(. == this.item.name)}
              
              #add start time
              items.df$start_time[this.row] <- items.intv$`start`[i]
              
              #add end time
              items.df$end_time[this.row] <- items.intv$`end`[i]
              
              #add participant
              items.df$participant[this.row] <- items.intv$`participant`[i]
              
              #add session
              items.df$session[this.row] <- items.intv$`session`[i]
              
              #add instance
              items.df$instance[this.row] <- instance.counter
              
              #add id
              items.df$id[this.row] <- counter
              
            }
          }
          
          # update row position
          row.pos <- row.pos + len.intv
        }
      }
    }
    #remove NAs from data frame
    items.intv <- items.df[rowSums(is.na(items.df)) <7,]
    
    #filter data frame [remove item of interest]
    items.intv.filter <- items.intv[items.intv$distance != 0, ]
    
    #non.items.intv <- non.items.df
    itemsAround.ls <- mget(c("items.intv", "items.intv.filter"))
  }
  return(itemsAround.ls)
}

# ================ [2.0] find groups in items used in combination ================ 
around_summary <- function(item, session, search, method){
  # search "bef" = before
  # search "aft" = after
  # search "in" in this one

  # method 1 = search items according to time
  # method 2 = search items according to number of items
  
  ### get data of items used in combination 
  
  # select time or n_items function 
  if (method == 1){
    # time selection
    items.interaction <- items_around(item, session, search)
  }
  else if(method == 2){
    # number of items selection
    items.interaction <- items_around_n(item, session, search)
  }
  
  # assign data frame to variable
  items.filter <- items.interaction$items.intv.filter
  
  # get the number of intervals in df
  intervals <- max(items.filter$instance)
  intervals.vector <- unique(items.filter$instance)
  
  # get data frame of items name and number
  items.data <- data.frame(seq_along(items.list$unique), items.list$unique)
  # change cols name of data frame
  colnames(items.data) <- c("number", "item")
  
  # determine whether or not remove duplicates 
  remove = "y"
  
  # remove duplicates if remove equal to y 
  if (remove == "y"){
    # initialize variables to store indxs of items to remove
    duplicates <- vector()
    
    # remove duplicates
    for (i in intervals.vector){
      # get current interval
      this.interval <- items.filter[items.filter$instance == i, ]
      
      # find duplicates in this interval
      repeated <- this.interval$item[duplicated(this.interval$item)]
      
      # get length of repeated items
      len.rep <- length(repeated)
      
      # if duplicated in interval, remove all but one instance of the duplicate from the interval in df
      if (len.rep > 0){
        # iterate over length of repeated items
        for (r in 1:len.rep){
          # get repeated item
          item.rep <- repeated[r]
          
          # get rows of repeated item
          item.rows <- items.filter[which(items.filter$instance== i & items.filter$item == item.rep), ]
          
          # get length of repeated rows 
          len.rows <- length(item.rows$distance)
          
          for (l in 1:len.rows-1){
            # get number of row to remove
            item.id <- item.rows$id[l]
            # add number to duplicates vector
            duplicates <- c(duplicates, item.id)
          }
        }
      }
    }
    # get row of items to keep
    to.keep <- items.filter$id[!items.filter$id %in% duplicates]
    
    ### remove duplicates from data frame
    items.filter <- items.filter[items.filter$id %in% to.keep, ]
  }
  
  ### the analysis starts here
  
  ### singles

  if (length(items.filter$item_n) == 1){
    items.freqs <- data.frame("Var1" = items.filter$item_n, "Freq" = 1)
  }
  else if (length(items.filter$item_n) == 0){
    items.freqs <- data.frame("Var1" = NA, "Freq" = NA)
    }
  else{
    # get table of frequencies 
    items.freqs <- as.data.frame(sort(table(items.filter$item_n), decreasing = TRUE))
  }
  
  # change name of data frame
  names(items.freqs)[names(items.freqs) == "Var1"] <- "item_n"
  
  # add name to data frame
  single.freqs <- merge(items.freqs, items.data, by.x = "item_n", by.y = "number", all.x = TRUE)
  
  # change colum names
  colnames(single.freqs) <- c("item_n", "Freq", "item_name")
  
  # sort single.freqs
  single.freqs <- single.freqs[order(-single.freqs$Freq), ]  
  
  if (search != "in"){
    ### doubles
    
    # create data frame to store combinations from list
    cols.names <- c("instance", "item_1", "item_2", "selected")
    comb.df <- data.frame()
    for (col in cols.names){comb.df[[col]] <- as.numeric()}
    # add 1000 rows -PROVISIONAL SOLUTION - find a better way
    comb.df[nrow(comb.df) + 2000, ] <- NA
    
    # select columns
    #cols<-c(13, 6) #interval, item.n
    cols<-c(8, 2)
    
    # select data 
    select.data <- items.filter[, cols]
    # define n combinations
    n.comb <-2 
    # set combinations counter
    row.counter <- 1
    
    # iterate over length of intervals
    for (i in 1:intervals){
      
      # select this interval [ocurrence]
      pos.int <- select.data$instance %>% {which(. == i)}
      # get items this interval
      items.int <- select.data$item_n[pos.int]
      # get length of this interval 
      len.items <- length(items.int)
      
      # check length of this interval
      if (len.items > 1){
        # combine items
        int.comb <- combn(items.int, n.comb)
        # get length of int.comb 
        len.comb <- length(int.comb[1, ])
        
        # add combinations to data frame
        for (c in 1:len.comb){
          #add interval
          comb.df[row.counter, 1] <- i
          #add items 
          comb.df[row.counter, 2] <- int.comb[1, c]
          comb.df[row.counter, 3] <- int.comb[2, c]
          row.counter <- row.counter + 1
        }
      }
    }
    
    # clean df; remove na from data frame and select of columsn with items 
    comb.clean <- (comb.df[rowSums(is.na(comb.df)) <2,])[2:3]
    
    # get freqs df 
    comb.freqs <- as.data.frame(table(comb.clean))
    freqs.clean <- comb.freqs[comb.freqs$Freq != 0, ]
    
    # get names of first colum
    double.freqs <- merge(freqs.clean, items.data, by.x = "item_1", by.y = "number", all.x = TRUE)
    
    # get names of second column 
    double.freqs <- merge(double.freqs, items.data, by.x = "item_2", by.y = "number", all.x = TRUE)
    
    # change colos names of data frame
    colnames(double.freqs) <- c("item_1", "item_2", "Freq", "item_name_1", "item_name_2")
    
    # sort freqs.names data frame
    double.freqs <- double.freqs[order(-double.freqs$Freq), ]
  }
  
  if (search != "in"){
    # assign both data frames freqs to a variable
    freqs <- mget(c("single.freqs", "double.freqs"))
  }
  else{
    freqs <- single.freqs
  }
  
  # return data frame
  return(freqs)
}
# ================ [3.1] summary of items used in combination [pairs] ================ 
around_summary_ouput <- function(session, search, method){
  
  # search "bef" = before
  # search "aft" = after
  # search "in" in this one
  
  # method 1 = search items according to time
  # method 2 = search items according to number of items
  
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
  
  # call the items_analysis
  summary.df <- items_analysis(session) # to get total number of interactions [could be done with dplyr]
  
  ### create data frame to store data, this data frame will be returned 
  
  # set cooking sessions
  sessions <- participants * len.session
  
  # set number of interaction to determine wheter or not to get items around
  threshold <- 2
  
  # set number of combinations to get
  combs <-3
  
  # names of data frame columns of values to compute
  if(search == "in"){columns <- c("item", "set1", "set2", "set3", "set4", "set5", "set6")
  }
  else{columns <- c("item", "set1_a", "set1_b", "set2_a", "set2_b", "set3_a", "set3_b")
  }
  # get length of columns
  len.columns <- length(columns)
  
  # data frame is initialized
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df) + len.items.names,] <- NA # add empty NAs
  stats.df[, 1] <- items.names # add items names
  
  # add values to data frame 
  for (i in 1:len.items.names){
    
    # get current item
    this.item <- items.list$unique[i]

    # get number of interactions of current item
    this.int <- summary.df[i, 3] # colum number 3 contains total number of interactions 
    
    # check number of interaction for current item greater than threshold
    if (this.int >= threshold){
      
      cat(this.item, "\n")

      # call function around_summary to get data frame of combined items for current item
      combs.df <- around_summary(this.item, session, search, method)
      
      # check "search" and select data frame
      if (search == "in"){
        
        # get length of combs.df 
        len.combs <- length(combs.df$Freq)
        
        if (len.combs >= 6){
          # select double combinations data frame
          sel.combs <- as.matrix(combs.df[1:(combs*2), ]) # get top three doubles
          # add items to data frame 
          stats.df[i, 2:7] <- as.vector(sel.combs[1:6, 3])
        }
        else{
          sel.combs <- as.matrix(combs.df[1:len.combs, ]) # get top three doubles
          
          
          # add items to data frame 
          stats.df[i, 2:(1+len.combs)] <- as.vector(sel.combs[1:len.combs, 3])
        }
      }
      else{ # [bef and aft]
        
        # select double combinations data frame
        sel.combs <- as.matrix(combs.df$double.freqs[1:combs, ])  # get top three doubles
                                                                  # here to change to single
        # add items to data frame
        for (c in 1:combs){
          # get d combination from around summary df 
          stats.df[i, (c+c)] <- sel.combs[c, 4] # column 4 equals to "item_name_1" 
          stats.df[i, (c+c+1)] <- sel.combs[c, 5] # column 5 equals to "item_name_2"
        }
      }
    }
  }
  # return data frame
  return(stats.df)
}

# ================ [3.2] summary of items used in combination [singles] ================ 
around_summary_singles <- function(session, search, method){
  
  # search "bef" = before
  # search "aft" = after
  # search "in" in this one
  
  # method 1 = search items according to time
  # method 2 = search items according to number of items
  
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
  
  # call the items_analysis
  summary.df <- items_analysis(session) # to get total number of interactions [could be done with dplyr]
  
  ### create data frame to store data, this data frame will be returned 
  
  # set cooking sessions
  sessions <- participants * len.session
  
  # set number of interaction to determine wheter or not to get items around
  threshold <- 1
  
  # set number of combinations to get # number of items to select 
  combs <- 4
  
  # names of data frame columns of values to compute
  if(search == "in"){columns <- c("item", "set1", "set2", "set3", "set4", "set5", "set6")
  }
  else{columns <- columns <- c("item", "set1", "set2", "set3", "set4", "set5", "set6")
  }
  # get length of columns
  len.columns <- length(columns)
  
  # data frame is initialized
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df) + len.items.names,] <- NA # add empty NAs
  stats.df[, 1] <- items.names # add items names
  
  # add values to data frame 
  for (i in 1:len.items.names){
    
    # get current item
    this.item <- items.list$unique[i]
    
    # get number of interactions of current item
    this.int <- summary.df[i, 3] # colum number 3 contains total number of interactions 
    
    # check number of interaction for current item greater than threshold
    if (this.int >= threshold){
      
      cat(this.item, "\n")
      
      ### call function around_summary to get data frame of combined items for current item
      combs.df <- around_summary(this.item, session, search, method)
      
      # check "search" and select data frame
      if (search == "in"){
        
        # get length of combs.df 
        len.combs <- length(combs.df$Freq)
        
        if (len.combs >= 6){
          # select double combinations data frame
          sel.combs <- as.matrix(combs.df[1:(combs*2), ]) # get top three doubles
          # add items to data frame 
          stats.df[i, 2:7] <- as.vector(sel.combs[1:6, 3])
        }
        else{
          sel.combs <- as.matrix(combs.df[1:len.combs, ]) # get top three doubles
          # add items to data frame 
          stats.df[i, 2:(2+len.combs-1)] <- as.vector(sel.combs[1:len.combs, 3])
        }
      }
      else{ ### search == "bef" or search == "aft"
        
        # select double combinations data frame
        # sel.combs <- as.matrix(combs.df$double.freqs[1:combs, ])  # get top three doubles
        # here to change to single
        sel.combs <- as.matrix(combs.df$single.freqs[1:combs, ])  # get top three doubles [#4 the number of items to select]
        # add items to data frame
        for (c in 1:combs){
          # get d combination from around summary df 
          stats.df[i, c+1] <- sel.combs[c, 3] 
        }
      }
    }
  }
  # return data frame
  return(stats.df)
}


# ================ [1.0] items concurrent around according to number of items method================ 
items_around_n <- function(item, session, search){
  # search "bef" = before
  # search "aft" = after
  # search "in" in this one
  
  # select session
  if (session == "reg"){
    session.list <- reg.list
    sessions <- 1
  } else if (session == "new"){
    session.list <- new.list
    sessions <- 1
  } else if (session == "both"){
    session.lists <- c(reg.list, new.list)
    sessions <- 2
  }
  else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\" or \"both\" (reg and new")
  }
  
  ### function starts here 
  
  # initialize variable to count the frequency of item of interest
  item.count <- 0
  
  # set range (seconds after and before to search for items)
  # range <- 3
  
  # initialize variable to count the instance counter
  instance.counter <- 0
  
  # initialize counter
  counter <- 0 
  
  # initialize variable to count the row position
  row.pos <- 0
  
  # initialize
  counter <- 0 
  
  # create data frame to store calculations time interval
  cols.names <- c("distance", "item_n", "order", "start_time", "end_time", "participant", "session", "instance")
  items.df <- data.frame()
  for (col in cols.names){items.df[[col]] <- as.numeric()}
  # add rows -PROVISIONAL SOLUTION - find a better way
  if(session == "both"){items.df[nrow(items.df) + 12000, ] <- NA}
  items.df[nrow(items.df) + 4000, ] <- NA
  
  # set session when session is equal to both (sessions = 2)
  for (s in 1:sessions){
    
    # iterate over sessions
    if((sessions == 2) & (s == 1)){
      session.list <- reg.list
    }
    else if((sessions == 2) & (s == 2)){
      session.list <- new.list
    }
    # iterate over participants 
    for (p in 1:participants){
      
      # find indexes of item.interest
      item.indxs <- session.list[[p]]$`items` %>% {which(. == item)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # get length of this list
      len.list <- length(session.list[[p]]$`items`)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      ### check item indexes are not empty and follow TIME interval approach
      if (len.indxs > 0){
        
        # iterate over indexs
        for(index in 1:len.indxs){
          
          # get the position of item of interest
          item.pos <- item.indxs[index]
          
          ### change to after
          
          #before range
          if (search == "bef"){
            
            # get items n indexes before the item of interest
            incl.items <- c(item.pos-1, item.pos-2, item.pos-3)
            
            # remove indexes not in indexes list
            incl.clean <- incl.items[incl.items > 0]
            
            # merge item.pos and items befor 
            to.keep <- c(incl.clean, item.pos)
            to.keep <- sort(to.keep)
          }
          
          #after range
          if (search == "aft"){
            
            # get items n indexes before the item of interest
            incl.items <- c(item.pos+1, item.pos+2, item.pos+3)
            
            # remove indexes not in indexes list
            incl.clean <- incl.items[incl.items < len.list]
            
            # merge item.pos and items befor 
            to.keep <- c(incl.clean, item.pos)
            to.keep <- sort(to.keep)
          }
          
          #after range
          if (search == "in"){
            
            # item interest
            item.start <- session.list[[p]]$`start`[item.pos]
            item.end <- session.list[[p]]$`end`[item.pos]
            
            # get items in interval to exclude
            right.items.excl <- which((session.list[[p]]$`start`) >  item.end)
            left.items.excl <- which((session.list[[p]]$`start`) <  item.start)
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
          }
          
          ### remove duplicates from data frame
          items.intv <- session.list[[p]][session.list[[p]]$order %in% to.keep, ]
          
          ### get items distance
          
          # get position of item of interest within interval
          item.int.pos <- which(items.intv$`order` == item.pos)
          
          # length items interval
          len.intv <- length(items.intv$order)
          
          # vector with order positions for each item in items.intv
          order.vc <- c(1:len.intv)
          
          # vector with relative position of items with respect to item of interest
          rel.pos.vec <-  order.vc - item.int.pos
          
          # increase instance counter by 1
          instance.counter <- instance.counter + 1
          
          if (len.intv > 0){
            for (i in 1:len.intv){
              
              # increase counter 
              counter <- counter + 1
              
              #row position
              this.row <- row.pos + i
              
              #add distance
              items.df$distance[this.row] <- rel.pos.vec[i]
              
              #add order
              items.df$order[this.row] <- items.intv$`order`[i]
              
              # item name
              this.item.name <- items.intv$`items`[i]
              
              #add item (number)
              # items.df$item_n[this.row] <-  items.list$unique  %>% {which(. == this.item.name)}
              items.df$item_n[this.row] <-  items.list.n$items  %>% {which(. == this.item.name)}
              
              #add start time
              items.df$start_time[this.row] <- items.intv$`start`[i]
              
              #add end time
              items.df$end_time[this.row] <- items.intv$`end`[i]
              
              #add participant
              items.df$participant[this.row] <- items.intv$`participant`[i]
              
              #add session
              items.df$session[this.row] <- items.intv$`session`[i]
              
              #add instance
              items.df$instance[this.row] <- instance.counter
              
              #add id
              items.df$id[this.row] <- counter
              
            }
          }
          
          # update row position
          row.pos <- row.pos + len.intv
        }
      }
    }
    #remove NAs from data frame
    items.intv <- items.df[rowSums(is.na(items.df)) <7,]
    
    #filter data frame [remove item of interest]
    items.intv.filter <- items.intv[items.intv$distance != 0, ]
    
    #non.items.intv <- non.items.df
    itemsAround.ls <- mget(c("items.intv", "items.intv.filter"))
  }
  return(itemsAround.ls)
}