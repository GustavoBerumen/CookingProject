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
  
  # set percentage of items to get 
  percent <- 10 # 10 = 10%
  
  #### set items to search 
  
  # get type of item
  type.index <- items.list$unique  %>% {which(. == item)}
  type.item <- items.list$type[type.index]
  
  # get n percent of items
  frequent.df <- most_frequent(session, type.item)

  # get the index of item of interest from most_frequent df
  inst.indx <- frequent.df$item %>% {which(. == item)}
  
  # get number of instance of this item
  total.inst <- frequent.df$int_total[inst.indx]
  
  ### get the indexes of item of interest 
  indxs <- session.list$items  %>% {which(. == item)}
  
  # create data frame with only indexes of item of interest 
  item.df <- session.list[indxs, c(10:12, 1, 5:6)] # participant # session # order # start # end # duration
  
  # get unique p_corrected
  corrected.uniq <- unique(item.df$p_corrected)
    
  # initialize vector to add items 
  sel.indxs <-vector()
  sel.counter <- 0
  
  # iterate over data frame to get a random index for every 10 instances for a participant
  for (i in corrected.uniq){
    
    # get items that belong to this p_corrected
    this.p <- item.df$p_corrected   %>% {which(. == i)}
    
    # get length of vector
    len.p <- length(this.p)
    
    # check length of vector
    number.times <- ceiling(len.p / percent)
    
    # get an random number for every ten items 
    for (number in 1:number.times){
      # get the left side of the interval to search for the number
      left <- (percent * number) -9
      
      # get length of this interval
      if (len.p >= (percent * number)){
        # get the left side of the interval to search for the number
        right <- percent* number
        }
      else{
        # get the left side of the interval to search for the number
        right <- mod(len.p, number*percent)
        }
      # get a random number between intervals 
      random <- round(runif(1, left, right))
      
      # update select indexes counter 
      sel.counter <- sel.counter + 1

      # add random index to vector 
      sel.indxs[sel.counter] <- this.p[random]
      }
    }
  #select rows within item.df 
  sel.df <- item.df[sel.indxs, ]
  
  # remove p_corrected column 
  drops <- c("p_corrected")
  sel.df <- sel.df[ , !(names(sel.df) %in% drops)]
  
  # get data frame based on selected indexes 
  sel.df$start_hms =  as.character(seconds_to_period(sel.df$start))
  sel.df$end_hms =  as.character(seconds_to_period(sel.df$end))
  
  # save data frame as .csv
  fileName = paste("/", item, "_", session, ".csv", sep = '')
  fileOutput = paste(path_output, fileName, sep = '')
  write.csv(sel.df, fileOutput)
  
  # return data frame
  return(sel.df)
}



# ================ [3.0] get n instances of a specific item  ================
# type list
types <- c("c", "u")
# set session
session <- "both"

# get list of items to search 
for (type in types){
  
  # call most frequent function
  items.search <- most_frequent(session, type)
  
  # get length of columns item.search
  len.items <- length(items.search$item)
  
  for (item in 1:len.items){
    # call get n instances function using this item as input
    this.item <- as.vector(items.search$item[item])
    get_instances(session, this.item)
  }
}


