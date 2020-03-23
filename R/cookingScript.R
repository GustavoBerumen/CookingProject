##    Cooking Study analysis with R                   ##
##		Gustavo Berumen 2019/2020                            ##

# ================ install packages ================ 
# install.packages("xlsx")
# install.packages("tidyverse")
# install.packages("devtools")
# install.packages("janitor")
# install.packages("pivottabler")
# install.packages("plyr")
# install.packages("epiDisplay")
# install.packages("summarytools")
# install.packages("ggplot2")
# install.packages("hash")
# install.packages("dplyr")
# install.packages("rstudioapi")
# install.packages("gridExtra")
# install.packages("reshape2")
# install.packages("viridis")
# install.packages("here")
# install.packages("gghighlight")
# install.packages("ggpubr")
# install.packages("multcomp")
# install.packages("carData")
# install.packages("qdap")
# install.packages("pracma")
# install.packages("treemap")
# install.packages("ArgumentCheck")
# install.packages("lubridate")


# ================ init functions ================ 
# mode function
getMode <- function(v){
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ================ load libraries ================ 
library(tidyverse)
library(xlsx)
library(readxl)
library(frequency)
library(janitor)
library(pivottabler)
library(plyr)
library(epiDisplay)
library(summarytools)
library(plyr)
library(ggplot2)
library(hash)
library(dplyr)
library(rstudioapi)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(viridis)
library(here)
library(gghighlight)
library(ggpubr)
library(multcomp)
library(car)
library(qdap)
library(treemap)
library(ArgumentCheck)
library(lubridate)

# ================ 0. load files ================ 
# clear workspace
rm(list = ls()) 

# set path to file source
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./_cookingFiles")

# select files in folder
file.list <- list.files(pattern = "p[0-9]{2}.xlsx", full.names = TRUE)
file.items <- list.files(pattern = "items.xlsx", full.names = TRUE)

# initialize empty df
reg.list <- vector("list", length(file.list))
new.list <- vector("list", length(file.list))
items.list <- as.data.frame(read_excel(file.items, sheet = "all-items"))
recipe.list <- as.data.frame(read_excel(file.items, sheet = "recipes"))
inventory.list <- as.data.frame(read_excel(file.items, sheet = "inventory"))
time.list <- as.data.frame(read_excel(file.items, sheet = "time"))
participants.list<- as.data.frame(read_excel(file.items, sheet = "participants"))

# assign files to df
for (i in 1:length(file.list)){
  reg.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = 1))
  new.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = 2))
}

# set global variables
participants <- length(file.list)
#participants <- length(file.list) -1 
sessions <- 2
items.names <- sort(unique(items.list$unique))

len.items.names <- length(items.names)
items.types <- sort(unique(items.list$type))
len.items.types <- length(items.types)

# store reg.list and new.list into a data structure
df.lists <- list(reg.list, new.list)
lists_names <- c("reg", "new")

# order of columns in data frame
# colums_order <- c("order", "items_clean", "items_uniq",  "type", "start_s", "end_s", "duration_s", "participant")
colums_order <- c("order", "items_clean", "items_uniq",  "type", "start_s", "end_s", "duration_s", "participant", "comments")

# select columns reg and new data frames
for (l in 1:length(df.lists)){
  # select df
  this.list <- df.lists[[l]]
  
  for (p in 1:participants){
    # sort 'start_s' column
    this.list[[p]] <- this.list[[p]][order(this.list[[p]]$`start_s`), ]
    # order columns by columns_order
    this.list[[p]] <- this.list[[p]][c(colums_order, setdiff(names(this.list[[p]]),colums_order))]
    # select columns 
    this.list[[p]] <- this.list[[p]][1:9]
    # rename participant colum to cook
    names(this.list[[p]])[names(this.list[[p]]) == "participant"] <- "cook"
    names(this.list[[p]])[names(this.list[[p]]) == "items_clean"] <- "items"
    names(this.list[[p]])[names(this.list[[p]]) == "start_s"] <- "start"
    names(this.list[[p]])[names(this.list[[p]]) == "end_s"] <- "end"
    names(this.list[[p]])[names(this.list[[p]]) == "duration_s"] <- "duration"
    
    # add id and session columns
    this.list[[p]]$participant <- rep(p, length(this.list[[p]]$type))
    this.list[[p]]$session <- rep(lists_names[l], length(this.list[[p]]$type))
    
    #change order colum
    len.ord <- length(this.list[[p]]$order)
    this.list[[p]]$order <- c(1:len.ord) 
  }
  if (l == 1){
    reg.list <- this.list
  }
  else {
    new.list <- this.list
  }
}

#create concatenated df of reg and new
reg.list.concat <- do.call("rbind", reg.list)
new.list.concat <- do.call("rbind", new.list)

#create concatenated df of both reg and new
reg.list.concat$session <- "reg" #add session column to df 
new.list.concat$session <- "new"
# merge data frames
reg.new.concat <- rbind(reg.list.concat,  new.list.concat)


# ================ 1. counting of items interactions for each participants  ================ 
# get frequency of use for each item and for all participants

freqs.analysis <- function(session){
  # change the name more function like name  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    len.session.list <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session.list <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session.list <- 2
  }
  else{
    stop(sQuote(session), "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # get numbber of cooking sessions (take into acount whether  list is simple or concatenated )
  cooking.sessions <- len.session.list * participants 
  
  # data frame columns
  cols.names <-  c("items", 1:cooking.sessions)
  len.cols.names <- length(cols.names)
  
  # create freqs data frame to store calculations 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+len.items.names,] <- NA #add empty NAs
  freqs.df[,1] <- items.names
  
  # iterate over session types (reg and/or new)
  for (s in 1:len.session.list){

    # assign the session type to the session list object
    if (len.session.list == 2)
    {
      this.session.list <- session.list[[s]]
    }
    else if (len.session.list == 1){
      this.session.list <- session.list
    }
    
    # set p.factor to correct participants when having regular and new session
    if (s == 1 ){
      p.factor <- 0
    }
    if (s == 2){
      p.factor <- 10
    }
    
    #iterate over participants
    for (p in 1:participants){
      #get frequencies by participant
      freqs <-  cbind(table(this.session.list[[p]]$`items`))
      #get length of frequencies
      len.freqs <-  length(dimnames(freqs)[[1]])
      
      #add frequencies to data frame
      for (f in 1:len.freqs){
        #find name in data frame
        item.name <- dimnames(freqs)[[1]][f]
        #find column position of current item
        col.pos <- match(item.name, items.names)
        #add frequency to item or update value in data frame
        freqs.df[col.pos, 1 + p + p.factor] <- freqs[f]
      }
    }
  }
  return(freqs.df)
}

# ================ 1.5 counting of unique items for each session  ================ 

# change the name more function like name
freqs.uniq <- function(session){
  
  # get frequency of use for each item and for all participants
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    len.session.list <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session.list <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session.list <- 2
  }
  else{
    stop(sQuote(session), "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # get numbber of cooking sessions (take into acount whether  list is simple or concatenated )
  cooking.sessions <- len.session.list * participants 
  
  # data frame columns
  cols.names <-  c("items", 1:cooking.sessions)
  len.cols.names <- length(cols.names)
  
  # create freqs data frame to store calculations 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+len.items.names,] <- NA #add empty NAs
  freqs.df[,1] <- items.names
  
  # iterate over session types (reg and/or new)
  for (s in 1:len.session.list){
    
    # assign the session type to the session list object
    if (len.session.list == 2)
    {
      this.session.list <- session.list[[s]]
    }
    else if (len.session.list == 1){
      this.session.list <- session.list
    }
    
    # set p.factor to correct participants when having regular and new session
    if (s == 1 ){
      p.factor <- 0
    }
    if (s == 2){
      p.factor <- 10
    }
    
    #iterate over participants
    for (p in 1:participants){
      # select data for each participant (items and items_uniq)
      p.data <- this.session.list[[p]][1:2] 
      
      # get unique items from selected data
      uniq.list <- unique(p.data[1:2])
      
      # count unique items 
      freqs <- cbind(table(uniq.list$items))
      
      # get length of frequencies
      len.freqs <-  length(dimnames(freqs)[[1]])
      
      cat(p, "\n")
      
      #add frequencies to data frame
      for (f in 1:len.freqs){
        
        #find name in data frame
        item.name <- dimnames(freqs)[[1]][f]
        
        cat(item.name, "\n")
        
        #find column position of current item
        col.pos <- match(item.name, items.names)
        
        #add frequency to item or update value in data frame
        freqs.df[col.pos, 1 + p + p.factor] <- freqs[f]
      }
    }
  }
  return(freqs.df)
}

# ================ 1.75 getting values from unique item in unique items ================ 

uniq.item <- function(item, session){
  
  # initalize an argument check 
  check <- newArgCheck()
  
  # select data and check "session" argument is valid
  if (session == "reg"){
    session.list <- reg.list
    len.session.list <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session.list <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session.list <- 2
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
  
  # call freqs.uniq function 
  freqs <- freqs.uniq(session)
  
  # find item in list
  col.pos <- match(item, freqs$items)
  
  # subselect data
  item.list <- freqs[col.pos, 2:21]
  
  # transform list to data frame and get values 
  item.values <- do.call(rbind.data.frame, item.list)[[1]]
  
  # remove NA
  item.clean <- item.values[!is.na(item.values)]#
  
  # create data frames to store results
  
  # data frame columns
  cols.names <-  c("item", "sessions", "mean", "mode", "median", "min", "max", "session")
  len.cols.names <- length(cols.names)
  
  # create freqs data frame to store calculations 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+1,] <- NA #add empty NAs
  
  # get length session 
  sessions <- length(item.clean)
  
  # perform operations and add value to df
  freqs.df[[1]] <- item
  
  # find if empty was used in this "reg" and/or "new" session 
  if (sessions > 0){
    freqs.df[[1]] <- item
    freqs.df[[2]] <- sessions
    freqs.df[[3]] <- mean(item.clean)
    freqs.df[[4]] <- getMode(item.clean)
    freqs.df[[5]] <- median(item.clean)
    freqs.df[[6]] <- min(item.clean)
    freqs.df[[7]] <- max(item.clean)
    freqs.df[[8]] <- session
  }
  
  #return data frame with stored values
  return(freqs.df)
}


# ================ 2. descriptive statistics items and time for all participants ================ 

# function to get descriptive statistics of items interactions and items time
# this function calls freq.analysis function
items.analysis <- function(session){
  
  # select session 
  if (session == "reg"){
    list.concat <- reg.list.concat
    len.session.list <- 1
    }
  else if (session == "new"){
    list.concat <- new.list.concat
    len.session.list <- 1
  }
  else if (session == "both"){
    list.concat <- reg.new.concat
    len.session.list <- 2
  }
  else{
      stop(sQuote(session), " Wrong input: session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # set cooking sessions
  cooking.sessions <- participants * len.session.list
  
  # data frame columns and values to compute
  columns <- c("items", "n", "sum", "mean", "median", "sum.t", "mean.t", "median.t", "min.t", "max.t",  "range.t", "type")

  # get lenght of columns [len.stats -> len.columns]
  len.columns <- length(columns)
  
  ### create stats data frame
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df)+len.items.names,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  stats.df[, 1] <- items.names
  
  # add type of item to type column
  for (i in 1:len.items.names)
  {
    name.item <- stats.df[i,1]
    name.item.index <- items.list$unique %>% {which(. == name.item)}
    stats.df[i, 12] <- items.list$type[name.item.index]
  }
  
  ### create frequencies of all items and all participants
  freqs.all <- cbind(table(list.concat$`items`))
  freqs.all.names <- dimnames(freqs.all)[[1]]
  len.freqs.all <- length(freqs.all.names)
  
  # call frequency.analysis function
  freqs.df <- freqs.analysis(session)
  
  ### create indexes data frame
  max.freq <- max(freqs.all)
  # create data frame
  indexes.df <-data.frame()
  
  for (item in items.names){indexes.df[[item]] <- as.numeric()}
  indexes.df[nrow(indexes.df)+max.freq,] <- NA #vector() #add empty vectors
  
  # may have to be fixed
  # create duration data frame 
  duration.df <- data.frame(indexes.df)
  
  for (i in 1:len.freqs.all){
    item.freq <- freqs.all.names[i]
    item.freq.indexs <- list.concat$`items` %>% {which(. == item.freq)}
    len.item.freq.indexs <- length(item.freq.indexs)
    
    #add indexes to indexes data frame
    for (freq.index in 1:len.item.freq.indexs){
      #find item in indexes data frame 
      indexes.df[freq.index, item.freq] <- item.freq.indexs[freq.index]}
    #add duration to indexes data frame
    freq.cols <- c(1:length(item.freq.indexs))
    duration.df[freq.cols, item.freq] <- list.concat$`duration`[item.freq.indexs]

    #find column position of current item
    col.pos <- match(item.freq, items.names)

    # get n
    
    items.freq.row <- as.numeric(freqs.df[col.pos, 1:cooking.sessions + 1])
    item.freq.n <- length(items.freq.row[!is.na(items.freq.row)])
    
    # add n to data frame
    stats.df[col.pos, 2] <- item.freq.n

    # sum to data frame(df[,3])
    stats.df[col.pos, 3] <- len.item.freq.indexs
    
    # mean to data frame(df[,4])
    stats.df[col.pos, 4] <- len.item.freq.indexs/item.freq.n
    
    # median to data frame (df[,5])
    stats.df[col.pos, 5] <- median(items.freq.row, na.rm = TRUE)
    
    # sum.time to data frame (df[,6])
    freqs.durations <- list.concat$`duration`[item.freq.indexs]
    len.freqs.duration <- length(freqs.durations)
    stats.df[col.pos, 6] <- sum(freqs.durations)
    
    # mean.time to data frame (df[,7])
    stats.df[col.pos, 7] <- sum(freqs.durations)/len.freqs.duration
    
    # median.time to data frame (df[,8])
    stats.df[col.pos, 8] <- median(freqs.durations)
    
    # min.time and max.time (df[,9:10]) and add to data frame
    stats.df[col.pos, 9] <- min(freqs.durations)
    stats.df[col.pos, 10] <- max(freqs.durations)
    
    # range.time to data frame (df[,11])
    stats.df[col.pos, 11] <- stats.df[col.pos, 10]-stats.df[col.pos, 9]
  }
  return(stats.df)
}




# ================ 2.5 unique items per type and per session ================

unique.items <- function(session){
#summary table to get unique items and items per participant 
    
  # TO DO
  #remove food from this counting 
  #correlation analysis between CPGs and items used
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    len.session.list <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session.list <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session.list <- 2
  }
  else{
    stop(sQuote(session), "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # get numbber of cooking sessions (take into acount whether  list is simple or concatenated )
  cooking.sessions <- len.session.list * participants 
  
  # data frame columns
  cols.names <-  c("counting", 1:cooking.sessions)
  len.cols.names <- length(cols.names)
  
  # data frame rows
  uniq.names <- c("part", "c", "e", "u", "sum_uniq", "c_total", "e_total", "u_total", "sum_total")
  len.uniq.names <- length(uniq.names)
  
  # create data frame to store calculations 
  uniq.df <- data.frame()
  for (col in uniq.names){uniq.df[[col]] <- as.numeric()}
  uniq.df[nrow(uniq.df)+cooking.sessions,] <- NA #add empty NAs
  uniq.df[,1] <- 1:cooking.sessions
  
  # get unique items for each participant 
  for (p in 1:participants){
    # select participant data
    p.data <- session.list[[p]]
    
    # do counting for each item type for both unique and raw 
    for (uniq in 2:4){
      # select type
      type <- uniq.names[uniq]
      
      # select a subset of the data
      uniq.data <- p.data[p.data$type == type,]
      
      # count unique selected items
      uniq.items <- unique(uniq.data[c("items")])
      
      #add items counting to data frame
      uniq.df[p, uniq] <- length(uniq.items$items)
    }
    
    # count all unique selected items
    uniq.df[p, 5] <- length(unique(p.data$items))
    
    for (uniq in 6:8){
      # select type
      type <- uniq.names[uniq-4]
      
      # select a subset of the data
      uniq.data <- p.data[p.data$type == type,]
      
      # count unique selected items
      uniq.items <- unique(uniq.data[c("items", "items_uniq")])
      
      #add items counting to data frame
      uniq.df[p, uniq] <- length(uniq.items$items)
    }
    
    # count all unique selected items_uniq
    uniq.df[p, 9] <- length(unique(p.data[c("items", "items_uniq")])$items)
  }
  return(uniq.df)
}

# ================ 3. summary participants function ================ 
summary.analysis <- function(session){
  
  #function variables
  rows.participants <- c("p", "inter", "inter_CPG", "items", "items_CPG", "mean", "sd", "median", "mad", 
                         "range", "min", "max", "duration(m)", "inter_u", "inter_e", "items_u", "items_e",
                         "mean_cpg","mean_u","mean_e","median_cpg","median_u","median_e")
  
  len.stats <- length(rows.participants)
  
  ### create data frame
  summary.df <- data.frame()
  for (row in rows.participants){summary.df[[row]] <- as.numeric()}
  summary.df[nrow(summary.df)+participants,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  summary.df[,1] <- c(1:participants)
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    }
  else if (session == "new"){
    session.list <- new.list
    }
  
  #number of interactions per participant 
  for (p in 1:participants){
    
    # call freq.analysis function and assign to variable  
    freqs.data <- freqs.analysis(session)
    
    # interactions to data frame (df[p,2])
    summary.df[p, 2] <- length(session.list[[p]]$`items`)
    
    # interactions CPGs to data frame (df[p,3])
    summary.df[p, 3] <- length(session.list[[p]]$type[session.list[[p]]$type == "c"])
    
    # items (unique) to data frame (df[p,4])
    summary.df[p, 4] <- length(cbind(sort(freqs.data[p+1][,1])))
    
    # CPGs (unique) to data frame (df[p,5])
    summary.df[p, 5] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "c"]))
    
    # mean to data frame (df[p,6])
    p.data <- na.omit(freqs.data[p+1])
    summary.df[p, 6] <- mean(p.data[[1]])
    
    # sd to data frame (df[p,7])
    summary.df[p, 7] <- sd(p.data[[1]])
    
    # median to data frame (df[p,8])
    summary.df[p, 8] <- median(p.data[[1]])
    
    # median absolute deviation to data frame (df[p,9])
    summary.df[p, 9] <- mad(p.data[[1]])
    
    # range to data frame (df[p,10])
    summary.df[p, 10] <- diff(range(p.data[[1]]))
    
    # min to data frame (df[p,11])
    summary.df[p, 11] <- min(p.data[[1]])
    
    # most.frequent_1,_2,_3 to data frame (df[p,12:14])
    n<-1
    top.freqs <- (sort(unique(p.data[[1]]), decreasing =TRUE))[1:n]
    
    summary.df[p, 12] <- top.freqs
    
    #find indexes frequency values
    for (freq.value in 1:n)
    {
      top.freqs.indxs <- freqs.data[p+1] %>% {which(. == top.freqs[freq.value])}
    }
    
    # duration (seconds) session to data frame (df[p,13])
    summary.df[p, 13] <- round((max(session.list[[p]]$`end`) - min(session.list[[p]]$`start`))/60, 1)
    
    # u interactions to data frame (df[p,14])
    summary.df[p, 14] <- length(session.list[[p]]$type[session.list[[p]]$type == "u"])
    
    # e to data frame (df[p,15])
    summary.df[p, 15] <- length(session.list[[p]]$type[session.list[[p]]$type == "e"])
    
    # u (unique) to data frame (df[p,16])
    summary.df[p, 16] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "u"]))
    
    # CPGs (unique) to data frame (df[p,17])
    summary.df[p, 17] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "e"]))
    
    # CPGs (unique) to data frame (df[p,18])
    cpg.indxs <- session.list[[p]]$type %>% {which(. == "c")}
    
    summary.df[p, 18] <- mean(session.list[[p]]$type)
    
    # get the most frequent item by type category (still to add to table)
    #length p.data 
    len.p.data <- length(p.data[[1]])
    
    #add column to data frame 
    p.data$type <-cbind(1:length(p.data[[1]]))
    
    #add type column to p.data
    for (item in 1:len.p.data){
      pos <- as.numeric(row.names(p.data)[item])
      p.data$type[item] <- items.list$type[pos] 
    }
    
    #get most frequent item by type 
    for (type in 1:len.items.types){
      max.type <- max(p.data[[1]][p.data$type == items.types[type]])
      type.pos <-  items.types[type]
      type.index <- as.numeric(row.names(subset(p.data, p.data$type == type.pos & p.data[[1]] == max.type)))
      type.name <- items.list$unique[type.index] 
      cat("p:", p, type.pos, ": ", type.name, "\n")
    }
  }
  return(summary.df)
}

# ================ 4. duration function  ================ 

duration.analysis <- function(session, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  #create list to store data 
  items.section.ls <-  vector("list", length = participants)
  items.section.type.ls <-  vector("list", length = participants)

  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
  
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      
      # sections <- 10
      sections <- 4
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # matrix to store data by type and section
    items.section.type <- matrix( c(0,0,0), nrow=sections, ncol=3, byrow=FALSE)
    
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    #count the number of items per section 
    items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    #add matrix to list 
    items.section.type.ls[[p]] <- items.section.type
    
    #a <- c() 
    #for (i in 1:length(start.time.indexes))
    #  {a[i] <- (start.time.indexes[i+1]-start.time.indexes[i])}
    
    # get the frequency on each range
    for (section in 1:sections){
      #get index per section
      start.index <- start.time.indexes[section]
      end.index <- (end.time.indexes[section])
      
      #table for type items
      freqs <-rep(NA, len.items.types)
      freqs.tab <- table(session.list[[p]]$type[start.index:end.index])
      
      # finding frequency for each type item
      pos.C = names(freqs.tab)  %>% {which(. == "c")}
      if (length(pos.C)==1){
        items.section.type.ls[[p]][section, 1] <- freqs.tab[[pos.C]]}
      pos.e = names(freqs.tab)  %>% {which(. == "e")}
      if (length(pos.e)==1){
        items.section.type.ls[[p]][section, 2] <- freqs.tab[[pos.e]]}
      pos.u = names(freqs.tab)  %>% {which(. == "u")}
      if (length(pos.u)==1){
        items.section.type.ls[[p]][section, 3] <- freqs.tab[[pos.u]]}
      }
    }
  mylist <- list(items.section.ls,items.section.type.ls)
  return(mylist)
}


# ================ 4.25 item use per section/duration - interactions  ================ 
duration.sections <- function(session, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 3
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    # total.sum <- sum(session.list[[p]]$items, na.rm = TRUE)
    total.sum <- length(session.list[[p]]$items)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
        
        # select data 
        p.data <- session.list[[p]]$items[start.index:end.index]
        
        # get frequency of "item" in this section 
        section.freq <- length(p.data)
        
        # add value to data frame 
        freqs.df[p, section+1] <-  section.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "session.info")) 
  return(freq.item)
}


# ================ 4.25 item use per section/duration - items ================ 
duration.sections.items <- function(session, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 3
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    # total.sum <- sum(session.list[[p]]$items, na.rm = TRUE)
    total.sum <- length(session.list[[p]]$items)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
      
        # select data 
        #p.data <- session.list[[p]]$items[start.index:end.index]
        p.data <-  session.list[[p]][start.index:end.index, 1:2] #1:2 "item", and "item_uniq"
        
        # get frequency of "item" in this section 
        #section.freq <- length(unique(p.data))
        
        section.freq <- length(unique(p.data)$items)
        
        # add value to data frame 
        freqs.df[p, section+1] <-  section.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  freqs.ave <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  for (i in 1:sections){
    freqs.ave[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)/participants
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "freqs.ave","session.info")) 
  return(freq.item)
}




# ================ 4.25 item use per section/duration - items ================ 
duration.sections.uniq <- function(session, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 3
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    # total.sum <- sum(session.list[[p]]$items, na.rm = TRUE)
    total.sum <- length(session.list[[p]]$items)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
        
        # select data 
        #p.data <- session.list[[p]]$items[start.index:end.index]
        p.data <-  session.list[[p]][start.index:end.index, 1] #1:2 "item", and "item_uniq"
        
        # get frequency of "item" in this section 
        #section.freq <- length(unique(p.data))
        
        section.freq <- length(unique(p.data))
        
        # add value to data frame 
        freqs.df[p, section+1] <-  section.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  freqs.ave <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  for (i in 1:sections){
    freqs.ave[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)/participants
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "freqs.ave","session.info")) 
  return(freq.item)
}




# ================ 4.5 item use per section  ================ 
item.sections <- function(session, item, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 4
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    total.sum <- sum(session.list[[p]]$items == item, na.rm = TRUE)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
        
        # select data 
        p.data <- session.list[[p]]$items[start.index:end.index]
        
        # get frequency of "item" in this section 
        item.freq <- sum(p.data == item, na.rm = TRUE)
        
        # add value to data frame 
        freqs.df[p, section+1] <-  item.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(item, session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "session.info")) 
  return(freq.item)
}

# ================ 5. freqs duration function  ================ 

# frequencies duration (frequencies of all items and participants) 
freqsDur.analysis <- function(session){
    if (session == "reg"){
      session.list <- reg.list
    }
    else if (session == "new"){
      session.list <- new.list
    }
    
    #function variables
    cols.names <-  c("items/participants", 1:participants)
    len.cols.names <- length(cols.names)
    
    #create freqs data frame
    freqs.df <- data.frame()
    for (col in cols.names){freqs.df[[col]] <- as.numeric()}
    freqs.df[nrow(freqs.df)+len.items.names,] <- NA #add empty NAs
    freqs.df[,1] <- items.names
    
    #iterate over participants
    for (p in 1:participants){
      #get frequencies by participant
      freqs <-  cbind(table(session.list[[p]]$`items`))
      #get length of frequencies
      len.freqs <-  length(dimnames(freqs)[[1]])
      #add frequencies to data frame
      for (f in 1:len.freqs){
        #find name in data frame
        item.name <- dimnames(freqs)[[1]][f]
        #find indexes of item in participant
        item.indexs <- reg.list[[p]] %>% {which(reg.list[[p]]$`items` == item.name)}
        #get duration of item
        item.duration <- sum(reg.list[[p]]$duration[item.indexs])
        print(item.duration)
        #find column position of current item
        col.pos <- match(item.name, items.names)
        #add frequency to item
        freqs.df[col.pos, p+1] <- item.duration
      }
    }
    return(freqs.df)
}

# ================ 6. summary participants duration function  ================ 

summaryDur.analysis <- function(session){
  
  #function variables
  rows.participants <- c("p", "session.d", "items.d", "CPG.d", "utensil.d", "environment.d", "mean.d", "sd.d", "median.d", "mad.d", 
                         "range.d", "CPG.range.d","min.d", "CPG.min.d", "max.d", "CPG.max.d")
  
  len.stats <- length(rows.participants)
  
  ### create data frame
  summary.df <- data.frame()
  for (row in rows.participants){summary.df[[row]] <- as.numeric()}
  summary.df[nrow(summary.df)+participants,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  summary.df[,1] <- c(1:participants)
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  }
  else if (session == "new"){
    session.list <- new.list
  }
  
  #number of interactions per participant 
  for (p in 1:participants){
    
    # session duration to data frame (df[p,2])
    summary.df[p, 2] <- round((max(session.list[[p]]$`end`) - min(session.list[[p]]$`start`))/60, 1)
    
    # items duration to data frame (df[p,3])
    summary.df[p, 3] <- round((sum(session.list[[p]]$duration))/60,1)
    
    # CPGs duration to data frame (df[p,4])
    CPG.indexs <- session.list[[p]]$type %>% {which(. == "c")}
    summary.df[p, 4] <- round(sum(session.list[[p]]$duration[CPG.indexs])/60,1)
    
    # utensil duration to data frame (df[p,5])
    u.indexs <- session.list[[p]]$type %>% {which(. == "u")}
    summary.df[p, 5] <- round(sum(session.list[[p]]$duration[u.indexs])/60,1)
    
    # environment duration to data frame (df[p,6])
    e.indexs <- session.list[[p]]$type %>% {which(. == "e")}
    summary.df[p, 6] <- round(sum(session.list[[p]]$duration[e.indexs])/60,1)
  
    # mean to data frame (df[p,5])
    summary.df[p, 7] <- mean(session.list[[p]]$duration)
    
    # sd to data frame (df[p,6])
    summary.df[p, 8] <- sd(session.list[[p]]$duration)
    
    # median to data frame (df[p,7])
    summary.df[p,9] <- median(session.list[[p]]$duration)
    
    # median absolute deviation to data frame (df[p,8])
    summary.df[p, 10] <- mad(session.list[[p]]$duration)
    
    # range to data frame (df[p,9])
    summary.df[p, 11] <- diff(range(session.list[[p]]$duration))
    
    # range to data frame (df[p,10])
    summary.df[p, 12] <- diff(range(session.list[[p]]$duration[CPG.indexs]))
    
    # min duration to data frame (df[p,11])
    summary.df[p, 13] <- min(session.list[[p]]$duration)
    
    # min duration to data frame (df[p,12])
    summary.df[p, 14] <- min(session.list[[p]]$duration[CPG.indexs])
    
    # max duration to data frame (df[p,13])
    summary.df[p, 15] <- max(session.list[[p]]$duration)
    
    # max duration to data frame (df[p,14])
    summary.df[p, 16] <- max(session.list[[p]]$duration[CPG.indexs])
  }
  return(summary.df)
}

# ================ 7. individual small operations ================ 

### top ten CPGs based on different criteria 
get_top_ten <-  function(session, x){
  # 1 = c, 2 = u, 3 = e
  if(x < 1 || x > 3){
    stop(sQuote(x), "x should be between 1 and 3 ('C', 'u', or 'e')")
    }
  
  #get data 
  session_df <- items.analysis(session)
  
  #get subset 
  items.subset <- subset(session_df, session_df$type == items.types[x])
  
  #criteria indexs
  indexs <- c(2,3,6)
  
  #array to store matrices
  top.items.array <- list()
  
  #get top ten by different criteria 
  for (i in 1:3){
    # n = 1 (participants), sum = 3 (usages), sum.t = 5 
    category.sort <- items.subset[order(-items.subset[[ indexs[i] ]]),][1:12,]
    top.items <- cbind(category.sort[1:12,]$items, category.sort[1:12,][[ indexs[i] ]])
    top.items.array[[i]] <- top.items
  }
  return(top.items.array)
}


# ================ 8. items around items of interest ================ 
itemsAround.analysis <- function(item, session){
  # select an item 
  # make this more interactive [shiny library / app like function] 
  item.interest <- item
  
  # create a vector character and numeric to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  # dummy session variable
  # session <- session
  
  #select session
  if (session == "reg"){
    session.list <- reg.list
  } else if (session == "new"){
    session.list <- new.list
  } else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }
  
  #define range time of slices
  range <-  60

  # iterate over participants 
  for (p in 1:participants){
    # find indexes of item.interest
    item.indxs <- session.list[[p]]$`items` %>% {which(. == item.interest)}
    
    # get lenght of indexs
    len.indxs <- length(item.indxs)
    
    # check item indexes are not empty 
    if (len.indxs > 0){
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # find limits before
        bef.lim.end <- session.list[[p]]$`start`[item.indxs[index]]
        bef.lim.beg <- bef.lim.end - range
        
        # find limits after
        aft.lim.beg <- bef.lim.end + 1
        aft.lim.end <- aft.lim.beg + 60
        
        # find index items closer to bef.lim.end and bef.lim.beg
        bef.beg.indx <- which((session.list[[p]]$`start`- bef.lim.beg) >=0)[1]
        bef.end.indx <- item.indxs[index]
        
        # find index item closer to aft.lim.end and aft.lim.beg
        aft.beg.indx <- bef.end.indx + 1
        aft.end.indx <- tail(which ((session.list[[p]]$`start` - aft.lim.end) <=0), n=1)
        
        # get items within range (before)
        bef.items.range <- session.list[[p]]$`items`[bef.beg.indx : bef.end.indx]
        # get items within range (after)
        aft.items.range <- session.list[[p]]$`items`[aft.beg.indx : aft.end.indx]
        
        #remove item interest form beg.items.range
        if (tail(bef.items.range, n=1) == item.interest){
          bef.items.range <- head(bef.items.range, -1)
        }
        
        # get length of items before interest
        len.bef.range <-  length(bef.items.range)
        # get length of items after
        len.aft.range <-  length(aft.items.range)
        
        # check if there are items before the item of interest
        if (len.bef.range > 0){
          # find the end of the vector before
          index.bef <-length(items.before)
          
          # add items at the end of the vector before
          items.before[(index.bef+1):(index.bef+len.bef.range)] <- bef.items.range  
        }
        
        # check if there are items after the item of interest
        if (len.aft.range > 0){
          # find the end of the vector after
          index.aft <-length(items.after)
          
          # add items at the end of the vector after
          items.after[(index.aft+1):(index.aft+len.aft.range)] <- aft.items.range
        }
      }
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
  }
  
  # create before items data frame 
  bef.items <- data.frame(items = items.before, id = id.before)
  
  # create after items data frame 
  aft.items <- data.frame(items = items.after, id = id.after)
  
  # create df of freqs of before items
  bef.freqs <-table(items.before) %>% as.data.frame() %>% arrange(desc(Freq))
  names(bef.freqs)[1:2] <- c("items", "freqs")
  
  # create df of freqs of after items
  aft.freqs <-table(items.after) %>% as.data.frame() %>% arrange(desc(Freq))
  names(aft.freqs)[1:2] <- c("items", "freqs")
  
  itemsAround.ls <- mget(c("bef.items", "aft.items", "bef.freqs", "aft.freqs"))
  return(itemsAround.ls)
}

# get frequencies of before and after items

# transform frequencies to data frame

# output frequencies data frame






# ================ 9. items around matrix ================ 

itemsAround.analysis <- function(item, session, approach, n.search){
  # if approach == 1 search items according to time / if approach == 2 search items according to positions (n grams)
  
  # select an item of interest (item that would be the focus of the analysis)
  item.interest <- item
  
  # make variable to count the frequency of item of interest
  item.count <- 0
  
  # create a character and numeric vector to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  id.items.before <- character()
  id.items.after <- character()
  
  #define range time of slices (seconds?)
  range <-  n.search
  
  #define n_grams length 
  n.items <-  n.search
  
  #select session
  if (session == "reg"){
    session.list <- reg.list
  } else if (session == "new"){
    session.list <- new.list
  } else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }
  
  browser()
  # iterate over participants 
  for (p in 1:participants){
    
    # find indexes of item.interest
    item.indxs <- session.list[[p]]$`items` %>% {which(. == item.interest)}
    
    # get lenght of indexs
    len.indxs <- length(item.indxs)
    
    # add number of item of interest indexes to item count 
    item.count <- item.count + len.indxs
    
    ### check item indexes are not empty and follow time approach
    if ((len.indxs > 0) & (approach == 1)){
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # find limits before
        bef.lim.end <- session.list[[p]]$`start`[item.indxs[index]]
        bef.lim.beg <- bef.lim.end - range
        
        # find limits after
        aft.lim.beg <- bef.lim.end + 1
        aft.lim.end <- aft.lim.beg + 60
        
        # find index items closer to bef.lim.end and bef.lim.beg
        bef.beg.indx <- which((session.list[[p]]$`start`- bef.lim.beg) >=0)[1]
        bef.end.indx <- item.indxs[index]
        
        # find index item closer to aft.lim.end and aft.lim.beg
        aft.beg.indx <- bef.end.indx + 1
        aft.end.indx <- tail(which ((session.list[[p]]$`start` - aft.lim.end) <=0), n=1)
        
        # get items within range (before)
        bef.items.range <- session.list[[p]]$`items`[bef.beg.indx : bef.end.indx]
        # get items within range (after)
        aft.items.range <- session.list[[p]]$`items`[aft.beg.indx : aft.end.indx]
        
        #remove item interest form beg.items.range
        if (tail(bef.items.range, n = 1) == item.interest){
          bef.items.range <- head(bef.items.range, -1)
        }
        
        # get length of items before interest
        len.bef.range <-  length(bef.items.range)
        # get length of items after
        len.aft.range <-  length(aft.items.range)
        
        # check if there are items before the item of interest
        if (len.bef.range > 0){
          
          # find the end of the vector after
          index.bef <-length(items.before)
          # add items at the end of the vector before
          items.before[(index.bef+1):(index.bef+len.bef.range)] <- bef.items.range
          
          # add participant where these items were found 
          
        }
        
        # check if there are items after the item of interest
        if (len.aft.range > 0){
          # find the end of the vector after
          index.aft <-length(items.after)
          
          # add items at the end of the vector after
          items.after[(index.aft+1):(index.aft+len.aft.range)] <- aft.items.range
        }
      }
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
    
    ### check item indexes are not empty and follow number of elements approach
    if ((len.indxs > 0) & (approach == 2)){
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # create an empty vector (before items)
        item.bef.vec <-  c()
        
        # create an empty vector (after items)
        item.aft.vec <-  c()
        
        #iterate over len of n.items
        for (n.item in 1:n.items){
          
          # find position of item interest
          item.pos <-  item.indxs[index]
          
          # find item that was used before n positions (n = 1) 
          item.bef <- session.list[[p]]$`items`[item.pos - n.item]
          # find item that was used after n position (n = 1)
          item.aft <- session.list[[p]]$`items`[item.pos + n.item]
          
          # check item.bef is not empty
          if (length(item.bef) == 1){
            # add items to a vector (before items)
            item.bef.vec[n.item] <- item.bef
          }
          
          # check item.bef is not empty
          if (length(item.aft) == 1){
            # add items to a vector (after items)
            item.aft.vec[n.item] <- item.aft
          }
        }
        
        # get unique items from vector (before items)
        item.bef.vec <- unique(item.bef.vec)
        # get unique items from vector (after items)
        item.aft.vec <- unique(item.aft.vec)
        
        # add unique items to end of the vector (before)
        for (i.bef in 1:length(item.bef.vec)){
          
          # find the end of the vector before
          index.bef <-length(items.before)
          # add items at the end of the vector before
          items.before[index.bef+1] <- item.bef.vec[i.bef]
        }
        
        
        # add unique items to end of the vector (after)
        for (i.aft in 1:length(item.aft.vec)){
          
          # find the end of the vector before
          index.aft <-length(items.after)
          # add items at the end of the vector before
          items.after[index.aft +1 ] <- item.aft.vec[i.bef]
          }
        }
    }
    
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
      
      browser()
      len.items <- length(items.before)
      len.id <- (len.bef.id+1):(len.bef.id+ len.bef.vector)
      diff <-  len.items - len.id
      if (diff != 0){
        cat(p, "\n")
        break
      }
      cat(length())
  }

  # create before items data frame 
  bef.items <- data.frame(items = items.before, id = id.before)
  
  # create after items data frame 
  aft.items <- data.frame(items = items.after, id = id.after)
  
  # create df of freqs of before items
  bef.freqs <-table(items.before) %>% as.data.frame() %>% arrange(desc(Freq))
  names(bef.freqs)[1:2] <- c("items", "freqs")
  
  # create df of freqs of after items
  aft.freqs <-table(items.after) %>% as.data.frame() %>% arrange(desc(Freq))
  names(aft.freqs)[1:2] <- c("items", "freqs")
  
  itemsAround.ls <- mget(c("bef.items", "aft.items", "bef.freqs", "aft.freqs", "item.count"))
  
  return(itemsAround.ls)
}

# get frequencies of before and after items

# transform frequencies to data frame

# output frequencies data frame

# ================ 9.1 items around matrix ================ 

itemsAround.analysis <- function(item, session, approach, n.search){
  # if approach == 1 search items according to time / if apprach == 2 search items according to positions (n grams)
  
  # select an item of interest (item that would be the focus of the analysis)
  item.interest <- item
  
  # make variable to count the frequency of item of interest
  item.count <- 0
  
  # create a character and numeric vector to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  id.items.before <- character()
  id.items.after <- character()
  
  #define range time of slices
  range <-  n.search
  
  #define n_grams length 
  n.items <-  n.search
  
  #select session
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
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }
  
  # set session when session = both
  for (s in 1:sessions){
    # iterate over participants 
    
    if((sessions == 2) & (s == 1)){
      session.list <- reg.list
    }
    else if((sessions == 2) & (s == 2)){
      session.list <- new.list
    }
    
    
    for (p in 1:participants){
      
      # find indexes of item.interest
      item.indxs <- session.list[[p]]$`items-clean` %>% {which(. == item.interest)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      ### check item indexes are not empty and follow time approach
      if ((len.indxs > 0) & (approach == 1)){
        
        # iterate over indexs 
        for(index in 1:len.indxs){
          
          # find limits before
          bef.lim.end <- session.list[[p]]$`start-time_s`[item.indxs[index]]
          bef.lim.beg <- bef.lim.end - range
          
          # find limits after
          aft.lim.beg <- bef.lim.end + 1
          aft.lim.end <- aft.lim.beg + 60
          
          # find index items closer to bef.lim.end and bef.lim.beg
          bef.beg.indx <- which((session.list[[p]]$`start-time_s`- bef.lim.beg) >=0)[1]
          bef.end.indx <- item.indxs[index]
          
          # find index item closer to aft.lim.end and aft.lim.beg
          aft.beg.indx <- bef.end.indx + 1
          aft.end.indx <- tail(which ((session.list[[p]]$`start-time_s` - aft.lim.end) <=0), n=1)
          
          # get items within range (before)
          bef.items.range <- session.list[[p]]$`items-clean`[bef.beg.indx : bef.end.indx]
          # get items within range (after)
          aft.items.range <- session.list[[p]]$`items-clean`[aft.beg.indx : aft.end.indx]
          
          #remove item interest form beg.items.range
          if (tail(bef.items.range, n = 1) == item.interest){
            bef.items.range <- head(bef.items.range, -1)
          }
          
          # get length of items before interest
          len.bef.range <-  length(bef.items.range)
          # get length of items after
          len.aft.range <-  length(aft.items.range)
          
          # check if there are items before the item of interest
          if (len.bef.range > 0){
            
            # find the end of the vector after
            index.bef <-length(items.before)
            # add items at the end of the vector before
            items.before[(index.bef+1):(index.bef+len.bef.range)] <- bef.items.range  
          }
          
          # check if there are items after the item of interest
          if (len.aft.range > 0){
            # find the end of the vector after
            index.aft <-length(items.after)
            
            # add items at the end of the vector after
            items.after[(index.aft+1):(index.aft+len.aft.range)] <- aft.items.range
          }
        }
        # find the end of id vector before
        len.bef.id <- length(id.before)
        # find the end of id vector after
        len.aft.id <- length(id.after)
        
        # find the lengths of before and after vector for this participant
        len.bef.vector <-  length(items.before) - len.bef.id
        len.aft.vector <-  length(items.after) - len.aft.id
        
        # add id to vector id before
        id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
        # add id to vector id after
        id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
      }
      
      
      ### check item indexes are not empty and follow number of elements approach
      if ((len.indxs > 0) & (approach == 2)){
        
        # iterate over indexs 
        for(index in 1:len.indxs){
          
          # create an empty vector (before items)
          item.bef.vec <-  c()
          
          # create an empty vector (after items)
          item.aft.vec <-  c()
          
          #iterate over len of n.items
          for (n.item in 1:n.items){
            
            # find position of item interest
            item.pos <-  item.indxs[index]
            
            # find item that was used before n positions (n = 1) 
            item.bef <- session.list[[p]]$`items-clean`[item.pos - n.item]
            # find item that was used after n position (n = 1)
            item.aft <- session.list[[p]]$`items-clean`[item.pos + n.item]
            
            # check item.bef is not empty
            if (length(item.bef) == 1){
              # add items to a vector (before items)
              item.bef.vec[n.item] <- item.bef
            }
            
            # check item.bef is not empty
            if (length(item.aft) == 1){
              # add items to a vector (after items)
              item.aft.vec[n.item] <- item.aft
            }
          }
          
          # get unique items from vector (before items)
          item.bef.vec <- unique(item.bef.vec)
          # get unique items from vector (after items)
          item.aft.vec <- unique(item.aft.vec)
          
          # add unique items to end of the vector (before)
          for (i.bef in 1:length(item.bef.vec)){
            
            # find the end of the vector before
            index.bef <-length(items.before)
            # add items at the end of the vector before
            items.before[index.bef+1] <- item.bef.vec[i.bef]
          }
          
          
          # add unique items to end of the vector (after)
          for (i.aft in 1:length(item.aft.vec)){
            
            # find the end of the vector before
            index.aft <-length(items.after)
            # add items at the end of the vector before
            items.after[index.aft +1 ] <- item.aft.vec[i.bef]
          }
        }
      }
      
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
    
    }
  
  

  
  
  # create before items data frame 
  bef.items <- data.frame(items = items.before, id = id.before)
  
  # create after items data frame 
  aft.items <- data.frame(items = items.after, id = id.after)
  
  # create df of freqs of before items
  bef.freqs <-table(items.before) %>% as.data.frame() %>% arrange(desc(Freq))
  names(bef.freqs)[1:2] <- c("items", "freqs")
  
  # create df of freqs of after items
  aft.freqs <-table(items.after) %>% as.data.frame() %>% arrange(desc(Freq))
  names(aft.freqs)[1:2] <- c("items", "freqs")
  
  itemsAround.ls <- mget(c("bef.items", "aft.items", "bef.freqs", "aft.freqs", "item.count"))
  
  return(itemsAround.ls)
}

# get frequencies of before and after items

# transform frequencies to data frame

# output frequencies data frame


# ================ 9.3 items around matrix [time intervals] ================ 

itemsAround.analysis.time <- function(item, session, approach, n.search){
  
  # if approach == 1 search items according to time / if approach == 2 search items according to positions (n grams)
  
  # select an item of interest (item that would be the focus of the analysis)
  item.interest <- item
  
  # initialize variable to count the frequency of item of interest
  item.count <- 0
  
  # initialize variable to count the block 
  block.counter <- 0
  
  # initialize variable to count the row position
  row.pos <- 0
  
  # create a character and numeric vector to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  id.items.before <- character()
  id.items.after <- character()
  
  #define range time of slices
  range <-  n.search
  
  #define n_grams length 
  n.items <-  n.search
  
  
  # create data frame to store calculations time interval [approach == 1]
  cols.names <- c("distance", "item", "item.n", "order", "start.time", "duration",  "participant", "session", "interval")
  intv.items.df <- data.frame()
  for (col in cols.names){intv.items.df[[col]] <- as.numeric()}
  # add empty rows to end of data frame
  # add 1000 rows -PROVISIONAL SOLUTION - find a better way
  intv.items.df[nrow(intv.items.df) + 2000, ] <- NA
  
  
  
  # select session
  if (session == "reg"){
    session.list <- reg.list
    non.items.df <- reg.list.concat
    sessions <- 1
  } else if (session == "new"){
    session.list <- new.list
    non.items.df <- new.list.concat
    sessions <- 1
  } else if (session == "both"){
    session.lists <- c(reg.list, new.list)
    non.items.df <- reg.new.concat
    sessions <- 2
  }
  else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }
  
  # data frame that will keep the items "non items cluster"
  columns.keep <- c("items", "type",  "start", "end", "duration",  "participant", "session")
  non.items.df <- non.items.df[columns.keep]
  
  
  # set session when session = both
  for (s in 1:sessions){
    # iterate over participants 
    
    if((sessions == 2) & (s == 1)){
      session.list <- reg.list
    }
    else if((sessions == 2) & (s == 2)){
      session.list <- new.list
    }
    
    for (p in 1:participants){
      
      # find indexes of item.interest
      item.indxs <- session.list[[p]]$`items-clean` %>% {which(. == item.interest)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      # create variable to store items not in interval 
      non.items.intv <- vector(mode="numeric", length=0)
      
      ### check item indexes are not empty and follow --- TIME --- intervals approach
      if ((len.indxs > 0) & (approach == 1)){
        
        # iterate over indexs
        for(index in 1:len.indxs){
          # get the position of item of interest
          item.int <- item.indxs[index]
          
          # find limits before
          bef.lim.end <- session.list[[p]]$`start-time`[item.int]
          bef.lim.beg <- bef.lim.end - range
          
          # find limits after
          aft.lim.beg <- bef.lim.end + 1
          aft.lim.end <- aft.lim.beg + range + 1
        
          # item interest
          item.int.start <- session.list[[p]]$`start`[item.int]
          item.int.end <- session.list[[p]]$`end`[item.int]
          item.int.dur <- session.list[[p]]$`duration`[item.int]
          
          # intervals
          intv.start <- item.int.start - range
          intv.middle <- bef.lim.end
          intv.end <- item.int.end + range
  
          # get items in interval 
          # aft.interval <- items.start >= interval.end
          aft.items.excl <- which((session.list[[p]]$`start`) >=  intv.end)
          # bef.interval <- items.end <= interval.start
          bef.items.excl <- which((session.list[[p]]$`end`) <=  intv.start)

          # combined excluded items in an object 
          excl.aft.bef <- unique(sort(c(aft.items.excl, bef.items.excl)))
          
          # get length of vector
          len.session <- length(session.list[[p]]$order)
          
          # create variable to store items in interval 
          items.intv <- vector(mode="numeric", length=0)
          
          # get items in interval
          for (i in 1:len.session){
            if (i %in% excl.aft.bef == FALSE){
              items.intv <- c(items.intv, i)
            }
          }
        
          # get items distance
          
          # get position of item of interest within interval
          item.int.pos <- which(items.intv == item.int)
          
          # legth items interval
          len.intv <- length(items.intv)
          
          # vector with order positions for each item in items.intv
          order.vc <- c(1:len.intv)
          
          # vector with relative position of items with respect to item of interest
          rel.pos.vec <-  order.vc - item.int.pos
          
          # increase block counter by 1
          block.counter <- block.counter + 1

          for (i in 1:len.intv){
            #get position of this item interval
            this.item <- items.intv[i]
            
            #row position
            this.row <- row.pos + i
            print(this.row)
            
            #add distance
            intv.items.df$distance[this.row] <- rel.pos.vec[i]
            
            #add order
            intv.items.df$order[this.row] <- items.intv[i]
            
            #add item (name)
            this.item.name <- session.list[[p]]$`items-clean`[this.item]
            intv.items.df$item[this.row] <- this.item.name
            
            #add item (number)
            intv.items.df$item.n[this.row] <-  items.list$unique  %>% {which(. == this.item.name)}
            
            #add start time
            intv.items.df$start.time[this.row] <- session.list[[p]]$`start`[this.item]
            
            #add duration
            intv.items.df$duration[this.row] <- session.list[[p]]$`duration`[this.item]
            
            #add id
            intv.items.df$participant[this.row] <- session.list[[p]]$`participant`[this.item]
            
            #add session
            intv.items.df$session[this.row] <- session.list[[p]]$`session`[this.item]
            
            #add block
            intv.items.df$interval[this.row] <- block.counter
          }
          # dummy.df[, row.pos: row.pos + len.intv] <-  intv.items.df$order[row.pos: row.pos + len.intv]
          
          # update row position
          row.pos <- row.pos + len.intv
          
          # update non items interval 
          non.items.intv <- c(non.items.intv, items.intv)

          # add items to non items data frame 
          if (index == len.indxs)
          {
            # get unique items on non.items 
            non.items.intv <- unique(non.items.intv)
            
            # get length non.items 
            len.non.items <- length(non.items.intv)
            
            for (pos in 1:len.non.items){
              # set item to be removed
              this.item <- non.items.intv[pos]
              # subset the data frame
              non.items.df <- (non.items.df[!(non.items.df$order == this.item & non.items.df$id == p),])
            }
          }
          
                    
        }
      }
      
      ### check item indexes are not empty and follow number of elements approach
      if ((len.indxs > 0) & (approach == 2)){
        
        # iterate over indexs 
        for(index in 1:len.indxs){
          
          # create an empty vector (before items)
          item.bef.vec <-  c()
          
          # create an empty vector (after items)
          item.aft.vec <-  c()
          
          #iterate over len of n.items
          for (n.item in 1:n.items){
            
            # find position of item interest
            item.pos <-  item.indxs[index]
            
            # find item that was used before n positions (n = 1) 
            item.bef <- session.list[[p]]$`items`[item.pos - n.item]
            # find item that was used after n position (n = 1)
            item.aft <- session.list[[p]]$`items`[item.pos + n.item]
            
            # check item.bef is not empty
            if (length(item.bef) == 1){
              # add items to a vector (before items)
              item.bef.vec[n.item] <- item.bef
            }
            
            # check item.bef is not empty
            if (length(item.aft) == 1){
              # add items to a vector (after items)
              item.aft.vec[n.item] <- item.aft
            }
          }
          
          # get unique items from vector (before items)
          item.bef.vec <- unique(item.bef.vec)
          # get unique items from vector (after items)
          item.aft.vec <- unique(item.aft.vec)
          
          # add unique items to end of the vector (before)
          for (i.bef in 1:length(item.bef.vec)){
            
            # find the end of the vector before
            index.bef <-length(items.before)
            # add items at the end of the vector before
            items.before[index.bef+1] <- item.bef.vec[i.bef]
          }
          
          
          # add unique items to end of the vector (after)
          for (i.aft in 1:length(item.aft.vec)){
            
            # find the end of the vector before
            index.aft <-length(items.after)
            # add items at the end of the vector before
            items.after[index.aft +1 ] <- item.aft.vec[i.bef]
          }
        }
        # find the end of id vector before
        len.bef.id <- length(id.before)
        # find the end of id vector after
        len.aft.id <- length(id.after)
        
        # find the lengths of before and after vector for this participant
        len.bef.vector <-  length(items.before) - len.bef.id
        len.aft.vector <-  length(items.after) - len.aft.id
        
        # add id to vector id before
        id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
        # add id to vector id after
        id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
        }
      }
    }
  
  if(approach == 1){
    
    items.intv <- intv.items.df[rowSums(is.na(intv.items.df)) <9,]
    non.items.intv <- non.items.df
    itemsAround.ls <- mget(c("items.intv", "non.items.intv"))
  }
  
  # if session equal 2 [number of items]
  
  if(approach == 2){
    # create before items data frame 
    bef.items <- data.frame(items = items.before, id = id.before)
    
    # create after items data frame 
    aft.items <- data.frame(items = items.after, id = id.after)
    
    # create df of freqs of before items
    bef.freqs <-table(items.before) %>% as.data.frame() %>% arrange(desc(Freq))
    names(bef.freqs)[1:2] <- c("items", "freqs")
    
    # create df of freqs of after items
    aft.freqs <-table(items.after) %>% as.data.frame() %>% arrange(desc(Freq))
    names(aft.freqs)[1:2] <- c("items", "freqs")
    
    itemsAround.ls <- mget(c("bef.items", "aft.items", "bef.freqs", "aft.freqs", "item.count"))
  }

  return(itemsAround.ls)
}

# get frequencies of before and after items

# transform frequencies to data frame

# output frequencies data frame


# ================ 9.4 re-make items around matrix ================ 
# this function works properly 

re_itemsAround.analysis <- function(item, session, approach, n.items){

  # if approach == 1 search items according to time 
  # if approach == 2 search items according to positions (n grams)
  
  # select an item of interest (item that would be the focus of the analysis)
  item.interest <- item
  
  # make variable to count the frequency of item of interest
  item.count <- 0
  
  # create a character and numeric vector to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  id.items.before <- character()
  id.items.after <- character()
  
  #select session
  if (session == "reg"){
    session.list <- reg.list
    } else if (session == "new"){
      session.list <- new.list
    }else if (session == "both"){
      session.list  <- reg.new.concat
    } else {
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\" or \"both\" ")
    }
  
  #define range time of slices
  # range <-  30
  range <- n.items
  
  #define n_grams length (elements to be searched)
  #n.items <-  1
  n.items <- n.items
  
  if (session == "both"){
    
    # find indexes of item.interest
    item.indxs <- session.list$`items` %>% {which(. == item.interest)}
    
    # get E subset A (average uses of A across all session). A is the item of interest 
    E_A <- length(item.indxs) / (participants*2)
    
    # get all the items that follow the use of item A 
    
    
    
    # get E subset B (average uses of B item after/before item of interest A)
    
    # get lenght of indexs
    len.indxs <- length(item.indxs)
    
    # add number of item of interest indexes to item count 
    item.count <- item.count + len.indxs
  }

  
  # iterate over participants 
  for (p in 1:participants){
    
    # find indexes of item.interest
    item.indxs <- session.list[[p]]$`items` %>% {which(. == item.interest)}
    
    # get lenght of indexs
    len.indxs <- length(item.indxs)
    
    # add number of item of interest indexes to item count 
    item.count <- item.count + len.indxs
    
    # check item indexes are not empty and follow time approach
    if ((len.indxs > 0) & (approach == 1)){
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # find limits before
        bef.lim.end <- session.list[[p]]$`start`[item.indxs[index]]
        bef.lim.beg <- bef.lim.end - range
        
        # find limits after
        aft.lim.beg <- bef.lim.end + 1
        aft.lim.end <- aft.lim.beg + 60
        
        # find index items closer to bef.lim.end and bef.lim.beg
        bef.beg.indx <- which((session.list[[p]]$`start`- bef.lim.beg) >=0)[1]
        bef.end.indx <- item.indxs[index]
        
        # find index item closer to aft.lim.end and aft.lim.beg
        aft.beg.indx <- bef.end.indx + 1
        aft.end.indx <- tail(which ((session.list[[p]]$`start` - aft.lim.end) <=0), n=1)
        
        # get items within range (before)
        bef.items.range <- session.list[[p]]$`items`[bef.beg.indx : bef.end.indx]
        # get items within range (after)
        aft.items.range <- session.list[[p]]$`items`[aft.beg.indx : aft.end.indx]
        
        #remove item interest form beg.items.range
        if (tail(bef.items.range, n=1) == item.interest){
          bef.items.range <- head(bef.items.range, -1)
        }
        
        # get length of items before interest
        len.bef.range <-  length(bef.items.range)
        # get length of items after
        len.aft.range <-  length(aft.items.range)
        
        # check if there are items before the item of interest
        if (len.bef.range > 0){
          
          # find the end of the vector before
          index.bef <-length(items.before)
          # add items at the end of the vector before
          items.before[(index.bef+1):(index.bef+len.bef.range)] <- bef.items.range  
        }
        
        # check if there are items after the item of interest
        if (len.aft.range > 0){
          # find the end of the vector after
          index.aft <-length(items.after)
          
          # add items at the end of the vector after
          items.after[(index.aft+1):(index.aft+len.aft.range)] <- aft.items.range
        }
      }
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
    
    if ((len.indxs > 0) & (approach == 2)){
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # find position of item interest 
        item.pos <-  item.indxs[index]
        
        # find item that was used before n positions (n = 1) 
        item.bef <- session.list[[p]]$`items`[item.pos - 1]
        
        # find item that was used after n position (n = 1)
        item.aft <- session.list[[p]]$`items`[item.pos + 1]
        
        # find the end of the vector before
        index.bef <-length(items.before)
        # add items at the end of the vector before
        items.before[index.bef+1] <- item.bef
        
        # find the end of the vector before
        index.aft <-length(items.after)
        # add items at the end of the vector before
        items.after[index.aft +1 ] <- item.aft
      }
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
  }
  
  # create before items data frame 
  bef.items <- data.frame(items = items.before, id = id.before)
  
  # create after items data frame 
  aft.items <- data.frame(items = items.after, id = id.after)
  
  # create df of freqs of before items
  bef.freqs <-table(items.before) %>% as.data.frame() %>% arrange(desc(Freq))
  names(bef.freqs)[1:2] <- c("items", "freqs")
  
  # create df of freqs of after items
  aft.freqs <-table(items.after) %>% as.data.frame() %>% arrange(desc(Freq))
  names(aft.freqs)[1:2] <- c("items", "freqs")
  
  itemsAround.ls <- mget(c("bef.items", "aft.items", "bef.freqs", "aft.freqs"))
  return(itemsAround.ls)
}

# get frequencies of before and after items

# transform frequencies to data frame

# output frequencies data frame


10.0

# ================ 10.0 go to specific item function ================ 
get.instance <- function(session, item){
  # check arguments and either break function or assign data set to variables 
  
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
  
  # check if item is in data set and either stop or continue  
  indxs <- session.list$items  %>% {which(. == item)}
  
  # get length of indxs
  len.indxs <- length(indxs)
  
  # stop if item is not in session.list
  if (len.indxs < 1){
    stop(sQuote(session), "item was not used in this set of/individual cooking session")
  }
  
  # get a random number in the range of len.indxs
  random.index <- runif(1, 1, len.indxs)
  
  # get random number row from session.list 
  instance.item <- reg.new.concat[indxs[random.index],]
  
  # get time from row 
  # time.instance <- instance.item$start
  
  # transform time.instance from seconds to h s m 
  # time.format <- seconds_to_period(time.instance)
  
  time.start <- instance.item$start
  time.end <- instance.item$end
  
  # print information
  cat(instance.item$participant, instance.item$session, "\n", time.start, time.end)
}

# ============== == re-make matrix from 9 ================ 

#item.interest 
item.interest <- "trashB"

# get data frames of items around 
session <- "reg"
interest.reg <- itemsAround.analysis("trashB", session, 2)
session <- "new"
interest.new <- itemsAround.analysis("trashB", session, 2)

# get E subset A (average uses of A across all session). A is the item of interest 
E_A <- (interest.reg$item.count + interest.new$item.count) / (participants*2)

# get unique items on new and reg df (concatenate those items in a vector)
items.matrix <- unique(c(as.character(interest.reg$bef.freqs[[1]]), as.character(interest.new$bef.freqs[[1]])))

# create dummy item of interest variable "A" and add to the list of items 
items.matrix <- c("A", items.matrix)

# sort items.matrix and add "id" of position in matrix 
items.matrix <- sort(items.matrix, decreasing = FALSE)

# create matrix
len.items.matrix <-  length(items.matrix)
prob.matrix <- matrix(NA, nrow = len.items.matrix, ncol = len.items.matrix)

#iterate over session 
for (s in 2){
  # select session reg or new 
  if (s == 1){
    session.list = reg.list
  }
  else if (s == 2){
    session.list = new.list
  }
  
  #iterate over participants 
  for (p in participants)
  {
    # find indexes of current item [this could be done in another way by looking in the dfs]
    interest.indexs <- reg.new.concat$`items-clean` %>% {which(. == item.interest)}
    
    # get indexes after item of interest 
    after.indexs <- interest.indexs + 1
    
    # find indexes of item.interest for this participant / session 
    item.indxs <- session.list[[p]]$`items-clean` %>% {which(. == item.interest)}
    
    # get lenght of indexs
    len.indxs <- length(item.indxs)
    
    #get A
    A <- len.indxs
    
    approach = 2
    
    # check item indexes are not empty and follow number of elements approach
    if ((len.indxs > 0) & (approach == 2)){
      
      # get all the items after item.interest current participant 
      items.aft <-  session.list[[p]]$`items-clean`[item.indxs + 1]
      
      # get unique items from items.aft vector 
      aft.unique <- unique(items.aft)
      
      len.aft.unique <- length(aft.unique)
      
      # count frequency of aft.unique items in current participant [items.aft]
      for (i in 1:len.aft.unique){
        
        # assign item to variable
        this.item <- aft.unique[i]
          
        # get the frequency of current item
        indxs.this <- items.aft %>% {which(. == this.item)}
        
          # get A -> B
        A.B <- length(indxs.this)
        
        # find the frequencies of current item across all sessions 
        B.indxs <- reg.new.concat$`items-clean` %>% {which(. == this.item)}
        
        # get E_B
        E_B <- length(B.indxs) / (participants* 2)
        
        # get B [all uses of B in this session]
        B <- length(session.list[[p]]$`items-clean` %>% {which(. == this.item)})
        
        # do calculation 
        this.prob <- sqrt(A.B) / sqrt((E_A - A)^2 * (E_B - B)^2)
        
        this.prob_a <- A.B / (E_A * E_B)
        
        # seems that it is not working - results greater than 1
        
        # add results to matrix
      }
      
      
      # iterate over indexs 
      for(index in 1:len.indxs){
        
        # find position of item interest
        item.pos <-  item.indxs[index]
        
        # find item that was used before n positions (n = 1) 
        item.bef <- session.list[[p]]$`items-clean`[item.pos - 1]
        
        # find item that was used after n position (n = 1)
        item.aft <- session.list[[p]]$`items-clean`[item.pos + 1]
      
        # find the end of the vector before
        index.bef <-length(items.before)
        # add items at the end of the vector before
        items.before[index.bef+1] <- item.bef
        
        # find the end of the vector before
        index.aft <-length(items.after)
        # add items at the end of the vector before
        items.after[index.aft +1 ] <- item.aft
      }
      # find the end of id vector before
      len.bef.id <- length(id.before)
      # find the end of id vector after
      len.aft.id <- length(id.after)
      
      # find the lengths of before and after vector for this participant
      len.bef.vector <-  length(items.before) - len.bef.id
      len.aft.vector <-  length(items.after) - len.aft.id
      
      # add id to vector id before
      id.before[(len.bef.id+1):(len.bef.id+ len.bef.vector)] <- p
      # add id to vector id after
      id.after[(len.aft.id+1):(len.aft.id+ len.aft.vector)] <- p
    }
    
    # get E_B after current item [get the index within after indexs vector]
    item.aft.indxs <- reg.new.concat$`items-clean`[after.indexs] %>% {which(. == item.aft)}
    
    #E_B current item
    length(item.aft.indxs) / (participants*2)
  
    # do calculation and add to matrix 
    
  }
}

#iterate over participants 


# ================ call functions  ================ 
sessions.names <- c("reg", "new")
names.functions <- c("freqs", "items", "summary", "duration", "freqsDur", "summaryDur")

# loop 
for (session in 1:sessions){
  str <- paste(names.functions, sessions.names[session], sep=".")
  assign(str[1], freqs.analysis(sessions.names[session]))
  assign(str[2], items.analysis(sessions.names[session]))
  assign(str[3], summary.analysis(sessions.names[session]))
  assign(str[4], duration.analysis(sessions.names[session]))
  assign(str[5], freqsDur.analysis(sessions.names[session]))
  assign(str[6], summaryDur.analysis(sessions.names[session]))
}

# manually 

#set item and session
item <-  "trashB"
item <- "knife"
#item <- "salt"
session <- "reg"

freqs.reg <- freqs.analysis(session)
items.reg <- items.analysis(session)
summary.reg <- summary.analysis(session)
duration.reg <- duration.analysis(session, 2)
freqsDur.reg <- freqsDur.analysis(session)
summaryDur.reg <- summaryDur.analysis(session)
# 2 search items according to number of items
# interest.reg <- itemsAround.analysis(item, session, 2, 3)
# interest.time.reg <- itemsAround.analysis.time(item, session, 1, 20) # (..., 1 [method, 1 equal to time], 30 [duration in seconds])
#items
interest.reg <- re_itemsAround.analysis(item, session, 2, 3)
interest.time.reg <- re_itemsAround.analysis(item, session, 1, 10)

uniq.reg <- unique.items(session)

#set item 
item <-  "onion"
#set session
session <- "new"

freqs.new <- freqs.analysis(session)
items.new <- items.analysis(session)
summary.new <- summary.analysis(session)
duration.new <- duration.analysis(session, 2)
freqsDur.new <- freqsDur.analysis(session)
summaryDur.new <- summaryDur.analysis(session)
# 2 search items according to number of items
interest.new <- itemsAround.analysis(item, session, 2, 3)

#set item 
item <-  "trashB"
#set session
session <- "both"
interest.both <- itemsAround.analysis(item, session, 2, 1)


# merge itemsAround.analysis() dfs
interest.both 
interest.reg
interest.new

########## clustering/interval analysis 

# get data around item of interest
test.time <- itemsAround.analysis.time("trashB", "reg", 1, 20)

# remove item of interest 
test.interval <- test.time[test.time$distance != 0, ]

# get clusters
clusters <- hclust(dist(test.interval[, c(1,6)]), method = 'average')

clusters <- hclust(dist(test.interval[, c(6)]), method = 'average')

clusters_all <- hclust(dist(reg.list.concat[, c(7,10)]), method = 'average')


# plot cluster
plot(clusters)
plot(clusters_all)

# select a desired number of clusters
clusterCut <- cutree(clusters, 12)
clusterCut_all <- cutree(clusters_all, 100)

# see clusters
table(clusterCut)
table(clusterCut_all)

# plot data 
ggplot(test.interval, aes(distance, duration, color = test.interval$item.n)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut)


ggplot(reg.list.concat, aes(`duration_s`, `start-time_s`, color = reg.list.concat$`duration_s`)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut_all)


# summary of most common items before item of interest

# filter data frame [get only items that were used before data]
test.before <- test.interval[test.interval$distance < 0, ]
sort(table(test.before$item), decreasing = TRUE)

# summary of most common items after item of interest 
test.after <- test.interval[test.interval$distance > 0, ]
sort(table(test.after$item), decreasing = TRUE)

# summary of most common items before and after item of interest 
sort(table(test.interval$item), decreasing = TRUE)



# ================ top ten ================ 

### get df of items.analysis 
items.both <- items.analysis("both")
items.reg <- items.analysis("reg")
items.new <- items.analysis("new")

### get top ten items by three different parameters 

# top all by n 
items.all.n <- items.both[order(items.both$'n', decreasing = TRUE),]
# top all by sum.t (duration)
items.all.dur <- items.both[order(items.both$'sum.t', decreasing = TRUE),]


# add percentage column to df of items 
items.both$sum.per = items.both$sum / sum(items.both$sum) *100
items.reg$sum.per = items.reg$sum / sum(items.reg$sum, na.rm = TRUE) *100
items.new$sum.per = items.new$sum / sum(items.new$sum, na.rm = TRUE) *100

# order items df and add cumulative percentage (also create a copy of the df to keep the order in the original df )

# all
items.all.int <- items.both[order(items.both$'sum', decreasing = TRUE),]
items.all.int$sum.cum <- round(cumsum(items.all.int$sum.per), 2)

#reg [# do a foor loop / do not repeat yourself #]
items.reg.int <- items.reg[order(items.reg$'sum', decreasing = TRUE),]
items.reg.int$sum.cum <- round(cumsum(items.reg.int$sum.per), 2)
#new
items.new.int <- items.new[order(items.new$'sum', decreasing = TRUE),]
items.new.int$sum.cum <- round(cumsum(items.new.int$sum.per), 2)


# get quantiles

percentiles <- seq(0, 1, by= 0.2)

quantiles.all <- quantile(items.all.int$sum.cum, percentiles, na.rm = TRUE)
quantiles.reg <- quantile(items.reg.int$sum.cum, percentiles, na.rm = TRUE)
quantiles.new <- quantile(items.new.int$sum.cum, percentiles, na.rm = TRUE)


# find position of object within a vector that meets a condition

# 80% of the interactions, all interactions  
all.80 <- items.all.int$items[1:which(items.all.int$sum.cum > 80)[1]]
# 50% of the interactions, all interactions 
all.50 <- items.all.int$items[1:which(items.all.int$sum.cum > 50)[1]]

# save data in a txt file
this.path <- "C:/Users/LPXJGB/Desktop/CHI-2020/_counting/participants/analysis/cookingStudy/results/"
file.name <- paste(this.path, "all.80.csv", sep = '')
write.csv(all.80, file.name)

file.name <- paste(this.path, "all.50.csv", sep = '')
write.csv(all.50, file.name)


# 50% of the interactions, reg interactions 
which(items.reg.int$sum.cum > 50)[1]
# 50% of the interactions, new interactions 
which(items.new.int$sum.cum > 50)[1]


### percentage and cumulative percentage 

items <- items.all.int$items
items.int <- items.all.int$sum
items.per <- (items.all.int$sum / sum(items.all.int$sum)) *100
items.cum <- round(cumsum(items.per), 2)


### histogram 
# create data frame
df <- data.frame(items = items.all.int$items, cumulative = items.cum, percent = items.per)

#basic histogram
ggplot(df, aes(x = cumulative)) + geom_histogram()
ggplot(df, aes(y = cumulative, x = items)) + geom_bar(stat="identity")



### bar plot

barplot(items.cum)
barplot(items.per)
qplot(data = df, percent, geom="density")
qplot(items.per, geom="density")



### histogram with a Normal Curve (Thanks to Peter Dalgaard)
x <- items.both$sum
h<-hist(x, breaks=20, col="grey", xlab="Interactions",
        main="Histogram with normal curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=1)

# Kernel Density Plot
d <- density(items.both$sum) # returns the density data
plot(d) # plots the results

# Filled Density Plot
plot(d, main="Kernel Density of Interactions per Item")
polygon(d, col="grey", border="darkgrey")


# plot n
# add a Normal Curve (Thanks to Peter Dalgaard)
x <- items.both$n
h<-hist(x, breaks=20, col="grey", xlab="n sessions",
        main="Histogram with normal curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=1)

# plot duration 
# add a Normal Curve (Thanks to Peter Dalgaard)
x <- items.both$sum.t
h<-hist(x, breaks=20, col="grey", xlab="duration",
        main="Histogram with normal curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=1)

x <- items.both$mean.t
h<-hist(x, breaks=20, col="grey", xlab="duration",
        main="Histogram with normal curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=1)

x <- items.both$mean
h<-hist(x, breaks=20, col="grey", xlab="duration",
        main="Histogram with normal curve")
xfit<-seq(min(x),max(x),length=100)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=1)


### cumulative frequency of interactions per range

interactions = items.all.int$sum
breaks = seq(1, 500, by = 50) 
interactions.cut = cut(interactions, breaks, right=FALSE) 
interactions.freq = table(interactions.cut)
interactions.cumfreq = cumsum(interactions.freq)
cbind(interactions.cumfreq)


### cumulative frequency graph

cumfreq0 = c(0, cumsum(interactions.freq)) 
plot(breaks, cumfreq0)
lines(breaks, cumfreq0)



#### plot histogram / quartiles (find where the accumulative frequency) /cumulative frequency 

# make my own dataframe for ggplot
x = items.all.int$sum
y = cumsum(x)
df <- data.frame(x = sort(x), y = y)

# make geom_histogram 
p <- ggplot(data = df, aes(x = x)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth = 1, boundary = 0,
                 color = "black", fill = "white")

# extract ggplot data
d <- ggplot_build(p)$data[[1]]

# make a data.frame for geom_line and geom_point
# add (0,0) to mimick base-R plots
df2 <- data.frame(x = c(0, d$xmax), y = c(0, d$y))

# combine plots: note that geom_line and geom_point use the new data in df2
p + geom_line(data = df2, aes(x = x, y = y),
              color = "darkblue", size = 1) +
  geom_point(data = df2, aes(x = x, y = y),
             color = "darkred", size = 1) +
  ylab("Frequency") + 
  scale_x_continuous(breaks = seq(0, 600, 50))

# save for posterity
ggsave("ggplot-histogram-cumulative-2.png")


### Combine histogram and density plots :
  
# Change line colors by groups
ggplot(df, aes(x=weight, color=sex, fill=sex)) +
geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
geom_density(alpha=0.6)+
geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")+
scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
labs(title="Weight histogram plot",x="Weight(kg)", y = "Density")+
theme_classic()


### make a dataframe for ggplot
set.seed(1)
x = runif(100, 0, 10)
y = cumsum(x)
df <- data.frame(x = sort(x), y = y)

# make geom_histogram 
p <- ggplot(data = df, aes(x = x)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth = 1, boundary = 0,
                 color = "black", fill = "white")

# extract ggplot data
d <- ggplot_build(p)$data[[1]]

# make a data.frame for geom_line and geom_point
# add (0,0) to mimick base-R plots
df2 <- data.frame(x = c(0, d$xmax), y = c(0, d$y))

# combine plots: note that geom_line and geom_point use the new data in df2
p + geom_line(data = df2, aes(x = x, y = y),
              color = "darkblue", size = 1) +
  geom_point(data = df2, aes(x = x, y = y),
             color = "darkred", size = 1) +
  ylab("Frequency") + 
  scale_x_continuous(breaks = seq(0, 10, 2))

# save for posterity
ggsave("ggplot-histogram-cumulative-2.png")


### sort and filter by type 

#c
items.c <- items.both[items.both$type =="c", ]

#sort by interactions column
items.c <- items.c[order(items.c$sum, decreasing = TRUE),]

#top ten cpgs
top.c <- items.c[, 1:10]

#u
items.u <- items.both[items.both$type =="u", ]

#sort by interactions column
items.u <- items.u[order(items.u$sum, decreasing = TRUE),]

#top ten u
top.u <- items.u[, 1:10]

#e
items.e <- items.both[items.both$type =="e", ]

#sort by interactions column
items.e <- items.e[order(items.e$sum, decreasing = TRUE),]

#top ten u
top.e <- items.e[, 1:10]






# ================ make matrix from 9 ================ 

# select session
session <- "reg"
# session <- "new"

# object containing df with items around matrix
around.df <- itemsAround.analysis("trashB", session, 2)
#around.df <- itemsAround.analysis("trashB", session, 2)

#get total sum of item of interest usages 
items.total <- sum(around.df$bef.freqs$freqs)

# get the items 
this.items.list <- around.df$bef.freqs$items

# get length items list  
len.this.items <- length(this.items.list)

# get len of nrows and ncols for matrix
len.matrix <- length(items.names)

# create matrix
items.matrix <- matrix(0, nrow = len.matrix, ncol = len.matrix)

# add diagonals of 1s to matrix
diag(items.matrix) <- 1

id.interest <- as.numeric(match("trashB", items.names))

# add item id to object df 
for (i in 1:len.this.items){
  
  #get current item
  this.item <- this.items.list[i]
  
  # find the id of item.bef
  id.item <- as.numeric(match(this.item, items.names))
  
  # add id to freqs in object df
  around.df$bef.freqs$item.id[[i]] <- id.item 
  
  # get raw frequency of id.item
  id.item.freq <- around.df$bef.freqs$freqs[[i]]
  
  # add item raw freq to matrix 
  items.matrix[id.interest, id.item] <- id.item.freq
  
  # add item probability to matrix
  items.matrix[id.item, id.interest] <- id.item.freq/items.total 
}

# ================ re-make matrix from 9 ================ 

# select session
session <- "reg"
# session <- "new"

# object containing df with items around matrix
around.df <- itemsAround.analysis("trashB", session, 2)
#around.df <- itemsAround.analysis("trashB", session, 2)

#get total sum of item of interest usages 
items.total <- sum(around.df$bef.freqs$freqs)

# get the items 
this.items.list <- around.df$bef.freqs$items

# get length items list  
len.this.items <- length(this.items.list)

# get len of nrows and ncols for matrix
len.matrix <- length(items.names)

# create matrix
items.matrix <- matrix(0, nrow = len.matrix, ncol = len.matrix)

# add diagonals of 1s to matrix
diag(items.matrix) <- 1

id.interest <- as.numeric(match("trashB", items.names))

# add item id to object df 
for (i in 1:len.this.items){
  
  #get current item
  this.item <- this.items.list[i]
  
  # find the id of item.bef
  id.item <- as.numeric(match(this.item, items.names))
  
  # add id to freqs in object df
  around.df$bef.freqs$item.id[[i]] <- id.item 
  
  # get raw frequency of id.item
  id.item.freq <- around.df$bef.freqs$freqs[[i]]
  
  # add item raw freq to matrix 
  items.matrix[id.interest, id.item] <- id.item.freq
  
  # add item probability to matrix
  items.matrix[id.item, id.interest] <- id.item.freq/items.total 
}


# ================ statistical analysis ================ 

# ------ t- tests ---- 

### t test #interactions 

reg <- summary.reg$inter
new <- summary.new$inter
# paired t-test
t.test(reg, new, paired=TRUE)

# create a data frame with type as factor 
reg.id <- rep("reg", 10)
new.id <- rep("new", 10)
 
session <- as.factor(c(reg.id, new.id))
interactions <- c(reg, new)

# create data frame
int.df <- data.frame(interactions, session)

# plot dots 
ggline(int.df, x = "session", y = "interactions", group = 1,
       add = c("mean_se", "jitter"), 
       color = "session", palette = c("#00AFBB", "#E7B800"),
       order = c("reg", "new"),
       xlab = "session", ylab = "interactions",
       title = "Number of interactions")



### t test #unique items

#t test #items 
reg <- summary.reg$items
new <- summary.new$items
# paired t-test
t.test(reg, new, paired=TRUE)

# create a data frame with type as factor 
reg.id <- rep("reg", 10)
new.id <- rep("new", 10)

session <- as.factor(c(reg.id, new.id))
interactions <- c(reg, new)

# create data frame
int.df <- data.frame(interactions, session)

# plot dots 
ggline(int.df, x = "session", y = "interactions", group = 1,
       add = c("mean_se", "jitter"), 
       color = "session", palette = c("#00AFBB", "#E7B800"),
       order = c("reg", "new"),
       xlab = "session", ylab = "unique items",
       title = "Number of (unique) items")




### t test #duration
reg <- summary.reg$`duration(m)`
new <- summary.new$`duration(m)`
# paired t-test
t.test(reg, new, paired=TRUE)

# create a data frame with type as factor 
reg.id <- rep("reg", 10)
new.id <- rep("new", 10)

session <- as.factor(c(reg.id, new.id))
interactions <- c(reg, new)

# create data frame
int.df <- data.frame(interactions, session)

# plot dots 
ggline(int.df, x = "session", y = "interactions", group = 1,
       add = c("mean_se", "jitter"), 
       color = "session", palette = c("#00AFBB", "#E7B800"),
       order = c("reg", "new"),
       xlab = "session", ylab = "duration (m)",
       title = "Duration (m) cooking session")




# reg
cpg_reg <- summary.reg$inter_CPG
u_reg <- summary.reg$inter_u
e_reg  <- summary.reg$inter_e

#new session
cpg_new <- summary.new$inter_CPG
u_new <- summary.new$inter_u
e_new  <- summary.new$inter_e

# paired t-test
t.test(cpg_reg, cpg_new, paired=TRUE)
t.test(u_reg, u_new, paired=TRUE)
t.test(e_reg, e_new, paired=TRUE)

#regular session (unique)
cpg_reg <- summary.reg$items_CPG
u_reg <- summary.reg$items_u
e_reg  <- summary.reg$items_e


#new session (unique)
cpg_new <- summary.new$items_CPG
u_new <- summary.new$items_u
e_new  <- summary.new$items_e



# ================ ANOVA analysis ================ 

### ANOVA # interactions by type

# interactions reg
# cpg_reg <- summary.reg$inter_CPG
# u_reg <- summary.reg$inter_u
# e_reg  <- summary.reg$inter_e
# interactions new
# cpg_new <- summary.new$inter_CPG
# u_new <- summary.new$inter_u
# e_new  <- summary.new$inter_e

summary.reg <- unique.items("reg")
summary.new <- unique.items("new")
  
# cpg_reg <- summary.reg$c
# u_reg <- summary.reg$u
# e_reg  <- summary.reg$e
# # interactions new
# cpg_new <- summary.new$c
# u_new <- summary.new$u
# e_new  <- summary.new$e


cpg_reg <- summary.reg$c_total
u_reg <- summary.reg$u_total
e_reg  <- summary.reg$e_total
# interactions new
cpg_new <- summary.new$c_total
u_new <- summary.new$u_total
e_new  <- summary.new$e_total

# create a data frame with type as factor 
c<- c(cpg_reg, cpg_new)
c_id <- rep("c", 20)
u<- c(u_reg, u_new)
u_id <- rep("u", 20)
e<- c(e_reg, e_new)
e_id <- rep("e", 20)

type <- c(c_id, u_id, e_id)
interactions <- c(c, u, e)

# create data frame
interactions.df <- data.frame(interactions, type)

# summary statistics by groups
group_by(interactions.df, interactions.df$type) %>%
  summarise(
    count = length(interactions),
    mean = mean(interactions, na.rm = TRUE),
    sd = sd(interactions, na.rm = TRUE)
  )

# plot means with bar errors
ggline(interactions.df, x = "type", y = "interactions", group =1,
       color = "type", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
       add = c("mean_se", "jitter"), 
       order = c("c", "u", "e"),
       ylab = "interactions", xlab = "type",
       title = "Number of interactions by type in all sessions)")

# compute the analysis of variance
aov.res <- aov(interactions ~ type, data = interactions.df)

# summary of results
summary(aov.res)

# multiple pairwise-comparisons
TukeyHSD(aov.res)

# multiple comparisions using general linear hypothesis tests
summary(glht(aov.res, linfct = mcp(type = "Tukey")))

# multiple comparison using t test
pairwise.t.test(interactions.df$interactions, interactions.df$type,
                p.adjust.method = "BH")

# check the homogeneity of variance assumption

# 1. homogeneity of variances 
# outliers will be marked, removal of them is advised 
plot(aov.res, 1)

# alternative to test for homogeneity of variances 
leveneTest(interactions ~ type, data = interactions.df)







### ANOVA # unique by type

# interactions reg
cpg_reg <- summary.reg$items_CPG
u_reg <- summary.reg$items_u
e_reg  <- summary.reg$items_e
# interactions new
cpg_new <- summary.new$items_CPG
u_new <- summary.new$items_u
e_new  <- summary.new$items_e

# create a data frame with type as factor 
c<- c(cpg_reg, cpg_new)
c_id <- rep("c", 20)
u<- c(u_reg, u_new)
u_id <- rep("u", 20)
e<- c(e_reg, e_new)
e_id <- rep("e", 20)

type <- c(c_id, u_id, e_id)
interactions <- c(c, u, e)

# create data frame
interactions.df <- data.frame(interactions, type)

# summary statistics by groups
group_by(interactions.df, interactions.df$type) %>%
  summarise(
    count = length(interactions),
    mean = mean(interactions, na.rm = TRUE),
    sd = sd(interactions, na.rm = TRUE)
  )

# plot means with bar errors
ggline(interactions.df, x = "type", y = "interactions", group =1,
       color = "type", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
       add = c("mean_se", "jitter"), 
       order = c("c", "u", "e"),
       ylab = "items  (unique)", xlab = "type",
       title = "Number of items (unique) by type in all sessions)")

# compute the analysis of variance
aov.res <- aov(interactions ~ type, data = interactions.df)

# summary of results
summary(aov.res)

# multiple pairwise-comparisons
TukeyHSD(aov.res)

# multiple comparisions using general linear hypothesis tests
summary(glht(aov.res, linfct = mcp(type = "Tukey")))

# multiple comparison using t test
pairwise.t.test(interactions.df$interactions, interactions.df$type,
                p.adjust.method = "BH")

# check the homogeneity of variance assumption

# 1. homogeneity of variances 
# outliers will be marked, removal of them is advised 
plot(aov.res, 1)

# alternative to test for homogeneity of variances 
leveneTest(interactions ~ type, data = interactions.df)









#duration 

### ANOVA # duration by type

# interactions reg
cpg_reg <- summaryDur.reg$CPG.d
u_reg <- summaryDur.reg$utensil.d
e_reg  <- summaryDur.reg$environment.d
# interactions new
cpg_new <- summaryDur.new$CPG.d
u_new <- summaryDur.new$utensil.d
e_new  <- summaryDur.new$environment.d

# create a data frame with type as factor 
c<- c(cpg_reg, cpg_new)
c_id <- rep("c", 20)
u<- c(u_reg, u_new)
u_id <- rep("u", 20)
e<- c(e_reg, e_new)
e_id <- rep("e", 20)

type <- c(c_id, u_id, e_id)
interactions <- c(c, u, e)

# create data frame
interactions.df <- data.frame(interactions, type)

# summary statistics by groups
group_by(interactions.df, interactions.df$type) %>%
  summarise(
    count = length(interactions),
    mean = mean(interactions, na.rm = TRUE),
    sd = sd(interactions, na.rm = TRUE)
  )

# plot means with bar errors
ggline(interactions.df, x = "type", y = "interactions",
       color = "type", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
       add = c("mean_se", "jitter"), 
       order = c("c", "u", "e"),
       ylab = "duration (m)", xlab = "type",
       title = "Duration (m) by items in all sessions")

# compute the analysis of variance
aov.res <- aov(interactions ~ type, data = interactions.df)

# summary of results
summary(aov.res)

# multiple pairwise-comparisons
TukeyHSD(aov.res)

# multiple comparisions using general linear hypothesis tests
summary(glht(aov.res, linfct = mcp(type = "Tukey")))

# multiple comparison using t test
pairwise.t.test(interactions.df$interactions, interactions.df$type,
                p.adjust.method = "BH")

# check the homogeneity of variance assumption

# 1. homogeneity of variances 
# outliers will be marked, removal of them is advised 
plot(aov.res, 1)

# alternative to test for homogeneity of variances 
leveneTest(interactions ~ type, data = interactions.df)







# ================ 10. plots ================ 

### 1 (summary.analysis)
#sessions interactions 
#interactions 
reg <- summary.reg$inter
new <- summary.new$inter

#duration
reg <- summary.reg$`duration(m)`
new <- summary.new$`duration(m)`

#items 
reg <- summary.reg$items
new <- summary.new$items

#mean interactions
reg <- summary.reg$mean
new <- summary.new$mean

#mean interactions
reg <- summary.reg$median
new <- summary.new$median

### barplot interactions session 
rows.barplot <- c("p", "reg", "new") 
barplot.df <- data.frame()
for (row in rows.barplot){barplot.df[[row]] <- as.numeric()}
barplot.df[nrow(barplot.df)+participants,] <- NA #add empty NAs
barplot.df[,1] <- c(1:10)
barplot.df[,2] <- reg
barplot.df[,3] <- new
barplot.m <- melt(barplot.df[,c('p','reg','new')],id.vars = 1)

#interactions
ggplot(barplot.m, aes(x = p, y = value)) +
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "all items interactions per session", x = "participant", y= "interactions", fill = "session") +
  scale_x_discrete(limits=c(1:10))

#duration
ggplot(barplot.m, aes(x = p, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "duration (m) per session", x = "participant", y= "minutes", fill = "session") +
  scale_x_discrete(limits=c(1:10))

#items
ggplot(barplot.m, aes(x = p, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "items per session", x = "participant", y= "items", fill = "session") +
  scale_x_discrete(limits=c(1:10))

#mean
ggplot(barplot.m, aes(x = p, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "mean interactions per session", x = "participant", y= "interactions", fill = "session") +
  scale_x_discrete(limits=c(1:10))

#median
ggplot(barplot.m, aes(x = p, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "median interactions per session", x = "participant", y= "interactions", fill = "session") +
  scale_x_discrete(limits=c(1:10))



### 2 (summary.analysis)
#interactions by type 
#regular session
cpg <- summary.reg$inter_CPG
u <- summary.reg$inter_u
e  <- summary.reg$inter_e

#regular session (unique)
cpg <- summary.reg$items_CPG
u <- summary.reg$items_u
e  <- summary.reg$items_e

#new session
cpg <- summary.new$inter_CPG
u <- summary.new$inter_u
e  <- summary.new$inter_e

#new session (unique)
cpg <- summary.new$items_CPG
u <- summary.new$items_u
e  <- summary.new$items_e

### barplot interactions session 
rows.barplot <- c("p", "cpg", "u", "e") 
barplot.df <- data.frame()
for (row in rows.barplot){barplot.df[[row]] <- as.numeric()}
barplot.df[nrow(barplot.df)+participants,] <- NA #add empty NAs
barplot.df[,1] <- c(1:10)
barplot.df[,2] <- cpg
barplot.df[,3] <- u
barplot.df[,4] <- e
barplot.m <- melt(barplot.df[,c('p','cpg','u', 'e')],id.vars = 1)
#interactions by type
ggplot(barplot.m, aes(x = p, y = value)) +
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "unique items by type - new session", x = "participant", y= "interactions", fill = "type") +
  scale_x_discrete(limits=c(1:10))



### 3 (barplot sesssion comparisons)
items.reg.comp <- items.reg[complete.cases(items.reg), ]
items.new.comp <- items.new[complete.cases(items.new), ]

#unique CPGs
reg.tab <- table(items.reg.comp$type)
new.tab <- table(items.new.comp$type)

#transformed to data frame
reg.items <- as.data.frame(reg.tab)
new.items <- as.data.frame(new.tab)

### barplot interactions session 
rows.barplot <- c("session", "item", "freq") 
barplot.df <- data.frame()
for (row in rows.barplot){barplot.df[[row]] <- as.numeric()}
barplot.df[nrow(barplot.df)+3,] <- NA #add empty NAs
barplot.df[1:3,1] <- c(1,1,1)
barplot.df[1:3,2] <- c("c","u","e")#reg.items$Var1
barplot.df[1,3] <- reg.items$Freq[1]
barplot.df[2,3] <- reg.items$Freq[3]
barplot.df[3,3] <- reg.items$Freq[2]

barplot.df[4:6,1] <- c(2,2,2)
barplot.df[4:6,2] <- c("c","u","e")
barplot.df[4,3] <- new.items$Freq[1]
barplot.df[5,3] <- new.items$Freq[3]
barplot.df[6,3] <- new.items$Freq[2]

#interactions by type
ggplot(barplot.df, aes(x = session, y = freq)) +
  geom_bar(aes(fill = item),stat = "identity",position = "dodge") +
  labs(title = "unique items by type - Sessions", x = "session", y= "items", fill = "type") +
  scale_x_discrete(limits=c("reg", "new"))



### 4 (cannot see the differences with ###3)
items.reg.comp <- items.reg[complete.cases(items.reg), ]
items.new.comp <- items.new[complete.cases(items.new), ]

#unique CPGs
reg.tab <- table(items.reg.comp$type)
new.tab <- table(items.new.comp$type)

#transformed to data frame
reg.items <- as.data.frame(reg.tab)
new.items <- as.data.frame(new.tab)

### barplot interactions session 
rows.barplot <- c("session", "item", "freq") 
barplot.df <- data.frame()
for (row in rows.barplot){barplot.df[[row]] <- as.numeric()}
barplot.df[nrow(barplot.df)+3,] <- NA #add empty NAs
barplot.df[1:3,1] <- c(1,1,1)
barplot.df[1:3,2] <- c("c","e","u")#reg.items$Var1
barplot.df[1:3,3] <- reg.items$Freq

barplot.df[4:6,1] <- c(2,2,2)
barplot.df[4:6,2] <- c("c","e","u")
barplot.df[4:6,3] <- new.items$Freq

#interactions by type
ggplot(barplot.df, aes(x = session, y = freq)) +
  geom_bar(aes(fill = item),stat = "identity",position = "dodge") +
  labs(title = "unique items by type - new session", x = "participant", y= "interactions", fill = "type") +
  scale_x_discrete(limits=c(1:2))



### 5 (plot of descriptive statistics)
reg <- (na.omit(items.reg))
new <- (na.omit(items.new))

#mean
c.reg <- mean(reg$mean[reg$type == "C"]) 
u.reg <- mean(reg$mean[reg$type == "u"]) 
e.reg <- mean(reg$mean[reg$type == "e"]) 

c.new <- mean(new$mean[new$type == "C"]) 
u.new <- mean(new$mean[new$type == "u"]) 
e.new <- mean(new$mean[new$type == "e"]) 

#median
c.reg <- median(reg$mean[reg$type == "C"]) 
u.reg <- median(reg$mean[reg$type == "u"]) 
e.reg <- median(reg$mean[reg$type == "e"]) 

c.new <- median(new$mean[new$type == "C"]) 
u.new <- median(new$mean[new$type == "u"]) 
e.new <- median(new$mean[new$type == "e"]) 


### barplot interactions session 
rows.barplot <- c("session", "item", "freq") 
barplot.df <- data.frame()
for (row in rows.barplot){barplot.df[[row]] <- as.numeric()}
barplot.df[nrow(barplot.df)+3,] <- NA #add empty NAs
barplot.df[1:3,1] <- c(1,1,1)
barplot.df[1:3,2] <- c("c","u","e")#reg.items$Var1
barplot.df[1,3] <- c.reg 
barplot.df[2,3] <- u.reg 
barplot.df[3,3] <- e.reg 

barplot.df[4:6,1] <- c(2,2,2)
barplot.df[4:6,2] <- c("c","u","e")
barplot.df[4,3] <- c.new
barplot.df[5,3] <- u.new
barplot.df[6,3] <- e.new

#interactions by type
ggplot(barplot.df, aes(x = session, y = freq)) +
  geom_bar(aes(fill = item),stat = "identity", position = "dodge") +
  labs(title = "mean interactions per session", x = "participant", y= "interactions", fill = "type") +
  scale_x_discrete(limits=c("reg","new"))


### 7 multiplot

#reg interactions
par(mfrow=c(5,2))
for (i in 1:10){
  plot(duration.reg[[1]][[i]], type ="o", xlab = "sections", ylab = "interactions", main = as.character(i))
}


#new interactions
par(mfrow=c(5,2))
for (i in 1:10){
  plot(duration.new[[1]][[i]], type ="o", xlab = "sections", ylab = "interactions", main = as.character(i))
}


#reg itneractions
y <- c(1:10)
par(mfrow=c(5,2))
for (i in 1:10){
  plot(y, duration.reg[[2]][[i]][,1],type="l",col="red", xlab = "sections", ylab = "interactions", ylim = c(0,30), main = as.character(i)) #cpgs
  lines(y, duration.reg[[2]][[i]][,3],col="green") #utensils
  lines(y, duration.reg[[2]][[i]][,2],col="blue") #environment
}

#new itneractions
y <- c(1:10)
par(mfrow=c(5,2))
for (i in 1:10){
  plot(y, duration.new[[2]][[i]][,1],type="l",col="red", xlab = "sections", ylab = "interactions", ylim = c(0,40), main = as.character(i)) #cpgs
  lines(y, duration.new[[2]][[i]][,3],col="green") #utensils
  lines(y, duration.new[[2]][[i]][,2],col="blue") #environment
}







### 8
#sessions interactions 
#interactions 
reg <- summaryDur.reg$inter
new <- summaryDur.new$inter

#duration
reg <- summary.reg$`duration(m)`
new <- summary.new$`duration(m)`

#items 
reg <- summary.reg$items
new <- summary.new$items

#mean interactions
reg <- summary.reg$mean
new <- summary.new$mean

#mean interactions
reg <- summary.reg$median
new <- summary.new$median

### barplot interactions session 
rows.barplot <- c("p", "reg", "new") 
ggplot(barplot.m, aes(x = p, y = value)) +
  labs(title = "duration (m) per session", x = "participant", y= "minutes", fill = "session") +
  scale_x_discrete(limits=c(1:10))

#items
ggplot(barplot.m, aes(x = p, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  labs(title = "items per session", x = "participant", y= "items", fill = "session") +
  scale_x_discrete(limits=c(1:10))







## 7 multiplot [plot cooking session progression]
session <- "reg"
session <- "new"


duration.reg <- duration.analysis(session, 1) # 1 slices 10 slices
duration.reg <- duration.analysis(session, 2) # minute
duration.reg <- duration.analysis(session, 3) # half minute


par(mfrow=c(1,1))
#par(mfrow=c(5,2))
for (i in 1:20){
  plot(duration.reg[[1]][[i]], type ="o", xlab = "minutes", ylab = "interactions", main = as.character(i))
  if (i ==1){
    mtext("new recipe", side = 3, cex = 1.2, line = 2.5)
  }
}

par(mfrow=c(5,4))
for (i in 1:20){
  plot(duration.reg[[1]][[i]], type ="o", xlab = "minutes", ylab = "interactions", main = as.character(i))
  if (i ==1){
    mtext("reg recipe", side = 3, cex = 1.2, line = 2.5)
  }
}




#reg.list[[1]]

#reg interactions
par(mfrow=c(5,2))

for (i in 1:20){
  
  par(mfrow=c(5,4))
  for (i in 1:20){
    plot(duration.reg[[1]][[i]], type ="o", xlab = "sections", ylab = "interactions", main = as.character(i))
  }
  
  'plot trashbin usage on top of a cooking session plot'
  # item.index <- freqs.reg$items  %>% {which(. == 'trashB')}
  # freqs.reg[item.index, ]
  
  item.indexs <- reg.list[[1]]$`items`  %>% {which(. == 'trashB')}
  item.pos <- reg.list[[1]]$`start`[item.indexs]
  item.dur <- reg.list[[1]]$`duration`[item.indexs]
  item.pos <- floor(item.pos/60)
  len.item.pos <-  length(item.pos)
  
  a<- rep(NA, 36)
  for (i in 1:len.item.pos)
  {
    a[item.pos[i]] <- 1
  }
  
  
  len.session <- length(duration.reg[[1]][[1]])
  plot(c(1:len.session), duration.reg[[1]][[2]], type ="o", xlab = "sections", ylab = "interactions", main = as.character(1))
  plot(c(1:len.session), a, type ="o", xlab = "sections", ylab = "interactions", main = as.character(1))
  points(c(1:len.session), a, col = "red")
  
  windows()
  
  plot(item.pos, col="red")
  
  #new interactions
  par(mfrow=c(5,2))
  for (i in 1:5){
    lines(y, duration.reg[[2]][[i]][,2],col="blue") #environment
  }
}




#interactions
ggplot(interest.reg$'bef.freqs', aes(x = items[1:10], y = freqs[1:10])) +
  labs(title = "all items interactions per session", x = "participant", y= "interactions", fill = "session") +
  scale_x_discrete(limits=c(1:10))


#---- ##### 11. interval time

item <- "trashB"
session <- "reg"

# call interval time function
interest.time <- itemsAround.analysis.time(item, session, 1, 20)

# select data 
interest.time.int <- interest.time$items.intv
interest.time.non <- interest.time$non.items.intv

# clean data

# remove item of interest
#int.tim.reg.cl <- interest.time.reg[interest.time.reg$distance != 0, ]
# filter data frame, only items used before items of interest
int.tim.reg <- interest.time.int[interest.time.int$distance < 0, ]

# add type to items (c,e,u)
len.items <- length(int.tim.reg$item)
for (i in 1:len.items){
  item <- int.tim.reg$item[i]
  item.pos <- items.list$unique %>% {which(. == item)}
  int.tim.reg$type[i] <- items.list$type[item.pos]
}

# load file 

# select file from folder 
path <- "_SpecificAnalysis/"
file <- 'items.around_clean.csv'
subtypes.list <- read.csv(paste0(path, file), header = T, sep = ",")

# add sub-type clean to items (c,e,u)
for (i in 1:len.items){
  item <- int.tim.reg$item[i]
  item.pos <- subtypes.list$item %>% {which(. == item)}
  int.tim.reg$subtype[i] <- subtypes.list$subtype_clean[item.pos]
}

# add sub-type words to items (c,e,u)
for (i in 1:len.items){
  item <- int.tim.reg$item[i]
  item.pos <- subtypes.list$item %>% {which(. == item)}
  subtype.w <- as.character(subtypes.list$subtype_words[item.pos])
  int.tim.reg$sub.w[i] <- subtype.w
  print(subtype.w)
}

# mean number of items used before
len.intervals <- length(unique(int.tim.reg$interval))
mean.bef <- length(int.tim.reg$item) / len.intervals

# table items type before
type.bef.tab <-sort(table(int.tim.reg$type), decreasing = TRUE)

# table items type before
type.bef.tab <-sort(table(int.tim.reg$type), decreasing = TRUE)

### table per interval

# create data frame to store data interval type data



# create data frame to store calculations time interval [approach == 1]
cols.names <- c("interval", "c", "e", "u")
intv.items.df <- data.frame()
for (col in cols.names){intv.items.df[[col]] <- as.numeric()}
# add empty rows to end of data frame
intv.items.df[nrow(intv.items.df) + len.intervals, ] <- NA

# get data about items types per interval and add them to data frame 
for (i in 1:len.intervals){
  
  # intialize "c", "e" , and "u"
  c <- 0
  e <- 0
  u <- 0
  
  # get this interval 
  this.intv <- int.tim.reg[int.tim.reg$interval == i,]
  
  # length this interval 
  len.this.intv <- length(this.intv$item)
  
  for (pos in 1:len.this.intv){
    #get item type 
    this.item <- this.intv$type[pos]
  
    # increase the variable  
    if (this.item == "c"){
      c <- c + 1}
    else if (this.item == "e"){
      e <- e + 1}
    else if (this.item == "u"){
      u <- u +1}
  }
  
  # add counting to data frame
  intv.items.df$interval[i] <- i
  intv.items.df$c[i] <- c
  intv.items.df$e[i] <- e
  intv.items.df$u[i] <- u
}

# descriptive statistics by type

# filter type data frame by type 
types <- c("c", "e", "u")
len.types <- length(types)
for (i in 1:len.types){
  # get current type
  this.column <- intv.items.df[[1+i]]
  
  #get decripotive statistics 
  mean.type <- mean(this.column)
  mode.type <- getMode(this.column)
  median.type <- median(this.column)

  cat("type: ", types[i], "\n",
      "mean: ", mean.type, "\n",
      "mode: ", mode.type, "\n",
      "median: ", median.type, "\n")
}


#################################### test for groups

# 1 ----- 1.1 & 3.1 (fresh + cutting)
# 2 ----- 1.2 & 3.5 (package + manipulating)

# 3 ----- 1.4 &/or 1.5 & 3.3 (essentials + food + heating)
# 4 ----- 1.6 & 3.2 (cleaning + cleaning)

interval <- c(1:95)

# get data about items types per interval and add them to data frame 
for (i in 1:len.intervals){
  
  # get this interval 
  this.intv <- int.tim.reg[int.tim.reg$interval == i,]
  
  # get length of this interval 
  len.this.intv <- length(this.intv$item)
  
  # get this items subtypes 
  this.subt <- sort(this.intv$subtype)
  
  # initialize subgroup 
  group <- 5
  sub.g <- c()
  
  #### test for groups
  # group 1 
  sub.g <- c(1.1, 3.1)
  sub.v <- 1 
  test <- is.element(sub.g, this.subt)
  if (sum(test) >= 2){
    group <- sub.v
  }
  
  # group 2
  sub.g <- c(1.2, 3.5)
  sub.v <- 2 
  test <- is.element(sub.g, this.subt)
  if (sum(test) >= 2){
    group <- sub.v
  }
  
  # group 3
  sub.g <- c(1.4, 3.3)
  sub.v <- 3 
  test <- is.element(sub.g, this.subt)
  if (sum(test) >= 2){
    group <- sub.v
  }
  sub.g <- c(1.5, 3.3)
  sub.v <- 3 
  test <- is.element(sub.g, this.subt)
  if (sum(test) >= 2){
    group <- sub.v
  }
  
  # group 4
  sub.g <- c(1.6, 3.2)
  sub.v <- 4 
  test <- is.element(sub.g, this.subt)
  if (sum(test) >= 2){
    group <- sub.v
  }
  
  # add group to data frame
  group.r[i] <- group
}


groups.df <- data.frame(interval, group.r)


# groups vector 
group.vc <- int.tim.reg$group
  
int.tim.reg[!is.na(int.tim.reg)]$group, na.rm = TRUE)


# make table 
group.table <- table(int.tim.reg$group)


################ make donut graph

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  labs(title="Clusters of items used before - Trash Bin - (-20 s) [raw approach]") +
  theme_void() +
  theme(legend.position = "center")


ggplot(data, aes(x=category, y=count)) + 
  geom_bar(stat = "identity") +
  labs(title="Clusters of items used before - Trash Bin - (-20 s)")
  


ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")








plot(int.tim.reg$subtype, int.tim.reg$interval)

rbind(int.tim.reg)

# make a table from function data 
int.reg.tab <- sort(table(int.tim.reg.cl$item), decreasing = TRUE)
# int.reg.tab <- sort(table(interest.time.non$item), decreasing = TRUE)


# transform table into a data frame
int.reg.df <- as.data.frame(int.reg.tab) 
colnames(int.reg.df) <- c("item", "freq")

# add type to items (c,e,u)
len.df <- length(int.reg.df$item)
for (i in 1:len.df){
  item <- int.reg.df$item[i]
  item.pos <- items.list$unique %>% {which(. == item)}
  int.reg.df$type[i] <- items.list$type[item.pos]
}







# donut chart 

# compute percentages
int.reg.df$fraction = int.reg.df$freq / sum(int.reg.df$freq)

# compute the cumulative percentages (top of each rectangle)
int.reg.df$ymax = cumsum(int.reg.df$fraction)

# compute the bottom of each rectangle
int.reg.df$ymin = c(0, head(int.reg.df$ymax, n=-1))

# compute label position
int.reg.df$labelPosition <- (int.reg.df$ymax + int.reg.df$ymin) / 2

# compute a good label
int.reg.df$label <- paste0(int.reg.df$item, ": ", int.reg.df$freq)


# Make the plot
ggplot(int.reg.df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=item)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()







# donut chart 

# compute percentages
int.reg.df$fraction = int.reg.df$freq / sum(int.reg.df$freq)

# compute the cumulative percentages (top of each rectangle)
int.reg.df$ymax = cumsum(int.reg.df$fraction)

# compute the bottom of each rectangle
int.reg.df$ymin = c(0, head(int.reg.df$ymax, n=-1))

# compute label position
int.reg.df$labelPosition <- (int.reg.df$ymax + int.reg.df$ymin) / 2

# compute a good label
int.reg.df$label <- paste0(int.reg.df$item, ": ", int.reg.df$freq)


# Make the plot
ggplot(int.reg.df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=item)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()
  



# Make the plot enhanced version)
ggplot(int.reg.df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=item)) +
  geom_rect() +
  geom_text(x=2, aes(y=labelPosition, label=label, color=item), size=2) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

#### treemap

# treemap
treemap(int.reg.df,
        index = c("type", "item"),
        vSize="freq",
        type="index",
        fontsize.labels=c(18,12),               # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("black","white"),    # Color of labels
        fontface.labels=c(2,1),                 # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),             # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                      # Where to place labels in the rectangle?
        overlap.labels= 0.5,                    # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels= F,
        
        border.col=c("black","white"),          # Color of borders of groups, of subgroups, of subsubgroups ....
        border.lwds=c(4,2),                     # Width of colors
        
        title="Items NOT used around \"Trash bin\" (outside +/- 20 s)",   # Customize your title
)


######################. working gantt split --------------------------------


session <- "reg"
#session <- "new"

#select session
if (session == "reg"){
  session.list <- reg.list
  }
if (session == "new"){
  session.list <- new.list
  }
else{
  stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }

# create empty list to store plots
pl <- list()

for (p in 1:participants){
  #create data frame by selecting specific columns from a particicipant reg.list data frame
  p.gantt <- session.list[[p]]
  
  # get axis position to mark in the plot
  #plot points/lines 
  item.indexs <- p.gantt$`items` %>% {which(. == 'garlic')}
  item.pos <- p.gantt$`start`[item.indexs]
  
  #rename columns data frame
  #names(p.gantt)[names(p.gantt) == "items"] <- "items"
  #names(p.gantt)[names(p.gantt) == "start"] <- "start"
  #names(p.gantt)[names(p.gantt) == "end"] <- "end"
  
  #get unique items names 
  p.acts <- unique(p.gantt$items)
  #get unique items types
  p.els <- unique(p.gantt$type)
  
  # merge content of "start" and "end" columns in a single column, and add an extra column identifying whether they are "start"/"end" 
  p.gantt <- gather(p.gantt, "state", "time", 5:6) %>% mutate(time = as.numeric(time), items = factor(items, p.acts[length(p.acts):1]), type = factor(type, p.els))
  
  #get range 
  p.min <- min(p.gantt$time)
  p.max <- max(p.gantt$time)
  p.range <- c(p.min, p.max)
  
  # make type a factor (this will help to fix the order and color of the plot)
  # p.gantt$type = factor(p.gantt$type, levels = c('c', 'e', 'u')) 
  p.gantt$type = factor(p.gantt$type, levels = c('c', 'e', 'u')) 
  
  #get indexs for horizontal rows of item to be highlighted
  u.indexs <- p.gantt$type %>% {which(. == "c")}
  #get unique items on indexs
  u.items <- unique(p.gantt$items[u.indexs])
  #get position of item to be highlighted
  u.pos <- u.items %>% {which(. == "garlic")}
  if (length(u.pos) == 0){
    u.pos <- 0
  } else {
    u.pos <- length(u.items) - u.pos + 1
  }
  
  #dummy variable to store the horizontal indexes to plot line 
  p.dummy <- data.frame(type = c(c('c', 'e', 'u')), types.pos = c(u.pos, 0, 0))
  
  # make the plot 
  pl[[p]] <- 
    ggplot(p.gantt, aes(time, items, color = type, group = order)) + geom_line(size = 3) +
    labs(x="time(s)", y=NULL, title = paste("p_", as.character(p), sep = ""), subtitle = recipe.list[ ,2][p]) + # select name of recipe [, 2]-reg and [, 3]-new
    xlim(p.min, p.max) + 
    theme(strip.text.y = element_text(angle = 0),  plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    facet_grid(rows = vars(type), scales = "free", space = "free") + 
    geom_vline(xintercept = item.pos, linetype="dotted", color = "black", size=1) +
    geom_hline(data = p.dummy, aes(yintercept = types.pos), color='coral', size=2, alpha=0.4)
}  
#plot multiple plots in a grid 
do.call(grid.arrange, pl[1:3])

pl[1]


#test <-     ggplot(p.gantt, aes(time, items, group = order)) + geom_line(size = 3)
#plot(test)







# ================ [0 ] load files ================ 
# clear workspace
rm(list = ls()) 

# set path to file source
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
#setwd("./_cookingFiles")

# select files in folder
file.list <- list.files(pattern = "p[0-9]{2}.xlsx", full.names = TRUE)
file.items <- list.files(pattern = "items.xlsx", full.names = TRUE)

# initialize empty df
reg.list <- vector("list", length(file.list))
new.list <- vector("list", length(file.list))
items.list <- as.data.frame(read_excel(file.items, sheet = "all-items"))
recipe.list <- as.data.frame(read_excel(file.items, sheet = "recipes"))
inventory.list <- as.data.frame(read_excel(file.items, sheet = "inventory"))
time.list <- as.data.frame(read_excel(file.items, sheet = "time"))
participants.list<- as.data.frame(read_excel(file.items, sheet = "participants"))

# assign files to df
for (i in 1:length(file.list)){
  reg.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = 1))
  new.list[[i]] <- as.data.frame(read_excel(file.list[i],  sheet = 2))
}

# set global variables
participants <- length(file.list)
#participants <- length(file.list) -1 
sessions <- 2
items.names <- sort(unique(items.list$unique))

len.items.names <- length(items.names)
items.types <- sort(unique(items.list$type))
len.items.types <- length(items.types)

# store reg.list and new.list into a data structure
df.lists <- list(reg.list, new.list)
lists_names <- c("reg", "new")

# order of columns in data frame
# colums_order <- c("order", "items_clean", "items_uniq",  "type", "start_s", "end_s", "duration_s", "participant")
colums_order <- c("order", "items_clean", "items_uniq",  "type", "start_s", "end_s", "duration_s", "participant", "comments")

# select columns reg and new data frames
for (l in 1:length(df.lists)){
  # select df
  this.list <- df.lists[[l]]
  
  for (p in 1:participants){
    # sort 'start_s' column
    this.list[[p]] <- this.list[[p]][order(this.list[[p]]$`start_s`), ]
    # order columns by columns_order
    this.list[[p]] <- this.list[[p]][c(colums_order, setdiff(names(this.list[[p]]),colums_order))]
    # select columns 
    this.list[[p]] <- this.list[[p]][1:9]
    # rename participant colum to cook
    names(this.list[[p]])[names(this.list[[p]]) == "participant"] <- "cook"
    names(this.list[[p]])[names(this.list[[p]]) == "items_clean"] <- "items"
    names(this.list[[p]])[names(this.list[[p]]) == "start_s"] <- "start"
    names(this.list[[p]])[names(this.list[[p]]) == "end_s"] <- "end"
    names(this.list[[p]])[names(this.list[[p]]) == "duration_s"] <- "duration"
    
    # add id and session columns
    this.list[[p]]$participant <- rep(p, length(this.list[[p]]$type))
    this.list[[p]]$session <- rep(lists_names[l], length(this.list[[p]]$type))
    
    #change order colum
    len.ord <- length(this.list[[p]]$order)
    this.list[[p]]$order <- c(1:len.ord) 
  }
  if (l == 1){
    reg.list <- this.list
  }
  else {
    new.list <- this.list
  }
}

#create concatenated df of reg and new
reg.list.concat <- do.call("rbind", reg.list)
new.list.concat <- do.call("rbind", new.list)

#create concatenated df of both reg and new
reg.list.concat$session <- "reg" #add session column to df 
new.list.concat$session <- "new"
# merge data frames
reg.new.concat <- rbind(reg.list.concat,  new.list.concat)




# ================ [1.0] counting items interactions per session  ================ 
items_interactions <- function(session){
# count how many times each times was used for every cooking session
# returns a data frame (matrix like)
  
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
    if (len.session == 1)
    {
      this.session <- session.list
    }
    else if (len.session == 2){
      this.session <- session.list[[s]]
    }
    
    #iterate over participants
    for (p in 1:participants){
      # select data for each participant (items and items_uniq)
      p.data <- this.session[[p]][1:2] 
      
      # get unique items from selected data
      uniq.list <- unique(p.data[1:2])
      
      # count unique items 
      freqs <- cbind(table(uniq.list$items))
      
      # get length of frequencies
      len.freqs <-  length(dimnames(freqs)[[1]])

      #add frequencies to data frame
      for (f in 1:len.freqs){
        
        #find name in data frame
        item.name <- dimnames(freqs)[[1]][f]
        
        #find column position of current item
        row.pos <- match(item.name, items.names)
        
        #add frequency to item or update value in data frame
        col.pos <- 1 + p + participants.factor[s]
        freqs.df[row.pos,  col.pos] <- freqs[f]
      }
    }
  }
  return(freqs.df)
}

# ================ [1.2] duration items sum per session ================ 

#make this one to work for both

items_duration <- function(session){
# frequencies duration (frequencies of all items and participants)
# previously as freqsDur.analysis
  
  # check variables
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
  cols.names <-  c("items", 1:participants)
  len.cols.names <- length(cols.names)
  
  # create freqs data frame
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+len.items.names,] <- NA #add empty NAs
  freqs.df[,1] <- items.names
  
  # iterate over participants
  for (p in 1:participants){
    
    #get frequencies by participants
    freqs <-  cbind(table(session.list[[p]]$`items`))
    
    #get length of frequencies
    len.freqs <-  length(dimnames(freqs)[[1]])
    
    #add frequencies to data frame
    for (f in 1:len.freqs){
      
      #find name in data frame
      item.name <- dimnames(freqs)[[1]][f]
      
      #find indexes of item in participant
      item.indexs <- session.list[[p]] %>% {which(session.list[[p]]$`items` == item.name)}
      
      #find column position of current item
      col.pos <- match(item.name, items.names)
      
      #get duration of item
      item.duration <- sum(session.list[[p]]$duration[item.indexs])
      
      #add frequency to item
      freqs.df[col.pos, p+1] <- item.duration
    }
  }
  return(freqs.df)
}

# ================ [1.4] summary counting interactions for a single item ================ 
item_summary <- function(item, session){
  
  # check function
  check <- newArgCheck()
  # select data and check "session" argument is valid
  if (session == "reg" || session == "new"){
    len.session <- 1
  }
  else if (session == "both"){
    len.session <- 2
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
  
  # call freqs.uniq function 
  freqs <- items_interactions(session)
  
  # find item in list
  col.pos <- match(item, freqs$items)
  
  # subselect data
  row.pos <- participants * len.session + 1
  item.list <- freqs[col.pos, 2:row.pos]
  
  # transform list to data frame and get values 
  item.values <- do.call(rbind.data.frame, item.list)[[1]]
  
  # remove NA
  item.clean <- item.values[!is.na(item.values)]
  
  # create data frames to store results
  
  # data frame columns
  cols.names <-  c("item", "sessions", "mean_interactions", "mode", "median", "min", "max", "session")
  len.cols.names <- length(cols.names)
  
  # create freqs data frame to store calculations 
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+1,] <- NA #add empty NAs
  
  # get length session 
  sessions <- length(item.clean)
  
  # perform operations and add value to df
  freqs.df[[1]] <- item
  
  # find if empty was used in this "reg" and/or "new" session 
  if (sessions > 0){
    freqs.df[[1]] <- item
    freqs.df[[2]] <- sessions
    freqs.df[[3]] <- format(round(mean(item.clean), 2))
    freqs.df[[4]] <- getMode(item.clean)
    freqs.df[[5]] <- median(item.clean)
    freqs.df[[6]] <- min(item.clean)
    freqs.df[[7]] <- max(item.clean)
    freqs.df[[8]] <- session
  }
  
  #return data frame with stored values
  return(freqs.df)
}

# ================ [2.0] summary interactions and time per session ================ 
items_analysis <- function(session){
  
  # select session 
  if (session == "reg"){
    list.concat <- reg.list.concat
    len.session.list <- 1
  }
  else if (session == "new"){
    list.concat <- new.list.concat
    len.session.list <- 1
  }
  else if (session == "both"){
    list.concat <- reg.new.concat
    len.session.list <- 2
  }
  else{
    stop(sQuote(session), " Wrong input: session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # set cooking sessions
  cooking.sessions <- participants * len.session.list
  
  # data frame columns and values to compute
  columns <- c("items", "sessions", "interactions_total", "mean_i", "median_i", "duration_total", "mean_d", "median_d", "min_d", "max_d", "type")
  #_i = interactions, _d = duration
  
  # get lenght of columns [len.stats -> len.columns]
  len.columns <- length(columns)
  
  ### create stats data frame
  stats.df <- data.frame()
  for (columns in columns){stats.df[[columns]] <- as.numeric()}
  stats.df[nrow(stats.df)+len.items.names,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  stats.df[, 1] <- items.names
  
  # add type of item to type column
  for (i in 1:len.items.names)
  {
    name.item <- stats.df[i,1]
    name.item.index <- items.list$unique %>% {which(. == name.item)}
    stats.df[i, 11] <- items.list$type[name.item.index]
  }
  
  ### create frequencies of all items and all participants
  freqs.all <- cbind(table(list.concat$`items`))
  freqs.all.names <- dimnames(freqs.all)[[1]]
  len.freqs.all <- length(freqs.all.names)
  # call frequency.analysis function
  #freqs.df <- freqs.analysis(session)
  freqs.df <- items_interactions(session)
  
  ### create indexes data frame
  max.freq <- max(freqs.all)
  # create data frame
  indexes.df <-data.frame()
  
  for (item in items.names){indexes.df[[item]] <- as.numeric()}
  indexes.df[nrow(indexes.df)+max.freq,] <- NA #vector() #add empty vectors
  
  # may have to be fixed
  # create duration data frame 
  duration.df <- data.frame(indexes.df)
  
  for (i in 1:len.freqs.all){
    item.freq <- freqs.all.names[i]
    item.freq.indexs <- list.concat$`items` %>% {which(. == item.freq)}
    len.item.freq.indexs <- length(item.freq.indexs)
    
    #add indexes to indexes data frame
    for (freq.index in 1:len.item.freq.indexs){
      #find item in indexes data frame 
      indexes.df[freq.index, item.freq] <- item.freq.indexs[freq.index]}
    #add duration to indexes data frame
    freq.cols <- c(1:length(item.freq.indexs))
    duration.df[freq.cols, item.freq] <- list.concat$`duration`[item.freq.indexs]
    
    #find column position of current item
    col.pos <- match(item.freq, items.names)
    
    # get n
    
    items.freq.row <- as.numeric(freqs.df[col.pos, 1:cooking.sessions + 1])
    item.freq.n <- length(items.freq.row[!is.na(items.freq.row)])
    
    # add n to data frame
    stats.df[col.pos, 2] <- item.freq.n
    
    # sum to data frame(df[,3])
    stats.df[col.pos, 3] <- len.item.freq.indexs
    
    # mean to data frame(df[,4])
    stats.df[col.pos, 4] <- round(len.item.freq.indexs/item.freq.n, 1)
    
    # median to data frame (df[,5])
    stats.df[col.pos, 5] <- round(median(items.freq.row, na.rm = TRUE), 1)
    
    # sum.time to data frame (df[,6])
    freqs.durations <- list.concat$`duration`[item.freq.indexs]
    len.freqs.duration <- length(freqs.durations)
    stats.df[col.pos, 6] <- sum(freqs.durations)
    
    # mean.time to data frame (df[,7])
    stats.df[col.pos, 7] <- round(sum(freqs.durations)/len.freqs.duration, 1)
    
    # median.time to data frame (df[,8])
    stats.df[col.pos, 8] <- median(freqs.durations)
    
    # min.time and max.time (df[,9:10]) and add to data frame
    stats.df[col.pos, 9] <- min(freqs.durations)
    stats.df[col.pos, 10] <- max(freqs.durations)
    
    # range.time to data frame (df[,11])
    #stats.df[col.pos, 11] <- stats.df[col.pos, 10]-stats.df[col.pos, 9]
  }
  return(stats.df)
}

# ================ [3.0] session slices items per section  ================ 
session_slices <- function(session, slicing){
# previously as duration.analysis()
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  #create list to store data 
  items.section.ls <-  vector("list", length = participants)
  items.section.type.ls <-  vector("list", length = participants)
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      
      # sections <- 10
      sections <- 4
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # matrix to store data by type and section
    items.section.type <- matrix( c(0,0,0), nrow=sections, ncol=3, byrow=FALSE)
    
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    #count the number of items per section 
    items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    #add matrix to list 
    items.section.type.ls[[p]] <- items.section.type
    
    #a <- c() 
    #for (i in 1:length(start.time.indexes))
    #  {a[i] <- (start.time.indexes[i+1]-start.time.indexes[i])}
    
    # get the frequency on each range
    for (section in 1:sections){
      #get index per section
      start.index <- start.time.indexes[section]
      end.index <- (end.time.indexes[section])
      
      #table for type items
      freqs <-rep(NA, len.items.types)
      freqs.tab <- table(session.list[[p]]$type[start.index:end.index])
      
      # finding frequency for each type item
      pos.C = names(freqs.tab)  %>% {which(. == "c")}
      if (length(pos.C)==1){
        items.section.type.ls[[p]][section, 1] <- freqs.tab[[pos.C]]}
      pos.e = names(freqs.tab)  %>% {which(. == "e")}
      if (length(pos.e)==1){
        items.section.type.ls[[p]][section, 2] <- freqs.tab[[pos.e]]}
      pos.u = names(freqs.tab)  %>% {which(. == "u")}
      if (length(pos.u)==1){
        items.section.type.ls[[p]][section, 3] <- freqs.tab[[pos.u]]}
    }
  }
  mylist <- list(items.section.ls,items.section.type.ls)
  return(mylist)
}


# ================ [3.2] session slices items per section - summary ================ 
session_slices_summary <- function(session, slicing){
#previously as duration.sections.uniq()
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 3
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    # total.sum <- sum(session.list[[p]]$items, na.rm = TRUE)
    total.sum <- length(session.list[[p]]$items)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
        
        # select data 
        #p.data <- session.list[[p]]$items[start.index:end.index]
        p.data <-  session.list[[p]][start.index:end.index, 1] #1:2 "item", and "item_uniq"
        
        # get frequency of "item" in this section 
        #section.freq <- length(unique(p.data))
        
        section.freq <- length(unique(p.data))
        
        # add value to data frame 
        freqs.df[p, section+1] <-  section.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  freqs.ave <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  for (i in 1:sections){
    freqs.ave[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)/participants
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "freqs.ave","session.info")) 
  return(freq.item)
}

# ================ [3.4] session slices an item per section - summary ================ 
session_slices_item_summary <- function(session, item, slicing){
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  } 
  else if (session == "new"){
    session.list <- new.list
  }
  
  #stop if slicing input is wrong
  if (slicing < 1 || slicing > 3){
    stop(sQuote(slicing), "slicing should be between 1 and 2 (minute,  or 10-portions)")
  }
  
  # create data frame to store calculation 
  # data frame columns
  cols.names <-  c("session", 1:4, "total")
  len.cols <- length(cols.names)
  rows.names <- c(1:20)
  
  freqs.df <- data.frame()
  for (col in cols.names){freqs.df[[col]] <- as.numeric()}
  freqs.df[nrow(freqs.df)+participants,] <- NA #add empty NAs
  freqs.df$session[1:20] <- 1:20
  
  #iterate over participants 
  for (p in 1:participants){
    #find the range duration
    start.time <- min(session.list[[p]]$`start`)
    end.time <- max(session.list[[p]]$`end`, n=1)
    range.time <- end.time - start.time
    
    # select slicing 
    if (slicing == 1)
    {
      # slicing by 10 slices 
      # sections <- 10
      sections <- 4
      sections.slices <- range.time/sections
    } else if(slicing ==2){
      # slicing by minute
      sections <- ceiling(range.time/60)
      sections.slices <- 60
    }
    else if(slicing ==3){
      # slicing by half minute
      sections <- ceiling(range.time/30)
      sections.slices <- 30
    }
    
    # get time ranges
    start.time.ranges <- c(start.time + sections.slices*(1:sections-1))
    end.time.ranges <- c(start.time + sections.slices*(2:sections-1), end.time)
    
    #list of start indexes
    start.time.indexes <- c()
    
    for (section in 1:sections){
      start.time.indexes[section] <- which.min(abs(start.time.ranges[section] -  session.list[[p]]$`start`))
    }
    #list of end indexes
    end.time.indexes <- c((start.time.indexes-1)[2:sections], length(session.list[[p]]$`end`))
    
    # count the number of items per section 
    # items.section.ls[[p]] <-  (end.time.indexes - start.time.indexes)+1
    
    # add matrix to list 
    # items.section.type.ls[[p]] <- items.section.type
    
    # get total number of times item was used in the current session 
    total.sum <- sum(session.list[[p]]$items == item, na.rm = TRUE)
    
    # browser()
    # find if the item was used, if not skip the sections loop
    if (total.sum == 0){
      freqs.df[p, 2:6] <- 0
    }
    else if (total.sum >0){
      freqs.df[p, 6] <- total.sum
      
      # get the frequency on each range
      for (section in 1:sections){
        #get index per section
        start.index <- start.time.indexes[section]
        end.index <- (end.time.indexes[section])
        
        # select data 
        p.data <- session.list[[p]]$items[start.index:end.index]
        
        # get frequency of "item" in this section 
        item.freq <- sum(p.data == item, na.rm = TRUE)
        
        # add value to data frame 
        freqs.df[p, section+1] <-  item.freq
      }
    }
  }
  
  # initalize vector to store sum data
  freqs.sum <- numeric(sections)
  
  # vector with info about the current session
  session.info <- c(item, session.list[[1]]$session[1]) 
  
  for (i in 1:sections){
    freqs.sum[i] <- sum(freqs.df[[i+1]], na.rm = TRUE)
  }
  
  freq.item <- mget(c("freqs.df", "freqs.sum", "session.info")) 
  return(freq.item)
}


# ================ [8.0] items concurrent around range================ 
items_around <- function(item, session, range, search){
  
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
    stop(sQuote(session), "session should equal to either \"reg\" or \"new\"")
  }
  
  # select an item of interest (item that would be the focus of the analysis)
  item.interest <- item
  
  ### function starts here 
  
  # initialize variable to count the frequency of item of interest
  item.count <- 0
  
  # initialize variable to count the block 
  block.counter <- 0
  
  # initialize counter
  counter <- 0 
  
  # initialize variable to count the row position
  row.pos <- 0
  
  # create a character and numeric vector to store items names and participants id before and after selected items
  items.before <- character()
  items.after <- character()
  
  id.before <- numeric()
  id.after <- numeric()
  
  id.items.before <- character()
  id.items.after <- character()
  
  # initialize
  counter <- 0 
  
  # create data frame to store calculations time interval
  cols.names <- c("distance", "item.n", "order", "start_time", "end_time", "participant", "session", "interval")
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
      item.indxs <- session.list[[p]]$`items` %>% {which(. == item.interest)}
      
      # get lenght of indexs
      len.indxs <- length(item.indxs)
      
      # add number of item of interest indexes to item count 
      item.count <- item.count + len.indxs
      
      ### check item indexes are not empty and follow TIME interval approach
      if (len.indxs > 0){
        
        # iterate over indexs
        for(index in 1:len.indxs){
          
          # get the position of item of interest
          item.int <- item.indxs[index]
          
          ### change to after
          
          #before range
          if (search == "bef"){
            # item interest
            item.start <- session.list[[p]]$`start`[item.int]
            item.end <- item.start -  range
            
            # get items not in interval before of the concurrent item
            left.items.excl <- which((session.list[[p]]$`end`) <=  item.end)
            right.items.excl <- which((session.list[[p]]$`start`) >  item.start)
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
          }
          
          #after range
          if (search == "aft"){
            # item interest
            item.start <- session.list[[p]]$`end`[item.int]
            item.end <- item.start +  range
            
            # get items not in interval before of the concurrent item
            left.items.excl <- which((session.list[[p]]$`start`) <  item.start) # items left tail 
            right.items.excl <- which((session.list[[p]]$`start`) >  item.end) # items right tail 
            
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
            to.keep <- c(item.int ,to.keep)
          }
          
          #after range
          if (search == "in"){

            # item interest
            item.start <- session.list[[p]]$`start`[item.int] - range
            item.end <- session.list[[p]]$`end`[item.int] + range 
            item.mid <- (item.start + item.end) / 2
            item.dur <- item.end - item.start
            
            # get items in interval 
            right.items.excl <- which((session.list[[p]]$`start`) >=  item.end)
            left.items.excl <- which((session.list[[p]]$`end`) <=  item.start)
            
            # combined excluded items in an object 
            excl.items <- unique(sort(c(right.items.excl, left.items.excl)))
            
            # get row of items to keep
            to.keep <- session.list[[p]]$order[!session.list[[p]]$order %in% excl.items]
            #to.keep <- c(item.int ,to.keep)
          }

          ### remove duplicates from data frame
          items.intv <- session.list[[p]][session.list[[p]]$order %in% to.keep, ]
          
          ### get items distance
          
          # get position of item of interest within interval
          item.int.pos <- which(items.intv$`order` == item.int)
          
          # length items interval
          len.intv <- length(items.intv$order)
          
          # vector with order positions for each item in items.intv
          order.vc <- c(1:len.intv)
          
          # vector with relative position of items with respect to item of interest
          rel.pos.vec <-  order.vc - item.int.pos
          
          # increase block counter by 1
          block.counter <- block.counter + 1
          
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
            items.df$item.n[this.row] <-  items.list$unique  %>% {which(. == this.item.name)}
            
            #add start time
            items.df$start_time[this.row] <- items.intv$`start`[i]
            
            #add end time
            items.df$end_time[this.row] <- items.intv$`end`[i]
            
            #add participant
            items.df$participant[this.row] <- items.intv$`participant`[i]
            
            #add session
            items.df$session[this.row] <- items.intv$`session`[i]
            
            #add block
            items.df$interval[this.row] <- block.counter
            
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

# ================ [8.2] find group in items used in combination ================ 
around_analysis <- function(item, session, range, search){
  
  ### get data of items used in combination 
  
  # call the function
  items.interaction <- items_around(item, session, range, search)
  
  # assign data frame to variable
  items.filter <- items.interaction$items.intv.filter
  
  # get the number of intervals in df
  intervals <- max(items.filter$interval)
  intervals.vector <- unique(items.filter$interval)
  
  # get data frame of items name and number
  items.data <- data.frame(items.list$number, items.list$unique)
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
      this.interval <- items.filter[items.filter$interval == i, ]
      
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
          item.rows <- items.filter[which(items.filter$interval== i & items.filter$item == item.rep), ]
          
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
  
  ### do the analysis 
  
  ### singles
  
  # get table of frequencies 
  item.freqs <- as.data.frame(sort(table(items.filter$item.n), decreasing = TRUE))
  
  # add name to data frame
  single.freqs <- merge(item.freqs, items.data, by.x = "Var1", by.y = "number", all.x = TRUE)
  
  # change colum names
  colnames(single.freqs) <- c("item", "freq", "name")
  
  # sort single.freqs
  single.freqs <- single.freqs[order(-single.freqs$freq), ]  
  
  ### doubles
  
  # create data frame to store combinations from list
  cols.names <- c("interval", "item_1", "item_2", "selected")
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
    pos.int <- select.data$interval %>% {which(. == i)}
    # get items this interval
    items.int <- select.data$item.n[pos.int]
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
  colnames(double.freqs) <- c("item_1", "item_2", "Freq", "name_1", "name_2")
  
  # sort freqs.names data frame
  double.freqs <- double.freqs[order(-double.freqs$Freq), ]  
  
  # assign both data frames freqs to a variable
  freqs <- mget(c("single.freqs", "double.freqs", "item"))
  
  # return data frame
  return(freqs)
}


# recipe/items group analysis

# ================ [9.0] get a random 'start' and 'end' times of an item ================ 
get_instance <- function(session, item, percent){
  # check arguments and either break function or assign data set to variables 
  
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
  
  # get indexes of item of interest
  indexes <- session.list$items  %>% {which(. == item)}
  
  # get length of indxs
  len.indexes <- length(indexes)
  
  # stop if item is not in session.list
  if (len.indexes < 1){
    stop(sQuote(session), "item was not used in this set of/individual cooking session")
  }
  
  # get length of percentage of items
  indexes.percent <- round(len.indexes * (percent/100))
  
  # create data frame to store random indexes
  cols.names <- c("participant", "session", "start", "end")
  items.df <- data.frame()
  for (col in cols.names){items.df[[col]] <- as.numeric()}
  # add rows -PROVISIONAL SOLUTION - find a better way
  items.df[nrow(items.df) + indexes.percent, ] <- NA
  
  # iterate over number of desired instances 
  for (i in 1:indexes.percent){
    # get a random number in the range of len.indxs
    random.index <- runif(1, 1, len.indexes)
    
    # get random index from session list
    instance.item <- reg.new.concat[indexes[random.index],]

    # add data to data frame
    items.df$participant[i] <- instance.item$participant
    items.df$session[i] <- instance.item$session
    items.df$start[i] <- instance.item$start
    items.df$end[i] <- instance.item$end
  }
  # order data frame by a column
  instances.selected <- items.df[order(items.df$participant), ]
  
  return(instances.selected)
}

# ================ [a ] unique items per type and per session ================

unique.items <- function(session){
  #summary table to get unique items and items per participant 
  
  # TO DO
  #remove food from this counting 
  #correlation analysis between CPGs and items used
  # select data 
  if (session == "reg"){
    session.list <- reg.list
    len.session.list <- 1
  }
  else if (session == "new"){
    session.list <- new.list
    len.session.list <- 1
  }
  else if (session == "both"){
    session.list <- list(reg.list, new.list)
    len.session.list <- 2
  }
  else{
    stop(sQuote(session), "session should equal to either \"reg\" (regular) or \"new\" (new) or \"both\" (regular and new)")
  }
  
  # get numbber of cooking sessions (take into acount whether  list is simple or concatenated )
  cooking.sessions <- len.session.list * participants 
  
  # data frame columns
  cols.names <-  c("counting", 1:cooking.sessions)
  len.cols.names <- length(cols.names)
  
  # data frame rows
  uniq.names <- c("part", "c", "e", "u", "sum_uniq", "c_total", "e_total", "u_total", "sum_total")
  len.uniq.names <- length(uniq.names)
  
  # create data frame to store calculations 
  uniq.df <- data.frame()
  for (col in uniq.names){uniq.df[[col]] <- as.numeric()}
  uniq.df[nrow(uniq.df)+cooking.sessions,] <- NA #add empty NAs
  uniq.df[,1] <- 1:cooking.sessions
  
  # get unique items for each participant 
  for (p in 1:participants){
    # select participant data
    p.data <- session.list[[p]]
    
    # do counting for each item type for both unique and raw 
    for (uniq in 2:4){
      # select type
      type <- uniq.names[uniq]
      
      # select a subset of the data
      uniq.data <- p.data[p.data$type == type,]
      
      # count unique selected items
      uniq.items <- unique(uniq.data[c("items")])
      
      #add items counting to data frame
      uniq.df[p, uniq] <- length(uniq.items$items)
    }
    
    # count all unique selected items
    uniq.df[p, 5] <- length(unique(p.data$items))
    
    for (uniq in 6:8){
      # select type
      type <- uniq.names[uniq-4]
      
      # select a subset of the data
      uniq.data <- p.data[p.data$type == type,]
      
      # count unique selected items
      uniq.items <- unique(uniq.data[c("items", "items_uniq")])
      
      #add items counting to data frame
      uniq.df[p, uniq] <- length(uniq.items$items)
    }
    
    # count all unique selected items_uniq
    uniq.df[p, 9] <- length(unique(p.data[c("items", "items_uniq")])$items)
  }
  return(uniq.df)
}



# ================ [b ] summary items category interaction ================ 
summary.analysis <- function(session){
  
  #function variables
  rows.participants <- c("p", "inter", "inter_CPG", "items", "items_CPG", "mean", "sd", "median", "mad", 
                         "range", "min", "max", "duration(m)", "inter_u", "inter_e", "items_u", "items_e",
                         "mean_cpg","mean_u","mean_e","median_cpg","median_u","median_e")
  
  len.stats <- length(rows.participants)
  
  ### create data frame
  summary.df <- data.frame()
  for (row in rows.participants){summary.df[[row]] <- as.numeric()}
  summary.df[nrow(summary.df)+participants,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  summary.df[,1] <- c(1:participants)
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  }
  else if (session == "new"){
    session.list <- new.list
  }
  
  #number of interactions per participant 
  for (p in 1:participants){
    
    # call freq.analysis function and assign to variable  
    freqs.data <- freqs.analysis(session)
    
    # interactions to data frame (df[p,2])
    summary.df[p, 2] <- length(session.list[[p]]$`items`)
    
    # interactions CPGs to data frame (df[p,3])
    summary.df[p, 3] <- length(session.list[[p]]$type[session.list[[p]]$type == "c"])
    
    # items (unique) to data frame (df[p,4])
    summary.df[p, 4] <- length(cbind(sort(freqs.data[p+1][,1])))
    
    # CPGs (unique) to data frame (df[p,5])
    summary.df[p, 5] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "c"]))
    
    # mean to data frame (df[p,6])
    p.data <- na.omit(freqs.data[p+1])
    summary.df[p, 6] <- mean(p.data[[1]])
    
    # sd to data frame (df[p,7])
    summary.df[p, 7] <- sd(p.data[[1]])
    
    # median to data frame (df[p,8])
    summary.df[p, 8] <- median(p.data[[1]])
    
    # median absolute deviation to data frame (df[p,9])
    summary.df[p, 9] <- mad(p.data[[1]])
    
    # range to data frame (df[p,10])
    summary.df[p, 10] <- diff(range(p.data[[1]]))
    
    # min to data frame (df[p,11])
    summary.df[p, 11] <- min(p.data[[1]])
    
    # most.frequent_1,_2,_3 to data frame (df[p,12:14])
    n<-1
    top.freqs <- (sort(unique(p.data[[1]]), decreasing =TRUE))[1:n]
    
    summary.df[p, 12] <- top.freqs
    
    #find indexes frequency values
    for (freq.value in 1:n)
    {
      top.freqs.indxs <- freqs.data[p+1] %>% {which(. == top.freqs[freq.value])}
    }
    
    # duration (seconds) session to data frame (df[p,13])
    summary.df[p, 13] <- round((max(session.list[[p]]$`end`) - min(session.list[[p]]$`start`))/60, 1)
    
    # u interactions to data frame (df[p,14])
    summary.df[p, 14] <- length(session.list[[p]]$type[session.list[[p]]$type == "u"])
    
    # e to data frame (df[p,15])
    summary.df[p, 15] <- length(session.list[[p]]$type[session.list[[p]]$type == "e"])
    
    # u (unique) to data frame (df[p,16])
    summary.df[p, 16] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "u"]))
    
    # CPGs (unique) to data frame (df[p,17])
    summary.df[p, 17] <- length(unique(session.list[[p]]$'items'[session.list[[p]]$type == "e"]))
    
    # CPGs (unique) to data frame (df[p,18])
    cpg.indxs <- session.list[[p]]$type %>% {which(. == "c")}
    
    summary.df[p, 18] <- mean(session.list[[p]]$type)
    
    # get the most frequent item by type category (still to add to table)
    #length p.data 
    len.p.data <- length(p.data[[1]])
    
    #add column to data frame 
    p.data$type <-cbind(1:length(p.data[[1]]))
    
    #add type column to p.data
    for (item in 1:len.p.data){
      pos <- as.numeric(row.names(p.data)[item])
      p.data$type[item] <- items.list$type[pos] 
    }
    
    #get most frequent item by type 
    for (type in 1:len.items.types){
      max.type <- max(p.data[[1]][p.data$type == items.types[type]])
      type.pos <-  items.types[type]
      type.index <- as.numeric(row.names(subset(p.data, p.data$type == type.pos & p.data[[1]] == max.type)))
      type.name <- items.list$unique[type.index] 
      cat("p:", p, type.pos, ": ", type.name, "\n")
    }
  }
  return(summary.df)
}
# ================ [c ] summary items category duration ================ 
summaryDur.analysis <- function(session){
  
  #function variables
  rows.participants <- c("p", "session.d", "items.d", "CPG.d", "utensil.d", "environment.d", "mean.d", "sd.d", "median.d", "mad.d", 
                         "range.d", "CPG.range.d","min.d", "CPG.min.d", "max.d", "CPG.max.d")
  
  len.stats <- length(rows.participants)
  
  ### create data frame
  summary.df <- data.frame()
  for (row in rows.participants){summary.df[[row]] <- as.numeric()}
  summary.df[nrow(summary.df)+participants,] <- NA #add empty NAs
  #add items.names to data frame(df[,1])
  summary.df[,1] <- c(1:participants)
  
  # select data 
  if (session == "reg"){
    session.list <- reg.list
  }
  else if (session == "new"){
    session.list <- new.list
  }
  
  #number of interactions per participant 
  for (p in 1:participants){
    
    # session duration to data frame (df[p,2])
    summary.df[p, 2] <- round((max(session.list[[p]]$`end`) - min(session.list[[p]]$`start`))/60, 1)
    
    # items duration to data frame (df[p,3])
    summary.df[p, 3] <- round((sum(session.list[[p]]$duration))/60,1)
    
    # CPGs duration to data frame (df[p,4])
    CPG.indexs <- session.list[[p]]$type %>% {which(. == "c")}
    summary.df[p, 4] <- round(sum(session.list[[p]]$duration[CPG.indexs])/60,1)
    
    # utensil duration to data frame (df[p,5])
    u.indexs <- session.list[[p]]$type %>% {which(. == "u")}
    summary.df[p, 5] <- round(sum(session.list[[p]]$duration[u.indexs])/60,1)
    
    # environment duration to data frame (df[p,6])
    e.indexs <- session.list[[p]]$type %>% {which(. == "e")}
    summary.df[p, 6] <- round(sum(session.list[[p]]$duration[e.indexs])/60,1)
    
    # mean to data frame (df[p,5])
    summary.df[p, 7] <- mean(session.list[[p]]$duration)
    
    # sd to data frame (df[p,6])
    summary.df[p, 8] <- sd(session.list[[p]]$duration)
    
    # median to data frame (df[p,7])
    summary.df[p,9] <- median(session.list[[p]]$duration)
    
    # median absolute deviation to data frame (df[p,8])
    summary.df[p, 10] <- mad(session.list[[p]]$duration)
    
    # range to data frame (df[p,9])
    summary.df[p, 11] <- diff(range(session.list[[p]]$duration))
    
    # range to data frame (df[p,10])
    summary.df[p, 12] <- diff(range(session.list[[p]]$duration[CPG.indexs]))
    
    # min duration to data frame (df[p,11])
    summary.df[p, 13] <- min(session.list[[p]]$duration)
    
    # min duration to data frame (df[p,12])
    summary.df[p, 14] <- min(session.list[[p]]$duration[CPG.indexs])
    
    # max duration to data frame (df[p,13])
    summary.df[p, 15] <- max(session.list[[p]]$duration)
    
    # max duration to data frame (df[p,14])
    summary.df[p, 16] <- max(session.list[[p]]$duration[CPG.indexs])
  }
  return(summary.df)
}
# ================ [z ] clear the space ================ 
rm(list = ls())

