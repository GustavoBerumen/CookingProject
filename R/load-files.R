# the function load all the excel files in folder files. The files include participant files for all the cooking session the item files that contain information 
# about all the cooking sessions 

# ================ [1.0] load files ================ 

# clear workspace
rm(list = ls()) 

# set path to file source
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd('./files')
path_output <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs"

# select files in folder
file.list <- list.files(pattern = "p[0-9]{2}.xlsx", full.names = TRUE)
file.items <- list.files(pattern = "items.xlsx", full.names = TRUE)


# initialize empty df
reg.list <- vector("list", length(file.list))
new.list <- vector("list", length(file.list))
items.list <- as.data.frame(read_excel(file.items, sheet = "all-items"))
recipe.list <- as.data.frame(read_excel(file.items, sheet = "recipes"))
inventory.list <- as.data.frame(read_excel(file.items, sheet = "inventory"))
#time.list <- as.data.frame(read_excel(file.items, sheet = "time"))
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
# add participants corrected (participant + 20 if session equal new)
reg.list.concat$p_corrected <- reg.list.concat$participant + ifelse(reg.list.concat$session == "new", 20, 0)
new.list.concat$p_corrected <- new.list.concat$participant + ifelse(new.list.concat$session == "new", 20, 0)

#create concatenated df of both reg and new
reg.list.concat$session <- "reg" #add session column to df 
new.list.concat$session <- "new"
# merge data frames
reg.new.concat <- rbind(reg.list.concat,  new.list.concat)


#get time list
### create data frame to store calculations
cols.names <-  c("reg", "new") # names of columns 
time.list <- data.frame()
for (col in cols.names){time.list[[col]] <- as.numeric()}
time.list[nrow(time.list)+ participants,] <- NA #add empty NAs

for (s in 1:2){
  # get data sets
  if (s == 1){p.list <- reg.list
  }
  else{p.list <- new.list
  }
  
  # get time for each participants
  for (i in 1:participants){
    #get time 
    start <- sort(p.list[[i]]$start)[1]
    end <- (sort(-p.list[[i]]$end)[1]*-1)
    
    #get time in seconds
    time <- round(((end - start)/60), digits=1)
    
    #add time to data frame 
    time.list[i, s] <- time
  }
}

#return to working directory
setwd(dirname(current_path))