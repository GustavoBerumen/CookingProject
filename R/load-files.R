# the function load all the excel files in folder files. The files include participant files for all the cooking session the item files that contain information 
# about all the cooking sessions 

# ================ [1.0] load files ================ 

# clear workspace
rm(list = ls()) 

# set path to file source
home_path = rstudioapi::getActiveDocumentContext()$path 

# set working directory
setwd("C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/files")

# set path output
path_output <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs"

# select files in folder
file.list <- list.files(pattern = "p[0-9]{2}.xlsx", full.names = TRUE)
file.items <- list.files(pattern = "items.xlsx", full.names = TRUE)

# initialize empty df
reg.list <- vector("list", length(file.list))
new.list <- vector("list", length(file.list))
# load excel sheets
items.list <- as.data.frame(read_excel(file.items, sheet = "all-items"))
recipe.list <- as.data.frame(read_excel(file.items, sheet = "recipes"))
inventory.list <- as.data.frame(read_excel(file.items, sheet = "inventory"))
participants.list<- as.data.frame(read_excel(file.items, sheet = "participants"))
places.list <- as.data.frame(read_excel(file.items, sheet = "places"))
forms.list <- as.data.frame(read_excel(file.items, sheet = "forms"))
activities.list <- as.data.frame(read_excel(file.items, sheet = "activities"))

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
cols.names <-  c("id", "reg", "new") # names of columns 
time.list <- data.frame()
for (col in cols.names){time.list[[col]] <- as.numeric()}
time.list[nrow(time.list)+ participants,] <- NA #add empty NAs
time.list[, 1] <- c(1:20)

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
    time.list[i, s+1] <- time
  }
}

# round time list
time.list <- round(time.list)

#set sessions 
sessions <- c("all", "reg", "new")
# set len.sessions
len.sessions <- 3

# set len.types
len.types <- 3
# set types
types <- c("c", "u", "e")

# add categories to reg.new.concat
items.list.m <-  items.list[, c(1, 3)]
names(items.list.m)[1] <- "items"
# vlookup like function in t
reg.new.con.cat <- (merge(items.list.m, reg.new.concat, by = 'items'))
reg.new.con.cat <- reg.new.con.cat[order(reg.new.con.cat$p_corrected, reg.new.con.cat$order),]
#re-order columns
#reg.new.con.cat <- reg.new.con.cat[, c(3, 1,4, 2, 5:9, 11:13, 10)]


# add items number to reg.new.concat
items.list$item.number <- c(1:length(items.list$unique))
items.list.n <-  items.list[, c(1,4)]
names(items.list.n)[1] <- "items"
# save category
df.cat <- reg.new.con.cat$category

# vlookup like function in t
# add item.number
reg.new.con.cat$item.number <- items.list.n$item.number[match(reg.new.con.cat$items, items.list.n$items)]

# re-reoder row (ascending and descending)
reg.new.con.cat <- reg.new.con.cat[order(reg.new.con.cat$p_corrected, reg.new.con.cat$order),]
#re-order columns
reg.new.con.cat <- reg.new.con.cat[, c(3, 2, 1,4:5, 14, 6:9, 11:12, 13, 10)]

#return to working directory
setwd(dirname(home_path))