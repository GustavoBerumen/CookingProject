# number of items - average 

# get unique items 
types <- c("c", "u", "e")

#alias for data set
sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]

# all types by type of session
type.list <- sessions.list

#get average
items.avg <- length(uniq.items[[1]])/ sessions
#get unique items
uniq.items <- unique(type.list[c("items", "items_uniq", "p_corrected")])
#get average
items.avg <- length(uniq.items[[1]])/ sessions 

#return data
cat(types[i], items.avg, "\n")

for (i in 1:3){
  #subset of data frame
  type.list <- sessions.list[sessions.list$type == types[i], ]
  #get unique items
  uniq.items <- unique(type.list[c("items", "items_uniq", "p_corrected")])
  #get average
  items.avg <- length(uniq.items[[1]])/ sessions 
  
  #return data
  cat(types[i], items.avg, "\n")
}

#get number of items per recipe
for (i in 1:participants){
  #subset of data frame
  type.list <- sessions.list[sessions.list$ == types[i], ]
  #get unique items
  uniq.items <- unique(type.list[c("items", "items_uniq", "p_corrected")])
  #get average
  items.avg <- length(uniq.items[[1]])/ sessions 
  
  #return data
  cat(types[i], items.avg, "\n")
}







#get unique items
uniq.items <- unique(reg.new.concat[c("items", "items_uniq", "p_corrected")])

# get total number of sessions
sessions <- participants * 2 

# get average 
items.avg <- length(uniq.items[[1]])/ sessions 




# number of items - per session




# summary data
type <- "new"
sum_both <- items_analysis(type)
sum_c <- sum_both[sum_both$type == 'c', ]
sum_c <- sum_c[order(-sum_c$dur_total), ]


# select data
all_data <- reg.new.concat
all_c <- all_data[all_data$type =='c', ]
reg_c <- all_c[all_c$session == 'reg', ]
new_c <- all_c[all_c$session == 'new', ]

data_list <- list(all_c, reg_c, new_c)
                  
# average
mean(all_c$duration)

descriptive <- function(dataset){
  average <- round(mean(dataset), digits = 1)
  stdev <- round(sd(dataset), digits = 1)
  med <- median(dataset)
  min_r <- min(dataset)
  max_r <- max(dataset)
  
  cat(average, stdev, med, min_r, max_r, "\n")
}

for (i in 1:3){
  descriptive(data_list[[i]]$duration)
}