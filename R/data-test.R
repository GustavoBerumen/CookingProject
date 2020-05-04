# ================ number of unique items used per session [resave] ================ 
unique_items_session <- function() 
{
  ### session x type x participants 
  ### unique knife_small and knife_large [counted as two unique items]
  
  # create n list to add n.df
  n.list <- list()
  
  # create data frame to store calculations
  cols.names <- c("session", "c", "u", "e") # names of columns 
  n.df <- data.frame()
  for (col in cols.names){n.df[[col]] <- as.numeric()}
  n.df[nrow(n.df)+ participants,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}
    
    # get data for each participant
    for (i in 1:participants){
      
      #participant data
      p.list <- sessions.list[sessions.list$participant == i, ]
      
      #add session
      n.df[i, 1] <- i
      
      #get data for each type 
      for (t in 1:len.types){
        
        #subset of data frame
        type.list <- p.list[p.list$type == types[t], ]
        
        #get unique items [data that is added] all different items used per session 
        uniq.items <- unique(type.list[c("items", "items_uniq")])
        
        #get number of items
        sum.uniq <- length(uniq.items$items)
        
        #add value
        n.df[i, t+1] <- sum.uniq
      }
    }
    # add df to list 
    n.list[[sessions[s]]] <- n.df
  }
  
  return(n.list)
}

# ================ number of unique items used per session total [resave] ================ 
unique_items_total <- function() 
{
  ### session x type
  n.total <- data.frame()
  cols.names <- c("session", "c", "u", "e", "total") # names of columns 
  for (col in cols.names){n.total[[col]] <- as.numeric()}
  n.total[nrow(n.total)+ 3,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}
    
    # get unique items for the entire sessions
    uniq.all <- unique(sessions.list[c("items", "items_uniq", "p_corrected")])
    
    # add type of session
    n.total[s, 1] <- sessions[s] 
    
    # add all items to n.total
    n.total[s, 5] <- length(uniq.all$items)  
    
    #get data for each type 
    for (t in 1:len.types){
      
      #subset of data frame
      n.type <- sessions.list[sessions.list$type == types[t], ]
      
      #get unique items
      n.items <- unique(n.type[c("items", "items_uniq", "p_corrected")])
      
      #get number of items
      n.sum <- length(n.items$items)
      
      #add value
      n.total[s, t+1] <- n.sum
    }
  }
  return(n.total)
}



# ================ number of different items used per session [resave] ================ 
different_items_session <- function() 
{
  ### session x type x participants 
  ### unique knife_small and knife_large [counted as one different item]
  
  # create n list to add n.df
  n.list <- list()
  
  # create data frame to store calculations
  cols.names <- c("session", "c", "u", "e") # names of columns 
  n.df <- data.frame()
  for (col in cols.names){n.df[[col]] <- as.numeric()}
  n.df[nrow(n.df)+ participants,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}
    
    # get data for each participant
    for (i in 1:participants){
      
      #participant data
      p.list <- sessions.list[sessions.list$participant == i, ]
      
      #add session
      n.df[i, 1] <- i
      
      #get data for each type 
      for (t in 1:len.types){
        
        #subset of data frame
        type.list <- p.list[p.list$type == types[t], ]
        
        #get unique items [data that is added] all different items used per session 
        uniq.items <- unique(type.list[c("items")])
        
        #get number of items
        sum.uniq <- length(uniq.items$items)
        
        #add value
        n.df[i, t+1] <- sum.uniq
      }
    }
    # add df to list 
    n.list[[sessions[s]]] <- n.df
  }
  
  return(n.list)
}

# ================ number of different items used per session total [resave] ================ 
different_items_total <- function() 
{
  ### session x type
  n.total <- data.frame()
  cols.names <- c("session", "c", "u", "e", "total") # names of columns 
  for (col in cols.names){n.total[[col]] <- as.numeric()}
  n.total[nrow(n.total)+ 3,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}
    
    # get unique items for the entire sessions
    uniq.all <- unique(sessions.list[c("items", "p_corrected")])
    
    # add type of session
    n.total[s, 1] <- sessions[s] 
    
    # add all items to n.total
    n.total[s, 5] <- length(uniq.all$items)  
    
    #get data for each type 
    for (t in 1:len.types){
      
      #subset of data frame
      n.type <- sessions.list[sessions.list$type == types[t], ]
      
      #get unique items
      n.items <- unique(n.type[c("items", "p_corrected")])
      
      #get number of items
      n.sum <- length(n.items$items)
      
      #add value
      n.total[s, t+1] <- n.sum
    }
  }
  return(n.total)
}










# ================ duration of use of items per session total [resave] ================ 
duration_items_total <- function() 
{
  ### session x type
  d.total <- data.frame()
  cols.names <- c("session", "c", "u", "e", "total") # names of columns 
  for (col in cols.names){d.total[[col]] <- as.numeric()}
  d.total[nrow(d.total)+ 3,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}

    # add type of session
    d.total[s, 1] <- sessions[s] 
    
    # add all items to d.total
    d.total[s, 5] <- round(sum(sessions.list$duration)/3600, digits=1)
    
    #get data for each type 
    for (t in 1:len.types){
      
      #subset of data frame
      d.type <- sessions.list[sessions.list$type == types[t], ]

      #get number of items
      d.sum <- round(sum(d.type$duration)/3600, digits=1)
      
      #add value
      d.total[s, t+1] <- d.sum
    }
  }
  return(d.total)
}

# ================ average duration of use of items individually [resave] ================ 
duration_items_individual <- function() 
{
  ### session x type
  d.item <- data.frame()
  cols.names <- c("session", "c", "u", "e", "total") # names of columns 
  for (col in cols.names){d.item[[col]] <- as.numeric()}
  d.item[nrow(d.item)+ 3,] <- NA #add empty NAs
  
  #select session
  for (s in 1:len.sessions){
    #all
    if (s == 1){sessions.list <- reg.new.concat
    this.session <- sessions[1]}
    #reg
    else if (s ==2){sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]}
    #new
    else if (s ==3){sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]}
    
    # add type of session
    d.item[s, 1] <- sessions[s] 
    
    # add all items to d.item
    d.item[s, 5] <- round(mean(sessions.list$duration), digits=1)
    
    #get data for each type 
    for (t in 1:len.types){
      
      #subset of data frame
      d.type <- sessions.list[sessions.list$type == types[t], ]
      
      #get number of items
      d.sum <- round(mean(d.type$duration), digits =1)
      
      #add value
      d.item[s, t+1] <- d.sum
    }
  }
  return(d.item)
}




# ================ items per recipe -waffle ================ 
#define output directory
out_dir <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs/"

# define types
types <- c("c", "u", "e")
# define cols
cols <- c("type", "value", "session")
# sessions
sessions <- c("all", "reg", "new")

#select type
for (type in 1:3){
  
  if (type == 1){
    #all
    sessions.list <- reg.new.concat
    this.session <- "all"
  }
  else if (type ==2){
    sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <- "reg"
  }
  else if (type ==3){
    #new
    sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <- "new"
    }
  
  ### create data frame to store calculations
  cols.names <-  cols # names of columns 
  items.list <- data.frame()
  for (col in cols.names){items.list[[col]] <- as.numeric()}
  items.list[nrow(items.list)+ participants*3,] <- NA #add empty NAs

  #start counter
  counter <- 1
  
  # get data for each participant
  for (i in 1:participants){
    
    #participant data
    p.list <- sessions.list[sessions.list$participant == i, ]
    
    #get data for each type 
    for (t in 1:3){
      
      #subset of data frame
      type.list <- p.list[p.list$type == types[t], ]
      
      #get unique items
      uniq.items <- unique(type.list[c("items", "items_uniq")])
      
      # get number of cpgs
      len.uniq <- length(uniq.items[[1]])
      
      #add data to df 
      #add type
      items.list[counter, 1] <- types[t]
      #add value
      items.list[counter, 2] <- len.uniq
      #add session
      items.list[counter, 3] <- i
      
      #update counter
      counter <- counter + 1
    }
  }
  ### grid waffle
  
 
  #get session name
  #this.session <- sessions[type]
  
  #open window
  windows()
  
  # plot
  ggplot(items.list, aes(fill=type, values=value)) +
  geom_waffle(color = "white", size=1, n_rows = 6) +
  facet_wrap(~session, ncol=4) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_tableau(direction = -2) +
  coord_equal() +
  labs(title = "number of items used per recipe",
       subtitle = paste0(this.session, " sessions"), 
       caption = "one square represents one item") +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()
  
  #file name
  # name <- paste0(this.session,".png")
  # f_name <- paste0(out_dir, name)
  # ggsave(f_name, width = 13.6, height = 13.6, limitsize = FALSE)

  
}


# ================ items per recipe -waffle percentage ================ 
#define output directory
out_dir <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs/"

# define types
types <- c("c", "u", "e")
# define cols
cols <- c("type", "value", "session")
# sessions
sessions <- c("all", "reg", "new")

#select type
for (type in 1:3){
  
  if (type == 1){
    #all
    sessions.list <- reg.new.concat
    this.session <- "all"
  }
  else if (type ==2){
    sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <- "reg"
  }
  else if (type ==3){
    #new
    sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <- "new"
  }
  
  ### create data frame to store calculations
  cols.names <-  cols # names of columns 
  items.list <- data.frame()
  for (col in cols.names){items.list[[col]] <- as.numeric()}
  items.list[nrow(items.list)+ participants*3,] <- NA #add empty NAs
  
  #start counter
  counter <- 1
  
  # get data for each participant
  for (i in 1:participants){
    
    #participant data
    p.list <- sessions.list[sessions.list$participant == i, ]
    
    # number of items in this section
    all.uniq<- unique(p.list[c("items", "items_uniq")])
    
    # get number of cpgs
    len.all <- length(all.uniq[[1]])
    
    #set sum counter
    sum <- 20
    
    #get data for each type 
    for (t in 1:3){
      
      #subset of data frame
      type.list <- p.list[p.list$type == types[t], ]
      
      #get unique items
      uniq.items <- unique(type.list[c("items", "items_uniq")])
      
      # get number of cpgs
      len.uniq <- length(uniq.items[[1]])
      
      #add data to df 
      #add type
      items.list[counter, 1] <- types[t]
      
      #add session
      items.list[counter, 3] <- i
      
      #add value
      percentage <- ceiling((len.uniq/len.all)*20)
      
      items.list[counter, 2] <- percentage
      if (t == 3){
        items.list[counter, 2] <- sum
      }
      sum <- sum - percentage
      
      #update counter
      counter <- counter + 1
    }
  }
  ### grid waffle
  
  
  #get session name
  #this.session <- sessions[type]
  
  #open window
  windows()
  
  # plot
  ggplot(items.list, aes(fill=type, values=value)) +
    geom_waffle(color = "white", size=0.75, n_rows = 2) +
    facet_wrap(~session, ncol=4) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_tableau(direction = -2) +
    coord_equal() +
    labs(title = "percentage of items used per recipe",
         subtitle = paste0(this.session, " sessions"), 
         caption = "one square represents 5% of the total number of items used") +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
  
  #file name
  # name <- paste0(this.session,".png")
  # f_name <- paste0(out_dir, name)
  # ggsave(f_name, width = 13.6, height = 13.6, limitsize = FALSE)
}



# ================ items per recipe - donuts ================ 
#define output directory
out_dir <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs/"

# define types
types <- c("c", "u", "e")
# define cols
cols <- c("type", "value", "session")
# sessions
sessions <- c("all", "reg", "new")

#select type
for (type in 1:3){
  
  if (type == 1){
    #all
    sessions.list <- reg.new.concat
    this.session <- "all"
  }
  else if (type ==2){
    sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <- "reg"
  }
  else if (type ==3){
    #new
    sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <- "new"
  }
  
  ### create data frame to store calculations
  cols.names <-  cols # names of columns 
  items.list <- data.frame()
  for (col in cols.names){items.list[[col]] <- as.numeric()}
  items.list[nrow(items.list)+ participants*3,] <- NA #add empty NAs
  
  #start counter
  counter <- 1
  
  # get data for each participant
  for (i in 1:participants){
    
    #participant data
    p.list <- sessions.list[sessions.list$participant == i, ]
    
    # get total number of items 
    
    
    #get data for each type 
    for (t in 1:3){
      
      #subset of data frame
      type.list <- p.list[p.list$type == types[t], ]
      
      #get unique items
      uniq.items <- unique(type.list[c("items", "items_uniq")])
      
      # get number of cpgs
      len.uniq <- length(uniq.items[[1]])
      
      #add data to df 
      #add type
      items.list[counter, 1] <- types[t]
      #add value
      items.list[counter, 2] <- len.uniq
      #add session
      items.list[counter, 3] <- i
      
      #update counter
      counter <- counter + 1
    }
  }
  ### grid waffle
  
  
  #get session name
  #this.session <- sessions[type]
  
  #open window
  windows()
  
  # plot
  ggplot(items.list, aes(fill=type, values=value)) +
    geom_waffle(color = "white", size=1, n_rows = 6) +
    facet_wrap(~session, ncol=4) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    scale_fill_tableau(direction = -2) +
    coord_equal() +
    labs(title = "number of items used per recipe",
         subtitle = paste0(this.session, " sessions"), 
         caption = "one square represents one item") +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
  
  #file name
  # name <- paste0(this.session,".png")
  # f_name <- paste0(out_dir, name)
  # ggsave(f_name, width = 13.6, height = 13.6, limitsize = FALSE)
  
  
}


# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

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
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

# Data: Create some data
# +++++++++++++++++++++++++++++++

df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50))

head(df)


# Basic pie charts
# ++++++++++++++++++++++++++++++++

ggdonutchart(df, "value", label = "group")

# Change color
# ++++++++++++++++++++++++++++++++

# Change fill color by group
# set line color to white
# Use custom color palette
p1 <- ggdonutchart(df, "value", label = "group",
             fill = "group", color = "white",
             palette = c("#00AFBB", "#E7B800", "#FC4E07") )



# ================ duration of sessions ================ 

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

# ================ duration of sessions table ================ 

#set sessions 
sessions<- c("all", "reg", "new")

# list to store df 
d.lists <- list()

### create data frame to store calculations
cols.names <- c("session", "c", "e", "u") # names of columns 
dur.list <- data.frame()
for (col in cols.names){dur.list[[col]] <- as.numeric()}
dur.list[nrow(dur.list)+ participants,] <- NA #add empty NAs

#select type
for (type in 1:3){
  
  if (type == 1){
    #all
    sessions.list <- reg.new.concat
    this.session <- sessions[1]
  }
  else if (type ==2){
    sessions.list <- reg.new.concat[reg.new.concat$session == "reg", ]
    this.session <-  sessions[2]
  }
  else if (type ==3){
    #new
    sessions.list <- reg.new.concat[reg.new.concat$session == "new", ]
    this.session <-  sessions[3]
  }

  # get data for each participant
  for (i in 1:participants){
    
    #participant data
    p.list <- sessions.list[sessions.list$participant == i, ]
    
    #get data for each type 
    for (t in 1:3){
      
      #subset of data frame
      type.list <- p.list[p.list$type == types[t], ]
      
      #get sum of the time for this type of item
      sum.dur <- floor(sum(type.list$duration)/60)
      
      #add data to df 
      dur.list[i, 1] <- i
      
      #add value
      dur.list[i, t+1] <- sum.dur
    }
  }
  print(sessions[type])
  # add df to list 
  d.lists[[sessions[type]]] <- dur.list
}


windows()
s <- 3
df.test <- d.lists[[s]] 
this.session <- sessions[s]

df.test %>%
  gather(key, value, -session) %>% 
  ggplot(aes(x=key, y=value, fill=key)) +
  labs(y="minutes", x="") +
  geom_col(position = "dodge") +
  facet_wrap(~session, ncol=4) +
  scale_fill_tableau(direction = -2) +
  theme_gdocs() +
  theme(legend.title = element_blank()) +
  labs(title = "total duration of items usage per session",
       subtitle = paste0(this.session, " sessions")) +
  theme(plot.background=element_blank())



# ================ plot duration times recipes divergent ================ 

# prepare data frame
time_df <- melt(time.list, id=c("id"))

# change name of columns
names(time_df) <- c("participant", "session", "duration")

# change name to store
time_df1 <- time_df

# save data to rData
resave(time_df1,file='fname.RData')

# change values to negative
time_df$duration[1:20] <- time_df$duration[1:20]*-1

#change name to store
time_df2 <- time_df

# save data to rData
resave(time_df2,file='fname.RData')



# ================ test ================ 

df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny", 
           "Hans", "Leo", "John", "Emily", "Lee"), 
  age = c(48, 47, 40, 28, 29, 29, 27, 27, 31, 30),
  test1_score = c(18.9, 19.5, 19.6, 12.9, 11.1, 7.3, 4.3, 3.9, 2.5, 1.6),
  test2_score = c(9.1, 9.1, 9.2, 11.1, 13.9, 14.5, 19.2, 19.3, 19.1, 18.8),
  stringsAsFactors = FALSE)

library(formattable)
formattable(df, list(
  age = color_tile("white", "orange"),
  test1_score = color_bar("pink"),
  test2_score = color_bar("pink")
))



data <- data.frame(id = 1:10, x = rbinom(10, 100, 0.8))

formattable(data, list(x = formatter("span",
                                     style = x ~ style(
                                       display = "block",
                                       direction = "rtl",
                                       "border-radius" = "4px",
                                       "padding-right" = "2px",
                                       "background-color" = csscolor("lightgray"),
                                       width = "50px",
                                       color = csscolor("transparent")
                                     ))))
# ================ resave/save files ================ 

# participants.list
# source(load.libraries/items.xlsx)
# resave('participants.list',file='fname.RData')

# recipe.list
# source(load.libraries/items.xlsx)
# resave('recipe.list',file='fname.RData')

# inventory.list
# source(load.libraries/items.xlsx)
# resave('inventory.list',file='fname.RData')

# items.lists
# source(load.libraries/items.xlsx)
# resave('items.list',file='fname.RData')

# time.list 
# source(data-test.R/items.xlsx)
# resave('time.list',file='fname.RData')

# unique.items.session
# source(data-test/number of unique items used per session)
# uniq.items.session <- unique_items_session()
# resave(uniq.items.session, file='fname.RData')

# unique.items.total
# source(data-test/number of unique items used per session total)
# uniq.items.total <- unique_items_total()
# resave(uniq.items.total, file='fname.RData')

# different.items.session
# source(data-test/number of different items used per session)
# diff.items.session <- different_items_session()
# resave(diff.items.session, file='fname.RData')

# different.items.total
# source(data-test/number of different items used per session total)
# diff.items.total <- different_items_total()
# resave(diff.items.total, file='fname.RData')

# duration.items.total
# source(data-test/duration of use of items per session total)
# dur.items.total <- duration_items_total()
# resave(dur.items.total, file='fname.RData')

# duration.items.total
# source(data-test/duration of use of items per session total)
# dur.items.total <- duration_items_total()
# resave(dur.items.total, file='fname.RData')

# duration.items.individual
# source(data-test/average duration of use of items individually)
# dur.items.individual <- duration_items_individual()
# resave(dur.items.individual, file='fname.RData')

# #comp.rec.new
 # comp_rec_new <- recipe.list[, c(1,7:8)]
 # diff <- different_items_session()
 # comp_rec_new$'newtime' <- time.list$new
 # comp_rec_new$'newCPGs' <- diff$new$c
 # comp_rec_new$'difftime' <- NA
 # for (i in 1:20){
 #   if (is.na(comp_rec_new$`s. time`[i]) == FALSE){
 #     comp_rec_new$`difftime`[i] <- comp_rec_new$`newtime`[i] - comp_rec_new$`s. time`[i]}
 # }
 # comp_rec_new$'diffCPGs' <- comp_rec_new$`newCPGs` - comp_rec_new$`s. CPGs`
 # # re-order data frame
 # col_order <- c("p", "s. CPGs", "newCPGs", "diffCPGs", "s. time", "newtime", "difftime")
 # comp.rec.new <- comp_rec_new[ ,col_order]
 # resave(comp.rec.new, file='fname.RData')

#reg.new.con.cat
# resave(reg.new.con.cat, file='fname.RData')

# select only c

subset.c <- reg.new.con.cat[which(reg.new.con.cat$type=='c'), ]
subset.u <- reg.new.con.cat[which(reg.new.con.cat$type=='u'), ]
subset.e <- reg.new.con.cat[which(reg.new.con.cat$type=='e'), ]

subset.reg <- reg.new.con.cat[which(reg.new.con.cat$session=='reg'), ]
subset.new <- reg.new.con.cat[which(reg.new.con.cat$session=='new'), ]

library(dplyr)

pivot <- df %>%
  dplyr::select(items, items_uniq, session, p_corrected) %>% 
  filter(session == "reg") %>% 
  dplyr::group_by(p_corrected) %>% 
  dplyr::summarise(unique = n_distinct(items, items_uniq))

pivot <- df %>%
  dplyr::select(items, type, category, p_corrected, type) %>% 
  filter(type == "e") %>% 
  dplyr::group_by(category) %>% 
  dplyr::summarise(unique = n_distinct(items))


# good function
by(df, df$session, summary)


library('funModeling')
# exploratory analysis
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(pivot)

#sd and mean
library(psych)
describe(mydata)


collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                               C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                               C3 = 1:15,
                               C4 = sample(c(0,1), 15, replace = TRUE))

kable(collapse_rows_dt, "latex", booktabs = T, align = "c") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")


# eda

#remove outliers

ggplot(df, aes("var", duration)) +
  geom_violin(outlier.alpha = .50) +
  scale_y_log10(
    breaks = quantile(df$duration)
  )

p1 <- dfc %>% 
  dplyr::count(dfc$category) %>%
  ggplot(aes(x = dfc$category)) +
  geom_col() +
  coord_flip() +
  ggtitle("Total count")

outliers <- outliers::scores(log(df$duration), type = "iqr", lim = 1.5)
stem(df$duration[outliers])

dfc %>% 
  mutate(df$category = fct_lump(category, n = 2)) %>% 
  count(df$category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(df$category, pct), pct)) +
  geom_col() +
  coord_flip()

dfc %>%  
  count(dfc$category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(dfc$category, pct))) +
  geom_point()

ggplot(dfc, aes(y = category, x = p_corrected)) +
  geom_point(alpha = .1)

# compute summary statistics by group

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )