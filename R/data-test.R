# ================ number of items per session ================ 

number_items <- function() 
{
  ### session x type x participants 
  
  # create data frame to store calculations
  cols.names <- c("session", "c", "u", "e") # names of columns 
  n.list <- data.frame()
  for (col in cols.names){n.list[[col]] <- as.numeric()}
  n.list[nrow(n.list)+ participants,] <- NA #add empty NAs
  
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
      n.list[i, 1] <- i
      
      #get data for each type 
      for (t in 1:len.types){
        
        #subset of data frame
        type.list <- p.list[p.list$type == types[t], ]
        
        #get unique items
        uniq.items <- unique(type.list[c("items", "items_uniq")])
        
        #get number of items
        sum.uniq <- length(uniq.items$items)
        
        #add value
        n.list[i, t+1] <- sum.uniq
      }
    }
    # add df to list 
    number.lists[[sessions[s]]] <- n.list
  }
  
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

  # put data frame together to return 
  n.items.ls <- mget(c("n.total", "number.lists"))
  return(n.items.ls)
}

num.list <- number_items()
test.num <- number_items()

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


