# central organizer of code. It calls functions and perfom data analysis

# clear workspace
rm(list = ls())

# load libraries
source("./R/load-libraries.R")

# load all the data files and assign them to variables
source("./R/load-files.R")

# get items usage in sections
source("./R/session-slices.R")

# get concurrent items used around an item of interest
source("./R/process-items.R")

# get descriptive stats of items
source("./R/stats-items.R")

# get descriptive stats of items
source("./R/video-instances.R")