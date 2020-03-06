# load libraries and functions

# ================ [1.0]install packages ================ 
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


# ================ [2.0] load libraries ================ 
library(tidyverse)
library(readxl)
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
library(treemap)
library(ArgumentCheck)
library(lubridate)

# ================ [3.0]init functions ================ 
# mode function
getMode <- function(v){
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
