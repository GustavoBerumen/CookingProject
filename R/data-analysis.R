# ================ 0 LOAD LIBRARIES, DATA SET and FUNCTIONS ================
### libraries

library(dplyr)
library(psych)
library(multcomp)
library(car)
library(yarrr)
library(FSA)
library(coin)
library(rcompanion)
library(report)
library(data.table)


### functions

# function to produce summary statistics from a vector 
data_summary <- function(x) {
  m <- mean(x)
  sd <- sd(x)
  minm <- min(x)
  maxm <- max(x)
  
  return(
    data.frame(
          mean = round(m, digits = 2),
          sd =  round(sd, digits = 2),
          min = minm,
          max = maxm))
}

# funciton to return summary statistics from a data frame 
desc_df <- function(df, column){
  df %>% dplyr::group_by(!!rlang::sym(names(.)[column])) %>%
    dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), min = min(total), max = max(total), total = sum(total))
}
  


# function to find relative position
rel_summary <- function(df, seqs){
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- which.min(abs(seqs - df[i]))}
  out
}

# function to get relative position vectors
rel_positions <- function(df){
  out <- vector()
  for (i in seq_along(unique(df$p_corrected))){
    # select participant
    pv <- df %>%
      dplyr::filter(p_corrected == i)
    # get min and max
    s_min <- min(pv$start)
    e_max <- max(pv$end)
    # create vector of dur_seg
    seq_vec <- seq(s_min, e_max, length = 101)[2:101]
    # get rel_postions
    rel_pos <- rel_summary(pv$start, seq_vec)
    # append data to vector
    out <- append(out, rel_pos)}
  out
}

### define class for one way ANOVA
setClass(Class="one-way",
         representation(
           desc.df ="ANY",
           desc.pl ="ANY",
           aov.summ ="ANY",
           aov.tukey ="ANY",
           aov.glh = "ANY",
           aov.pair ="ANY",
           levene ="ANY",
           shapiro ="ANY",
           kruskal.summ ="ANY",
           kruskal.pair ="ANY"))

### one way anova
one_anova <- function(pivot, f1){

  # make column a factor
  pivot[[f1]] <- as.factor(pivot[[f1]])
  # determine factors
  factor1 <- pivot[[f1]] # main factor

  # get length factor1
  len.factor1 <- length(unique(factor1))

  # summary statistics
  desc.df <-
    pivot %>% dplyr::group_by(!!rlang::sym(names(.)[f1])) %>%
    dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

  # visualize data [version a]
  desc.pl <-
    ggboxplot(pivot, x = names(pivot[f1]), y = "total", color = names(pivot[f1]),  add = "jitter") +
    stat_compare_means()

  # one way anova test
  res.aov <- aov(total ~ factor1, data = pivot)
  # summary of the analysis
  aov.summ <-
    summary(res.aov)

  ### multiple pairwise comparisons

  # [a] using Tukey HSD
  aov.tukey <-
    TukeyHSD(res.aov)

  # [b] using General Linear Hypothesis
  if (len.factor1 < 1){
    library("multcomp")
    aov.glh <-
      summary(glht(res.aov, linfct = mcp(factor1 = "Tukey")))
  }

  # [c] using Pairwise t-test
  aov.pair <-
    pairwise.t.test(pivot$total, factor1, p.adjust.method = "BH")

  ### check the normality assumption

  # [1] homogeneity of variances
  plot(res.aov, 1)

  # [2] levene's test
  levene <-
    leveneTest(total ~ factor1, data = pivot)

  # relaxing homogeneity
  # no assumptions of equal variances
  oneway.test(total ~ factor1, data = pivot)

  # pairwise t-tests with no assumption of equal variances
  pairwise.t.test(pivot$total, factor1,
                  p.adjust.method = "BH", pool.sd = FALSE)

  # [2] normality
  plot(res.aov, 2)

  # [2] extract the residuals
  aov_residuals <- residuals(object = res.aov)
  # run Shapiro-Wilk test [p > 0.05 distribution are not significantly different]
  shapiro <-
    shapiro.test(x = aov_residuals)

  ### normality assumptions break

  # non-parametric alternative to one-way ANOVA test
  kruskal.summ <-
    kruskal.test(total ~ factor1, data = pivot)

  # multiple pairwise-comparison between groups
  kruskal.pair <-
    pairwise.wilcox.test(pivot$total, factor1, p.adjust.method = "BH")

  # return
  return(new("one-way",
             desc.df = desc.df,
             desc.pl = desc.pl,
             aov.summ = aov.summ,
             aov.tukey = aov.tukey,
             aov.pair = aov.pair,
             levene = levene,
             shapiro = shapiro,
             kruskal.summ = kruskal.summ,
             kruskal.pair = kruskal.pair
             ))
}


# define class for two way ANOVA
setClass(Class="two-way",
         representation(
           desc.df ="ANY",
           desc.pl ="ANY",
           aov2.summ ="ANY",
           aov3.summ ="ANY",
           aov.tukey ="ANY",
           aov.tukey.int ="ANY",
           aov.glh = "ANY",
           aov.pair ="ANY",
           levene ="ANY",
           shapiro ="ANY"))

### two way anova
two_anova <- function(pivot, f1, f2){

  # make columns a factor
  pivot[[f1]] <- as.factor(pivot[[f1]])
  pivot[[f2]] <- as.factor(pivot[[f2]])

  # determine factors
  factor1 <- pivot[[f1]] # main factor
  factor2 <- pivot[[f2]] #

  # visualize data
  desc.pl <-
    ggboxplot(pivot, x = names(pivot[f1]), y = "total", color = names(pivot[f2]),  add = "jitter") +
    stat_compare_means()

  # two-way anova test
  res.aov2 <- aov(total ~ factor1 + factor2, data = pivot)
  aov2.summ <-
    summary(res.aov2)

  # two-way anova test with interactions
  res.aov3 <- aov(total ~ factor1 * factor2, data = pivot)
  aov3.summ <-
    summary(res.aov3)

  # summary statistics [to be fixed]
  desc.df <-
    pivot %>% dplyr::group_by(start_gr, category) %>%
    dplyr::summarise(count = length(category), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE))

  ### multiple pairwise comparisons

  # [a] using Tukey HSD
  # one factor
  aov.tukey <-
    TukeyHSD(res.aov3, which = "factor1")
  # interactions factors
  aov.tukey.int <-
    TukeyHSD(res.aov3, which = "factor1:factor2")

  # [b] using General Linear Hypothesis
  aov.glh <-
    summary(glht(res.aov2, linfct = mcp(factor1= "Tukey")))

  # [c] using Pairwise t-test
  aov.pair <-
    pairwise.t.test(pivot$total, factor1, p.adjust.method = "BH")

  ### check the normality assumption

  # [1] homogeneity of variances
  plot(res.aov3, 1)

  # [1] levene's test
  levene <-
    leveneTest(total ~ factor1*factor2, data = pivot)

  # [2] normality
  plot(res.aov3, 2)

  # [2] extract the residuals
  aov_residuals <- residuals(object = res.aov3)
  # run Shapiro-Wilk test
  shapiro <-
    shapiro.test(x = aov_residuals)

  ### normality assumptions break
  ### no straigthforward way for two-way anova

  # return
  return(new("two-way",
             desc.df = desc.df,
             desc.pl = desc.pl,
             aov2.summ = aov2.summ,
             aov3.summ = aov3.summ,
             aov.tukey = aov.tukey,
             aov.tukey.int = aov.tukey.int,
             aov.glh =  aov.glh,
             aov.pair = aov.pair,
             levene = levene,
             shapiro = shapiro
             ))
}

# data frame
df <- reg.new.con.cat

# ================ 1 INTERACTIONS ================
# -------~~ sessions --------

# prepare data frame
pivot <- df %>%
  dplyr::select(items, session, p_corrected) %>%
  dplyr::group_by(session, p_corrected) %>%
  dplyr::summarise(total = length(items))

# order data frame
pivot <- pivot[order(pivot$session, decreasing = TRUE), ]
data_summary(pivot$total)

# filter reg
pivotReg <- pivot %>%
  filter(session == "reg")
data_summary(pivotReg$total)

# filter new
pivotNew <- pivot %>%
  filter(session == "new")
data_summary(pivotNew$total)

# check for normality
shapiro.test(pivot$total)
# visual inspection normality
ggqqplot(pivot$total)

# normality was met
t.test(pivotReg$total, pivotNew$total, paired = TRUE, alternative = "two.sided")

# visualize groups 
pivot %>% ggplot(aes(x=session, y=total, fill = session)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Total Interactions by Sessions")


# # normality was not met -> prepare data frame summary
# dplyr::group_by(pivot, session) %>%
#   dplyr::summarise(count = n(),
#                    median = median(total, na.rm = TRUE),
#                    IQR = IQR(total, na.rm = TRUE))

# # wilconson (normality is not meet)
# wilcox.test(pivot$total[pivot$session=="reg"], pivot$total[pivot$session=="new"], paired=T)
# 
# wilcoxsign_test(pivot$total[pivot$session=="reg"] ~ pivot$total[pivot$session=="new"])

# calculate effect size (absolute Z/sqrt(len of both groups))
# 61.5/sqrt(40)

# -------~~ type --------
pivot <- df %>%
  dplyr::group_by(session, participant, type) %>%
  dplyr::summarise(total = length(items), percent = (length(items)/ length(df$items))*100)

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))

# anova function summary 
all.aov <- one_anova(pivot, 3)


# check normatlity for anova
# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~type, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=type)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ type, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$type,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ type,  data = pivot)

# if significant do a dunn's test
dunnTest(total~type, data = pivot)

# set comparisons
my_comparisons <-  list(c("c", "u"),  c("c", "e"), c("u", "e"))

ggboxplot(pivot, x = "type", y = "total",
          color = "type", add = "jitter") +
          stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
          stat_compare_means(label.y = 800) +                     # Add global p-value
          ggtitle("Interactions and Type")

# mean and sd
pivot %>% dplyr::group_by(type) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))


# -------~~ categories --------
# -------~~ categories c --------
# prepare data
pivot <- df %>%
  filter(type == "c") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

# -------~~ categories u--------
# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

# -------~~ categories e--------
# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

# -------~~ categories previous --------

# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Interactions")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE))


cat("------------------ ", "categories", "u")

# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::summarise(total = n())


# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Interactions")


cat("------------------ ", "categories", "e")

# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::summarise(total = n())


# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Interactions")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))


# -------~~ data most items --------
# summary statistics

# prepare data
pivot <- df %>%
  dplyr::select(items, items_uniq, type, category, p_corrected) %>%
  dplyr::group_by(items, category, type, p_corrected) %>%
  dplyr::summarise(total = n())

# data frame summary 
pivot_items <- desc_df(pivot, 1)

# add type to data frame
items.list.m <-  items.list[, 1:2]
names(items.list.m)[1] <- "items"
# vlookup like function in t
pivot_items <- (merge(items.list.m, pivot_items, by = 'items'))

# filter items 
subset.c <- pivot_items  %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))

# filter items 
subset.u <- pivot_items  %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))

# filter items 
subset.e <- pivot_items  %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))

# create data frame
cols.names <- c("item", "type", "min", "max", "mean", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

inter.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
inter.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
inter.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
inter.df$item <- c(1:10)

# re-order columns
inter.df <- inter.df %>%
  dplyr::select(item, everything())

### items average
pivot <- df %>%
  dplyr::group_by(items, type, p_corrected) %>%
  dplyr::summarise(total = n(), p = length(unique(p_corrected))) %>%
  dplyr::summarise(min = min(total), max = max(total), total = sum(total), p = sum(p), avg = total/p)
  dplyr::arrange(desc(total))

# save df to fname.RData
# resave('inter.df', file='fname.RData')

# ================ 2 DURATION ================

cat("------------------ ", "sessions")

# prepare data frame
pivot <- df %>%
  dplyr::group_by(session, participant) %>%
  dplyr::summarise(total = sum(duration))

# check for normality (p should be greater than 0.05)
shapiro.test(pivot$total)

# visual inspection normality
ggqqplot(pivot$total)

# normality was not met -> prepare data frame summary
dplyr::group_by(pivot, session) %>%
  dplyr::summarise(count = n(),
                   median = median(total, na.rm = TRUE),
                   IQR = IQR(total, na.rm = TRUE))

# visualize groups
ggboxplot(pivot, x = "session", y = "total")

wilcox.test(pivot$total[pivot$session=="reg"], pivot$total[pivot$session=="new"], paired=T)

# wilconson (normality is not meet)
wilcoxsign_test(pivot$total[pivot$session=="reg"] ~ pivot$total[pivot$session=="new"])

# calculate effect size (absolute Z/sqrt(len of both groups))
1.60/sqrt(40)

cat("------------------ ", "type")

pivot <- df %>%
  dplyr::select(items, type, session, p_corrected, duration) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::summarise(total = sum(duration), n = length(items))

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))


# anova
aov.df <- aov(duration ~ type, data = pivot)

# summary of the analysis
summary(aov.df)

# check normatlity for anova
# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~type, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=type)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ type, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$type,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ type,  data = pivot)

# if significant do a dunn's test
dunnTest(total~type, data = pivot)

# set comparisons
my_comparisons <-  list(c("c", "u"), c("u", "e"), c("c", "e"))

ggboxplot(pivot, x = "type", y = "total",
          color = "type", add = "jitter") +
  stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 32000) +                     # Add global p-value
  ggtitle("Duration and Type")

# mean and sd
pivot %>% dplyr::group_by(type) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))


cat("------------------ ", "categories", "c")

# prepare data
pivot <- df %>%
  filter(type == "c") %>%
  dplyr::select(items, items_uniq, category, p_corrected, duration)

# anova
aov.df <- aov(duration ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(duration~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=duration, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(duration ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$duration, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(duration ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(duration~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=duration, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Duration")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(duration, na.rm = TRUE), sd = sd(duration, na.rm = TRUE), total = sum(duration))

cat("------------------ ", "categories", "u")

# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::select(items, items_uniq, category, p_corrected, duration)

# anova
aov.df <- aov(duration ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(duration~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=duration, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(duration ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$duration, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(duration ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(duration~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=duration, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Duration")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(duration, na.rm = TRUE), sd = sd(duration, na.rm = TRUE), total = sum(duration))

cat("------------------ ", "categories", "e")

# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::select(items, items_uniq, category, p_corrected, duration)

# anova
aov.df <- aov(duration ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(duration~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=duration, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(duration ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$duration, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(duration ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(duration~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=duration, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Duration")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(duration, na.rm = TRUE), sd = sd(duration, na.rm = TRUE), total = sum(duration))

cat("------------------ ", "data frame most items")

pivot <- df %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = sum(duration), n = length(items)) %>%
  dplyr::arrange(desc(total))

subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))

subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))

subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))

# create data frame
cols.names <- c("c", "ctotal", "u", "utotal", "e", "etotal", "item") # names of columns
dur.df <- data.frame()
for (col in cols.names){dur.df[[col]] <- as.numeric()}
dur.df[nrow(dur.df)+ 10,] <- NA #add empty NAs

dur.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
dur.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
dur.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
dur.df$item <- c(1:10)

# re-order columns
dur.df <- dur.df %>%
  dplyr::select(item, everything())

### items average
pivot <- df %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(min = min(duration), max = max(duration), total = sum(duration), n = length(items), avg = total/n) 

# ================ 3 FREQUENCY grouping ================

cat("------------------ ", "all")

# frequencies items
pivot <- df %>%
  dplyr::group_by(items, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items) %>%
  dplyr::count(items)

# counts items per session
pivot <- df %>%
  dplyr::group_by(items, session, p_corrected) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(p_corrected) %>%
  dplyr::count(p_corrected)

cat("------------------ ", "sessions")

# frequencies items sessions
pivot <- df %>%
  dplyr::group_by(items, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(session, participant) %>%
  dplyr::summarise(total = length(participant))

# check for normality (p should be greater than 0.05)
shapiro.test(pivot$total)

# visual inspection normality [dots should be withing the way range]
ggqqplot(pivot$total)

# compute summary statistics by groups
group_by(pivot, session) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(total, na.rm = TRUE),
    sd = sd(total, na.rm = TRUE))

# prepare data frame summary
dplyr::group_by(pivot, session) %>%
  dplyr::summarise(count = n(),
                   median = median(total, na.rm = TRUE),
                   IQR = IQR(total, na.rm = TRUE))

# plot weight by group and color by group
ggboxplot(pivot, x = "session", y = "total")

# Shapiro-Wilk normality test for reg session [values should be p > 0.05]
with(pivot, shapiro.test(total[session == "reg"]))
# Shapiro-Wilk normality test for new session
with(pivot, shapiro.test(total[session == "new"]))

#  F-test to test for homogeneity in variances [values should be p > 0.05]
res.ftest <- var.test(total ~ session, data = pivot)
res.ftest

# use t-test witch assume equality of the two variances.
res <- t.test(total ~ session, data = pivot, var.equal = TRUE)
res
cat("no significant differences")


cat("------------------ ", "type")

# prepare data
pivot <- df %>%
  dplyr::select(items, type, p_corrected) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::summarise(total = length(p_corrected))

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))

# anova
aov.df <- aov(total ~ type, data = pivot)

# summary of the analysis
summary(aov.df)

# check normatlity for anova
# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~type, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=type)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ type, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$type,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ type,  data = pivot)

# if significant do a dunn's test
dunnTest(total~type, data = pivot)

# set comparisons
my_comparisons <-  list(c("u", "e"), c("c", "e"))

ggboxplot(pivot, x = "type", y = "total",
          color = "type", add = "jitter") +
  stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 40) +                      # Add global p-value
  ggtitle("Grouping Items and Type")

# mean and sd
pivot %>% dplyr::group_by(type) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
cat("significant differences")

cat("------------------ ", "categories", "c")

# prepare data
pivot <- df %>%
  filter(type == "c") %>%
  dplyr::distinct(items, category, p_corrected)  %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::summarise(total = length(category))

# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Item")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "categories", "u")

# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::distinct(items, category, p_corrected)  %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::summarise(total = length(category))

# anova
aov.df <- aov(duration ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Items")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "categories", "e")

# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::distinct(items, category, p_corrected)  %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::summarise(total = length(category))

# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Items")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "data frame most items")

# prepare data
pivot <- df %>%
  dplyr::group_by(items, session, participant, type) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = length(items))

subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))

subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))

subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))

# create data frame
cols.names <- c("c", "ctotal", "u", "utotal", "e", "etotal", "item") # names of columns
top.df <- data.frame()
for (col in cols.names){top.df[[col]] <- as.numeric()}
top.df[nrow(top.df)+ 10,] <- NA #add empty NAs

top.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
top.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
top.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
top.df$item <- c(1:10)

# re-order columns
top.df <- top.df %>%
  dplyr::select(item, everything())

# save df to fname.RData
# resave('top.df', file='fname.RData')

# ================ 3 FREQUENCY unique ================
cat("------------------ ", "all")

# frequencies items
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items) %>%
  dplyr::count(items)

# counts items per session
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, p_corrected) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(p_corrected) %>%
  dplyr::count(p_corrected)

cat("------------------ ", "sessions")

# frequencies items sessions
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(session, participant) %>%
  dplyr::summarise(total = length(participant))

# sort data frame
pivot[order(pivot$session, decreasing = TRUE), ]

# check for normality (p should be greater than 0.05)
shapiro.test(pivot$total)

# visual inspection normality [dots should be withing the way range]
ggqqplot(pivot$total)

# compute summary statistics by groups
group_by(pivot, session) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(total, na.rm = TRUE),
    sd = sd(total, na.rm = TRUE))

# prepare data frame summary
dplyr::group_by(pivot, session) %>%
  dplyr::summarise(count = n(),
                   median = median(total, na.rm = TRUE),
                   IQR = IQR(total, na.rm = TRUE))

# plot weight by group and color by group
ggboxplot(pivot, x = "session", y = "total")

# Shapiro-Wilk normality test for reg session [values should be p > 0.05]
with(pivot, shapiro.test(total[session == "reg"]))
# Shapiro-Wilk normality test for new session
with(pivot, shapiro.test(total[session == "new"]))

#  F-test to test for homogeneity in variances [values should be p > 0.05]
res.ftest <- var.test(total ~ session, data = pivot)
res.ftest

# use t-test witch assume equality of the two variances.
res <- t.test(total ~ session, data = pivot, var.equal = TRUE)
res
cat("no significant differences")

cat("------------------ ", "type")

# prepare data
pivot <- df %>%
  dplyr::select(items, items_uniq, type, p_corrected) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::summarise(total = length(p_corrected))

# re-order type
pivot <- pivot %>%
  dplyr::arrange(factor(type, levels = c("c", "u", "e")))

# anova
aov.df <- aov(total ~ type, data = pivot)

# summary of the analysis
summary(aov.df)

# check normatlity for anova
# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~type, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=type)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ type, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$type,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ type,  data = pivot)

# if significant do a dunn's test
dunnTest(total~type, data = pivot)

# set comparisons
my_comparisons <-  list(c("c", "u"), c("u", "e"), c("c", "e"))

ggboxplot(pivot, x = "type", y = "total",
          color = "type", add = "jitter") +
  stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 100) +                      # Add global p-value
  ggtitle("Unique Items and Type")

# mean and sd
pivot %>% dplyr::group_by(type) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
cat("significant differences")

cat("------------------ ", "categories", "c")

# prepare data
pivot <- df %>%
  filter(type == "c") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::summarise(total = length(p_corrected))

# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Item")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "categories", "u")

# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::summarise(total = length(p_corrected))

# anova
aov.df <- aov(duration ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Items")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "categories", "e")

# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::select(items, items_uniq, category, p_corrected) %>%
  dplyr::group_by(category, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::summarise(total = length(p_corrected))

# anova
aov.df <- aov(total ~ category, data = pivot)

# summary of the analysis
summary(aov.df)

# check normality for anova

# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~category, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=category)) + geom_density(alpha=.3)

# anova assumptions are not meet
kruskal.test(total ~ category, data = pivot)

# find wich pairs are different
pairwise.wilcox.test(pivot$total, pivot$category,
                     p.adjust.method = "BH")
# global test
compare_means(total ~ category,  data = pivot)

# if significant do a dunn's test
dunnTest(total~category, data = pivot)

# plot violin
p <- ggplot(pivot, aes(x=category, y=total, color = category)) +
  geom_violin()

p + stat_summary(fun=mean, geom="point", size=1, color ="red") +
  geom_jitter(position=position_jitter(0.2), size = 0.8) +
  ggtitle("Category and Grouping Items")

# mean and sd
pivot %>% dplyr::group_by(category) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

cat("------------------ ", "data frame most items")

# prepare data
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, participant, type) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = length(items))

subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))

subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))

subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))

# create data frame
cols.names <- c("c", "ctotal", "u", "utotal", "e", "etotal", "item") # names of columns
top.df <- data.frame()
for (col in cols.names){top.df[[col]] <- as.numeric()}
top.df[nrow(top.df)+ 10,] <- NA #add empty NAs

top.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
top.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
top.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
top.df$item <- c(1:10)

# re-order columns
top.df <- top.df %>%
  dplyr::select(item, everything())


### items average
pivot <- df %>%
  dplyr::distinct(items, items_uniq, p_corrected) %>%
  dplyr::group_by(items) %>%
  dplyr::summarise(n = length(unique(p_corrected)),  total = length(items), avg = total/n)
  
# save df to fname.RData
# resave('top.df', file='fname.RData')

# ================ 4 STAGE OF USE ================

# preparation of data [adding relative start position column]
# add relative position column to data frame
dfR <- df
dfR$start_rel <- rel_positions(df)
dfR$start_gr <- as.factor(ceiling(dfR$start_rel/33.34))

cat("------------------ ", "all")

pivot <- dfR %>%
  dplyr::group_by(session, participant, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# sort data frame
pivot[order(pivot$session, decreasing = TRUE), ]

# anova
aov.df <- aov(total ~ start_gr, data = pivot)

# summary of the analysis
summary(aov.df)

# check normatlity for anova
# a. Homogeneity of variances
plot(aov.df, 1)

# b. Levene test (if p less than 0.5 ---> violation of assumption)
leveneTest(total~start_gr, data = pivot)

# c. check for distributions
ggplot(pivot, aes(x=total, fill=start_gr)) + geom_density(alpha=.3)

# anova assumptions are meet
compare_means(total ~ start_gr, data = pivot, method = "anova")

# plot
ggboxplot(pivot, x = "start_gr", y = "total",
          color = "start_gr", add = "jitter") +
  stat_compare_means() +                     # Add global p-value
  ggtitle("start_gr")

# mean and sd
pivot %>% dplyr::group_by(start_gr) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
cat("no significant differences")


cat("------------------ ", "sessions")

# frequencies start_gr sessions
pivot <- dfR %>%
  dplyr::group_by(session, participant, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# sort data frame
pivot <- pivot[order(pivot$session, decreasing = TRUE), ]

# table of frequencies
table(dfR$session, dfR$start_gr)

# two-way anova
res.aov2 <- aov(total ~ session + start_gr, data = pivot)
summary(res.aov2)

# two-way ANOVA with interaction effect
res.aov3 <- aov(total ~ session * start_gr, data = pivot)
summary(res.aov3)

# plot
ggboxplot(pivot, x = "start_gr", y = "total", color = "session", add = "jitter") +
  stat_compare_means() + ggtitle("start_gr")

# mean and sd
pivot %>% dplyr::group_by(session, start_gr) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

# model summary
model.tables(res.aov3, type="means", se = TRUE)

# [a] multiple pairwise comparisons
TukeyHSD(res.aov3, which = "session")
TukeyHSD(res.aov3, which = "start_gr")
# comparing factors
TukeyHSD(res.aov3, which = "session:start_gr")

# [b]
summary(glht(res.aov2, linfct = mcp(start_gr= "Tukey")))
summary(glht(res.aov2, linfct = mcp(session= "Tukey")))

# [c]
pairwise.t.test(pivot$total, pivot$start_gr, p.adjust.method = "BH")

# check the normality assumption

# normality
plot(res.aov3, 2)

# extract the residuals
aov_residuals <- residuals(object = res.aov3)

# run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

cat("no significant differences of interaction")

cat("------------------ ", "type")

# frequencies start_gr sessions

two_anova_v0 <- function(pivot){

  # two-way anova
  res.aov2 <- aov(total ~ type + start_gr, data = pivot)
  summary(res.aov2)

  # two-way ANOVA with interaction effect
  res.aov3 <- aov(total ~ type * start_gr, data = pivot)
  summary(res.aov3)

  # plot
  ggboxplot(pivot, x = "start_gr", y = "type", color = "type", add = "jitter") +
    stat_compare_means() + ggtitle("start_gr")

  # mean and sd
  pivot %>% dplyr::group_by(type, start_gr) %>%
    dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

  # model summary
  model.tables(res.aov3, type="means", se = TRUE)

  # [a] multiple pairwise comparisons
  # TukeyHSD(res.aov3, which = "type")
  TukeyHSD(res.aov3, which = "start_gr")
  # comparing factors
  TukeyHSD(res.aov3, which = "type:start_gr")

  # [b]
  summary(glht(res.aov2, linfct = mcp(start_gr= "Tukey")))
  summary(glht(res.aov2, linfct = mcp(type= "Tukey")))

  # [c]
  pairwise.t.test(pivot$total, pivot$start_gr, p.adjust.method = "BH")

  # check the normality assumption

  # homogeneity of variances
  plot(res.aov3, 1)

  # levene's test
  leveneTest(total ~ type*start_gr, data = pivot)

  # normality
  plot(res.aov3, 2)

  # extract the residuals
  aov_residuals <- residuals(object = res.aov3)
  # run Shapiro-Wilk test
  shapiro.test(x = aov_residuals )

  ### normality assumptions break


  ### anova for unbalanced designs
  aov.ud <- aov(total ~ type * start_gr, data = pivot)
  Anova(aov.ud, type = "III")

  # mean and sd
  pivot %>% dplyr::group_by(type, start_gr) %>%
    dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
}

one_anova_v0 <- function(pivot){

  # anova
  aov.df <- aov(total ~ start_gr, data = pivot)

  # summary of the analysis
  summary(aov.df)

  # anova report
  aov_results <- aov(total ~ start_gr, data = pivot)
  report(aov_results)

  # check normatlity for anova
  # a. Homogeneity of variances
  plot(aov.df, 1)

  # b. Levene test (if p less than 0.5 ---> violation of assumption)
  leveneTest(total~start_gr, data = pivot)

  # c. check for distributions
  ggplot(pivot, aes(x=total, fill=start_gr)) + geom_density(alpha=.3)

  # anova assumptions are meet
  compare_means(total ~ start_gr, data = pivot, method = "anova")

  ### anova assumptions are not meet
  kruskal.test(total ~ start_gr, data = pivot)

  # find wich pairs are different
  pairwise.wilcox.test(pivot$total, pivot$start_gr,
                       p.adjust.method = "BH")
  # global test
  compare_means(total ~ start_gr,  data = pivot)

  # if significant do a dunn's test
  dunnTest(total~start_gr, data = pivot)

  # anova for unbalanced designs
  aov.ud <- aov(len ~ supp * dose, data = pivot)
  Anova(pivot, type = "III")

  # plot
  ggboxplot(pivot, x = "start_gr", y = "total",
            color = "start_gr", add = "jitter") +
    stat_compare_means() +                     # Add global p-value
    ggtitle("start_gr")

  # mean and sd
  pivot %>% dplyr::group_by(start_gr) %>%
    dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))

}

# INTERACTIONS - data preparation to use anova
pivot <- dfR %>%
  # dplyr::filter(type =="c") %>% # comment for two way anova
  dplyr::group_by(type, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))

# GROUPING - items
pivot <- dfR %>%
  dplyr::distinct(items, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# UNIQUE - items
pivot <- dfR %>%
  dplyr::distinct(items, items_uniq, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

cat("no differences by start_gr")


### items average
pivot <- dfR %>%
  dplyr::group_by(items, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))



cat("------------------ ", "categories")
### set factors
f1 <- 3
f2 <- 1

# INTERACTIONS
pivot <- dfR %>%
  dplyr::filter(type =="c") %>% # comment for two way anova
  dplyr::group_by(category, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))


# INTERACTIONS PERCENTAGE
pivot <- dfR %>%
  dplyr::group_by(category, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 0)))

# GROUPING - items
pivot <- dfR %>%
  dplyr::filter(type =="c") %>%
  dplyr::distinct(items, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# UNIQUE - items
pivot <- dfR %>%
  dplyr::filter(type =="c") %>%
  dplyr::distinct(items, items_uniq, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

### anova report [fast report]

# interactions
aov_results <- aov(total ~ pivot[[f1]]*pivot[[f2]], data = pivot)
report(aov_results)

# factor 1
aov_results <- report(aov(total ~ pivot[[f1]], data = pivot))

# factor 2
aov_results <- report(aov(total ~ pivot[[f2]], data = pivot))

cat("------------------ ", "data frame most items")

# prepare data
pivot <- dfR %>%
  dplyr::group_by(category, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))

pivot <- dfR %>%
  dplyr::group_by(category, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(per = (round(n/sum(n)*100, 1)))

pivot <- dfR %>%
  dplyr::filter(items == "oil") %>%
  dplyr::group_by(p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 1)))


report(aov(total ~ pivot[[2]], data = pivot))

subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))

subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))

subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))

# create data frame
cols.names <- c("c", "ctotal", "u", "utotal", "e", "etotal", "item") # names of columns
top.df <- data.frame()
for (col in cols.names){top.df[[col]] <- as.numeric()}
top.df[nrow(top.df)+ 10,] <- NA #add empty NAs

top.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
top.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
top.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
top.df$item <- c(1:10)

# re-order columns
top.df <- top.df %>%
  dplyr::select(item, everything())

# ================ 5 ITEMS SEQUENCE / ITEMS AROUND ================

# bef <- items_around_n("salt", "both", "bef")
# dur <- items_around_n("salt", "both", "in")
# aft <- items_around_n("salt", "both", "aft")
# 
# bef.summ <- around_summary_ouput("both", "bef", 2)
# dur.summ <- around_summary_ouput("both", "in", 2)
# aft.summ <- around_summary_ouput("both", "aft", 2)

# load  data files from RData
bef.summ
in.summ
aft.summ

test <- bef$items.intv

pivot <- test %>%
  dplyr::filter(distance > -3 & distance < 0) %>%
  dplyr::group_by(item_n) %>%
  dplyr::summarise(count = n())

# ================ 5 ITEMS SEQUENCE / ITEMS AROUND [BEFORE] ================

# ================ 6 PLACES ================

### create a matrix of frequencies

# prepare data
df.p <- as.data.frame(table(places.list$item, places.list$place))
names(df.p)[1:3] <- c("item", "place", "count")

# create a matrix
mt.p <- dcast(places.list, item~place)

# get percentage
cols.mt <- length(mt.p)
for (i in seq_along(mt.p$item)){
  mt.p[i, 2:cols.mt] <- round((mt.p[i, 2:cols.mt]/sum(mt.p[i, 2:cols.mt])*100), digits = 0)}

# get type list
types <-  places.list %>%
  dplyr::group_by(type, item) %>%
  dplyr::distinct(type, items)

# add column to data frame
df.places <- cbind(types[1], mt.p)

### summary statistics

### get table of percentages
pivot <-  places.list %>%
  dplyr::distinct(item, place, type) %>%
  dplyr::group_by(type, item) %>%
  dplyr::count(item)

# get means by type
pivot %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(avg = mean(n))

### tranform matrix into data fram
m.places <- melt(df.places, id=(c("item", "type")))
names(m.places)[3:4] <- c("place", "value")

# frequency by place
pivot <- m.places %>%
  dplyr::group_by(place, type) %>%
  dplyr::summarise(avg = mean(value))





# ================ 7 FORMS ================
# prepare data frame
pivot <- forms.list %>%
  dplyr::distinct(type, item, form) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(total= n())

# prepare data frame
pivot <- forms.list %>%
  dplyr::group_by(type, item, form) %>%
  dplyr::summarise(total= n())

# ================ 8 ACTIVITIES ================
# prepare data frame
pivot <- activities.list %>%
  dplyr::group_by(item, activity) %>%
  dplyr::summarise(total= n())

pivot <- activities.list %>%
  dplyr::group_by(item, activity) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 0)))


# ================ [9] SITUATIONS [PROBLEMS AND REMARKABLE] ================
# ================ [8] CONSUMPTION ================
# prepare data frame
pivot <- df %>%
  dplyr::select(items, items_uniq, p_corrected) %>%
  dplyr::group_by(items, items_uniq) %>%
  dplyr::summarise(total = length(items))


# ================ [10] NETWORKS ================
# ================ [11] PEOPLES' OBSERVATIONs ================