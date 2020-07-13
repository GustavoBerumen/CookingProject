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
library(cowplot)


### functions

# find smaller than 0.05
sigCount <- function(sigVec){
  vc.comp <- sigVec < 0.05
  vc.true <- length(vc.comp[vc.comp == TRUE])
  vc.len <- length(sigVec)
  
  return(c("vc.true: ", vc.true, "vc.len:", vc.len))
}


# copy data frame
cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}

# function add rank
addRank <- function(df, dfRank){
  df <- (merge(df, dfRank, by = 'items'))
  df <- df %>%
    dplyr::filter(rank < 11)
  df <- df[order(df$rank, decreasing = FALSE), ]
  return(df)
}


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

# function to return summary statistics from a data frame 
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
           res.aov = "ANY",
           aov.summ ="ANY",
           aov.tukey ="ANY",
           aov.glh = "ANY",
           aov.pair ="ANY",
           qq.pl ="ANY",
           homog.pl ="ANY",
           levene ="ANY",
           shapiro ="ANY",
           kruskal.summ ="ANY",
           kruskal.pair ="ANY",
           dunn.test = "ANY"))

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
    dplyr::summarise(count = n(), mean = round(mean(total, na.rm = TRUE), digits = 1), sd = round(sd(total, na.rm = TRUE), digits = 1), total = sum(total))

  # visualize data [version a]
  desc.pl <-
    ggboxplot(pivot, x = names(pivot[f1]), y = "total", color = names(pivot[f1]),  add = "jitter") +
    stat_compare_means(method = "anova", label = "p.format")

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

  # [c] using Pairwise t-test{
  try(aov.pair <- pairwise.t.test(pivot$total, pivot[[f1]], p.adjust.method = "BH"))

  ### check the normality assumption
  
  # [0] qqplot
  qq.pl <- ggqqplot(pivot$total)

  # [1] homogeneity of variances
  try(plot(res.aov, 1))
  homog.pl <- recordPlot()

  # [2] levene's test
  levene <-
    leveneTest(total ~ factor1, data = pivot)

  # relaxing homogeneity
  # no assumptions of equal variances
  # oneway.test(total ~ factor1, data = pivot)

  # pairwise t-tests with no assumption of equal variances
  #pairwise.t.test(pivot$total, factor1,
  #                p.adjust.method = "BH", pool.sd = FALSE)

  # [2] normality
  # plot(res.aov, 2)

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

  # report anova
  report(aov(total ~ pivot[[f1]], data = pivot))
  
  # dunn's test
  dunn.test <- dunnTest(total~factor1, data = pivot)
  
  # return
  return(new("one-way",
             desc.df = desc.df,
             desc.pl = desc.pl,
             res.aov = res.aov, 
             aov.summ = aov.summ,
             aov.tukey = aov.tukey,
             aov.pair = aov.pair,
             qq.pl = qq.pl,
             homog.pl = homog.pl,
             levene = levene,
             shapiro = shapiro,
             kruskal.summ = kruskal.summ,
             kruskal.pair = kruskal.pair,
             dunn.test = dunn.test
             ))
}


# define class for two way ANOVA
setClass(Class="two-way",
         representation(
           desc.df ="ANY",
           desc.pl ="ANY",
           res.aov2 ="ANY",
           res.aov3 ="ANY",
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
    stat_compare_means(method = "anova", label = "p.format") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
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
    pivot %>% dplyr::group_by(!!rlang::sym(names(.)[f1]), !!rlang::sym(names(.)[f2])) %>%
    dplyr::summarise(count = length(!!rlang::sym(names(.)[f2])), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE))

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
             res.aov2 = res.aov2, 
             aov2.summ = aov2.summ,
             res.aov3 = res.aov3, 
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
# load files
load(file='fname.RData')

# ================ 1 INTERACTIONS ================
# -------~~ sessions --------

# prepare data frame
pivot <- df %>%
  # dplyr::select(items, session, p_corrected) %>%
  dplyr::select(items, session, participant) %>%
  #dplyr::group_by(session, p_corrected) %>%
  dplyr::filter(participant < 20) %>% # filter participant
  dplyr::group_by(session, participant) %>%
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

# plot 
ggplot(pivot, aes(factor(participant), total,  fill = factor(session, levels=c("reg", "new")))) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Total number of interactions per session") +
  xlab("Participants") + ylab("Interactions per session") + labs(fill = "Sessions")

### correlation
cor(pivotReg$total, pivotNew$total,  method = "pearson", use = "complete.obs")
#
res <- cor.test(pivotReg$total, pivotNew$total, method = "pearson")

# data to plot
pivotPlot <- pivotReg[, 2:3]
names(pivotPlot)[2] <- "reg"
pivotPlot$new <- pivotNew$total

# order data
pivotPlot <- pivotPlot[ order(pivotPlot[,2] ), ]

# plot correlation between two variables
ggscatter(pivotPlot, x = "reg", y = "new", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "interactions regular sessions", ylab = "interactions new sessions") +
          ggtitle("Interactions in Regular Meal vs Interactions in New Meal")

# plot t test 
library(ggplot2)
library(ggsignif)
ggboxplot(pivot, x = "session", y = "total", fill = "session", add = "jitter", xlab = "sessions", ylab = "number of interactions")  +
  stat_compare_means(method = "t.test", label = "p.format", paired = TRUE) +
        ggtitle("Mean of interactions by session") +
        labs(subtitle = "regular and new sessions")

# plot anova 
ggboxplot(pivot, x = "type", y = "total", fill = "type", add = "jitter") +
  stat_compare_means(method = "anova", label = "p.format")



### plot interactions by type 
ggplot(pivot, aes(x = session, y = total, fill = session)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Mean of interaction by type of item") +
  xlab("Participants") + ylab("Proportion of Interactions") + labs(fill = "Type", subtitle = "regular and new sessions")


# -------~~ type --------
# df to plot 
pivot <- df %>%
  # dplyr::group_by(session, participant, type) %>%
  dplyr::group_by(type, p_corrected, session) %>%
  dplyr::summarise(total = length(items)) %>%
  # dplyr::mutate(per = (round(total/sum(total)*100, 0)))
  dplyr::group_by(type, session) %>%
  dplyr::summarise(total = mean(total))
  
# previous df (to do anova)
pivot <- df %>%
  dplyr::group_by(session, participant, type) %>%
  #dplyr::filter(type == "c") %>%
  dplyr::summarise(total = length(items), percent = (length(items)/ length(df$items))*100)

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))

# anova function summary 
all.aov <- one_anova(pivot, 3)

two.aov <- two_anova(pivot, 1, 3)

# t test
t.test(total ~ session, pivot, paired=TRUE)

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

pivotF <- pivot %>%
  dplyr::filter(session == "new")

# plot types per session 
pivotP <- pivot %>%
  dplyr::group_by(session, type) %>%
  dplyr::summarise(tot <- sum(total), mean <- tot/20)


# plot proportions
pivotF <- pivot %>%
  dplyr::filter(session == "new")


n <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) + 
  geom_bar(stat="identity", position = "fill") +
  ggtitle("Proportion of interaction by type of item") +
  xlab("Participants") + ylab("Proportion of Interactions") + labs(fill = "Type", subtitle = "new sessions")

pivotF <- pivot %>%
  dplyr::filter(session == "reg")

r <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) + 
  geom_bar(stat="identity", position = "fill") +
  ggtitle("Proportion of interaction by type of item") +
  xlab("Participants") + ylab("Proportion of Interactions") + labs(fill = "Type", subtitle = "regular sessions")


### plot interactions by type 
ggplot(pivot, aes(x = session, y = total, fill = type)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Mean of interaction by type of item") +
  xlab("Session") + ylab("Number of interactions") + labs(fill = "Type", subtitle = "regular and new sessions")

### plot anova 
pivot$type <- ordered(pivot$type, levels = c("c", "e", "u"))

ggboxplot(pivot, x = "type", y = "total", fill = "session", add = "jitter") +
  # scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF"))  +
  ggtitle("Mean of interaction by type of item and session") +
  xlab("Type") + ylab("Number of interactions") + labs(fill = "Session", subtitle = "regular and new sessions")




# -------~~ categories --------
# -------~~ categories c --------
# prepare data
pivot <- df %>%
  filter(type == "c") %>%
  dplyr::select(items, items_uniq, category, p_corrected, session) %>%
  dplyr::group_by(items, category, p_corrected) %>% 
  # dplyr::filter(session == "new") %>% 
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

# -------~~ categories u--------
# prepare data
pivot <- df %>%
  filter(type == "u") %>%
  dplyr::select(items, items_uniq, category, p_corrected, session) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  # dplyr::filter(session == "new") %>% 
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

test <- all.aov@desc.df
cb(test)

# -------~~ categories e--------
# prepare data
pivot <- df %>%
  filter(type == "e") %>%
  dplyr::select(items, items_uniq, category, p_corrected, session) %>%
  dplyr::group_by(items, category, p_corrected) %>%
  dplyr::filter(session == "new") %>%
  dplyr::summarise(total = n())

# anova summary class
all.aov <- one_anova(pivot, 2)

test <- all.aov@desc.df

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
# sort pivot items
pivot_items <- pivot_items %>%
  dplyr::filter(session == "reg") %>%
  dplyr::arrange(desc(total))

# items c
subset.c <- pivot_items  %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))
subset.c <- subset.c %>%
  dplyr::mutate(per = round(subset.c$total/sum(subset.c$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = round(cumsum(100*per/sum(per)), digits =1)) %>%
  dplyr::mutate(rank = row_number())


# items u
subset.u <- pivot_items  %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))
subset.u <- subset.u %>%
  dplyr::mutate(per = round(subset.u$total/sum(subset.u$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = round(cumsum(100*per/sum(per)), digits =1)) %>%
  dplyr::mutate(rank = row_number())


# items e
subset.e <- pivot_items  %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))
subset.e <- subset.e %>%
  dplyr::mutate(per = round(subset.e$total/sum(subset.e$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = cumsum(100*per/sum(per))) %>%
  dplyr::mutate(rank = row_number())


#filter ranks df
c.ranks <- subset.c[, c(1, 9)]
u.ranks <- subset.u[, c(1, 9)]
e.ranks <- subset.e[, c(1, 9)]

# save all ranks
# all.ranks <- rbind(c.ranks, u.ranks, e.ranks)
# resave(all.ranks, file = "fname.RData")

# create data frame
cols.names <- c("item", "type", "mean", "sd", "min", "max", "mean", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

cols.df <- c(1, 2, 4:9)

inter.df[1:10, 1:8] <- subset.c[1:10, cols.df]
inter.df[11:20, 1:8] <- subset.u[1:10, cols.df]
inter.df[21:30, 1:8] <- subset.e[1:10, cols.df]


### plots 
ggboxplot(subset.c, x = "items", y = "per", fill = "per")

sub.c <- subset.c %>%
  dplyr::filter(per < 10)

r <- ggplot(sub.c, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "CPGs", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

sub.u <- subset.u %>%
  dplyr::filter(per < 10)

n <- ggplot(sub.u, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "utensils", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

plot_grid(r, n, ncol =1)


# -------~~ data most items sessions --------
# prepare data
pivot <- df %>%
  dplyr::group_by(items, category, type, session, p_corrected) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::filter(session == "new") # select session

# data frame summary 
pivot_items <- desc_df(pivot, 1)
# add type to data frame
items.list.m <-  items.list[, 1:2]
names(items.list.m)[1] <- "items"
# vlookup like function in t
pivot_items <- (merge(items.list.m, pivot_items, by = 'items'))
# sort pivot items
pivot_items <- pivot_items %>%
  dplyr::arrange(desc(total))


# items c
subset.c <- pivot_items  %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(desc(total))
subset.c <- subset.c %>%
  dplyr::mutate(per = round(subset.c$total/sum(subset.c$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = round(cumsum(100*per/sum(per)), digits =1)) %>%
  dplyr::mutate(rank = row_number())

# subselect data frame by session
# c.reg <- subset.c
# c.new <- subset.c
# lookup function to add position in other session
c.both <- (merge(c.reg, c.new, by = 'items'))


# items u
subset.u <- pivot_items  %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))
subset.u <- subset.u %>%
  dplyr::mutate(per = round(subset.u$total/sum(subset.u$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = round(cumsum(100*per/sum(per)), digits =1)) %>%
  dplyr::mutate(rank = row_number())

# subselect data frame by session
#u.reg <- subset.u
# u.new <- subset.u
# lookup function to add position in other session
u.both <- (merge(u.reg, u.new, by = 'items'))


# items e
subset.e <- pivot_items  %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))
subset.e <- subset.e %>%
  dplyr::mutate(per = round(subset.e$total/sum(subset.e$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = cumsum(100*per/sum(per))) %>%
  dplyr::mutate(rank = row_number())
# subselect data frame by session
# e.reg <- subset.e
e.new <- subset.e
# lookup function to add position in other session
e.both <- (merge(e.reg, e.new, by = 'items'))





#filter ranks df
c.ranks <- subset.c[, c(1, 9)]
u.ranks <- subset.u[, c(1, 9)]
e.ranks <- subset.e[, c(1, 9)]

# save all ranks
# all.ranks <- rbind(c.ranks, u.ranks, e.ranks)
# resave(all.ranks, file = "fname.RData")

# create data frame
cols.names <- c("item", "type", "mean", "sd", "min", "max", "mean", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

cols.df <- c(1, 2, 4:9)

inter.df[1:10, 1:8] <- subset.c[1:10, cols.df]
inter.df[11:20, 1:8] <- subset.u[1:10, cols.df]
inter.df[21:30, 1:8] <- subset.e[1:10, cols.df]


### plots 
ggboxplot(subset.c, x = "items", y = "per", fill = "per")

sub.c <- subset.c %>%
  dplyr::filter(per < 10)

r <- ggplot(sub.c, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "CPGs", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

sub.u <- subset.u %>%
  dplyr::filter(per < 10)

n <- ggplot(sub.u, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "utensils", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

plot_grid(r, n, ncol =1)



# ================ 2 DURATION ================
# -------~~ sessions --------
# prepare data frame
pivot <- df %>%
  dplyr::group_by(session, participant) %>%
  dplyr::filter(participant < 20) %>% # filter participant
  dplyr::summarise(total = sum(duration)) #%>%
  # dplyr::filter(participant < 20)  # filter participant
  
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

# check for normality (p should be greater than 0.05)
shapiro.test(pivot$total)

# visual inspection normality
ggqqplot(pivot$total)

# visualize groups
ggboxplot(pivot, x = "session", y = "total")

### normality was meet 
t.test(pivotNew$total, pivotReg$total,  paired = TRUE, alternative = "two.sided")

a <- t.test(pivotReg$total, pivotNew$total, paired = TRUE, alternative = "two.sided")

# normality was not met -> prepare data frame summary
dplyr::group_by(pivot, session) %>%
  dplyr::summarise(count = n(), median = median(total, na.rm = TRUE), IQR = IQR(total, na.rm = TRUE))

# test normality not met 
wilcox.test(pivot$total[pivot$session=="reg"], pivot$total[pivot$session=="new"], paired=T)

# wilconson (normality is not meet)
wilcoxsign_test(pivot$total[pivot$session=="reg"] ~ pivot$total[pivot$session=="new"])

# calculate effect size (absolute Z/sqrt(len of both groups))
1.60/sqrt(40)


# plot 
library(ggplot2) #Main package for graph
library(ggthemes)
ggplot(pivot, aes(factor(participant), total, fill = factor(session, levels=c("reg", "new")))) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Total duration of interactions per session") +
  xlab("Participants") + ylab("Duration per session") + labs(fill = "Sessions")

### correlation 
cor(pivotReg$total, pivotNew$total,  method = "pearson", use = "complete.obs")
#
res <- cor.test(pivotReg$total, pivotNew$total, method = "pearson")

# data to plot
pivotPlot <- pivotReg[, 2:3]
names(pivotPlot)[2] <- "reg"
pivotPlot$new <- pivotNew$total

# order data
pivotPlot <- pivotPlot[ order(pivotPlot[,2] ), ]

# plot correlation between two variables
ggscatter(pivotPlot, x = "reg", y = "new", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "duration of interactions regular sessions", ylab = "duration of interactions new sessions") +
  ggtitle("Duration of Interactions in Regular vs New Meal")


# plot t test 
library(ggplot2)
library(ggsignif)
ggboxplot(pivot, x = "session", y = "total", fill = "session", add = "jitter", xlab = "sessions", ylab = "number of interactions")  +
  stat_compare_means(method = "t.test", label = "p.format", paired = TRUE) +
  ggtitle("Mean of interactions by session") +
  labs(subtitle = "regular and new sessions")

### plot differences 
# plot t test 
ggboxplot(pivot, x = "session", y = "total", fill = "session", add = "jitter", xlab = "sessions", ylab = "duration of interactions")  +
  stat_compare_means(method = "t.test", label = "p.format", paired = TRUE) +
  ggtitle("Mean of duration of interactions by session") +
  labs(subtitle = "regular and new sessions")


# -------~~ type --------
# prepare data frame
pivot <- df %>%
  dplyr::select(items, type, session, p_corrected, duration) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::summarise(total = sum(duration), n = length(items))

# new data frame selection
pivot <- df %>%
  # dplyr::select(items, session, p_corrected) %>%
  dplyr::select(items, type, session, participant, duration) %>%
  #dplyr::group_by(session, p_corrected) %>%
  # dplyr::filter(participant < 20) %>% # filter participant
  dplyr::group_by(session, type, participant) %>%
  dplyr::summarise(total = sum(duration), n = length(items))

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "e", "u"))
pivot$session <- ordered(pivot$session, levels = c("reg", "new"))


# all.aov summary
all.aov <- one_anova(pivot, 1)

two.aov <- two_anova(pivot, 1, 3)

# plot proportions
library(ggplot2) #Main package for graph
library(ggthemes)

pivotF <- pivot %>%
  dplyr::filter(session == "new")


n <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) +  
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  ggtitle("Proportion of duration by type of item") +
  xlab("Participants") + ylab("Proportion of duration") + labs(fill = "Type", subtitle = "new sessions") 

pivotF <- pivot %>%
  dplyr::filter(session == "reg")

r <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) +  
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  ggtitle("Proportion of duration by type of item") +
  xlab("Participants") + ylab("Proportion of duration") + labs(fill = "Type", subtitle = "regular sessions") 

plot_grid(r, n, ncol =1)

### plot interactions by type 
ggplot(pivot, aes(x = session, y = total, fill = type)) + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Mean of interaction by type of item") +
  xlab("Session") + ylab("Number of interactions") + labs(fill = "Type", subtitle = "regular and new sessions")


### plot anova 
pivot$type <- ordered(pivot$type, levels = c("c", "e", "u"))

ggboxplot(pivot, x = "type", y = "total", fill = "type", add = "jitter") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF"))  +
  ggtitle("Mean of duration of interactions by type") +
  xlab("Type") + ylab("Duration of interactions") + labs(fill = "Session", subtitle = "regular and new sessions")

### plot anova 
pivot$type <- ordered(pivot$type, levels = c("c", "e", "u"))

ggboxplot(pivot, x = "type", y = "total", fill = "type", add = "jitter") +
  # scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF"))  +
  ggtitle("Mean of duration of interactions by type and session") +
  xlab("Type") + ylab("Duration of interactions") + labs(fill = "Session", subtitle = "regular and new sessions")

ggboxplot(pivot, x = "session", y = "total", fill = "type", add = "jitter") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF"))  +
  ggtitle("Mean of duration of interactions by type and session") +
  xlab("Type") + ylab("Duration of interactions") + labs(fill = "Session", subtitle = "regular and new sessions")


# -------~~ categories --------
# -------~~ categories c --------
# # prepare data
# pivot <- df %>%
#   dplyr::select(items, items_uniq, type, category, p_corrected, duration)
# # rename duration to total
# names(pivot)[5] <- "total"

# prepare data
pivot <- df %>%
  dplyr::group_by(type, category, p_corrected) %>%
  dplyr::summarise(total = sum(duration))

# subset c 
pivot.sub <- pivot %>%
  filter(type == "c")

# anova summary class
all.aov <- one_anova(pivot.sub, 2)

# -------~~ categories u --------
# subset c 
pivot.sub <- pivot %>%
  filter(type == "u")

# anova summary class
all.aov <- one_anova(pivot.sub, 2)

# -------~~ categories e --------
# subset c 
pivot.sub <- pivot %>%
  filter(type == "e")

# anova summary class
all.aov <- one_anova(pivot.sub, 2)

# -------~~ categories previous --------

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

# -------~~ data most items --------
# prepare data
pivot <- df %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = sum(duration), n = length(items), mean = round(mean(duration), digits = 1), sd = round(sd(duration), digits =1), min = min(duration), max = max(duration)) %>%
  dplyr::arrange(desc(total))
pivot$ranks <- all.ranks$rank[match(pivot$items, all.ranks$items)]

# filter items 
subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(ranks)

# filter items 
subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(ranks)

# filter items 
subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(ranks)

# create data frame
cols.names <- c("item", "type", "mean", "sd", "min", "max", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

# set cols data frame
cols.df <- c(1,2,5:8,3)

inter.df[1:10, c(1:7)] <- subset.c[1:10, cols.df]
inter.df[11:20, c(1:7)] <- subset.u[1:10, cols.df]
inter.df[21:30, c(1:7)] <- subset.e[1:10, cols.df]

cat("------------------ ", "data frame most items")

pivot <- df %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = sum(duration), n = length(items), mean = mean(duration), sd = sd(duration)) %>%
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
cols.names <- c("item", "type", "mean", "sd", "min", "max", "mean", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

cols.df <- c(1, 2, 4:9)

inter.df[1:10, 1:8] <- subset.c[1:10, cols.df]
inter.df[11:20, 1:8] <- subset.u[1:10, cols.df]
inter.df[21:30, 1:8] <- subset.e[1:10, cols.df]



# re-order columns
dur.df <- dur.df %>%
  dplyr::select(item, everything())

### items average
pivot <- df %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(min = min(duration), max = max(duration), total = sum(duration), n = length(items), avg = total/n) 

# -------~~ data most items sessions --------

# prepare data
# pivot <- df %>%
#   dplyr::group_by(items, type, participant, session) %>%  
#   #dplyr::filter(session == "new") %>% # select session
#   dplyr::summarise(n = length(items), mean = round(mean(duration), digits = 1), sd = round(sd(duration), digits =1), min = min(duration), max = max(duration), total = sum(duration)) %>%
#   dplyr::arrange(desc(total))  
# pivot$ranks <- all.ranks$rank[match(pivot$items, all.ranks$items)]

# prepare data version two
pivot_items <- df %>%
  dplyr::group_by(items, type) %>%  
  # dplyr::filter(session == "reg") %>% # select session
  #dplyr::filter(session == "new") %>% # select session
  dplyr::summarise(total = sum(duration), count = length(unique(p_corrected)), n = length (items), 
                  mean = round(total/n, digits = 1), sd = round(sd(duration), 
                  digits =1), min = min(duration), max = max(duration)) %>%
  dplyr::arrange(desc(total))

# select items 
i <- "c"

# items c
subset.c <- pivot_items  %>%
  dplyr::filter(type == i) %>%
  dplyr::arrange(desc(total))
subset.c$per <- round(subset.c$total/sum(subset.c$total)*100, digits = 2)
subset.c$cum <- round(cumsum(100*subset.c$per/sum(subset.c$per)), digits =1)
subset.c$rank <- c(1:length(subset.c$total))


# subselect data frame by session
# c.reg <- subset.c
# c.new <- subset.c
# lookup function to add position in other session
c.both <- (merge(c.reg, c.new, by = 'items'))



# items u
subset.u <- pivot_items  %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(desc(total))
subset.u <- subset.u %>%
  dplyr::mutate(per = round(subset.u$total/sum(subset.u$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = round(cumsum(100*per/sum(per)), digits =1)) %>%
  dplyr::mutate(rank = row_number())

# subselect data frame by session
#u.reg <- subset.u
# u.new <- subset.u
# lookup function to add position in other session
u.both <- (merge(u.reg, u.new, by = 'items'))


# items e
subset.e <- pivot_items  %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(desc(total))
subset.e <- subset.e %>%
  dplyr::mutate(per = round(subset.e$total/sum(subset.e$total)*100, digits = 2)) %>%
  dplyr::mutate(cum = cumsum(100*per/sum(per))) %>%
  dplyr::mutate(rank = row_number())
# subselect data frame by session
# e.reg <- subset.e
e.new <- subset.e
# lookup function to add position in other session
e.both <- (merge(e.reg, e.new, by = 'items'))





#filter ranks df
c.ranks <- subset.c[, c(1, 9)]
u.ranks <- subset.u[, c(1, 9)]
e.ranks <- subset.e[, c(1, 9)]

# save all ranks
# all.ranks <- rbind(c.ranks, u.ranks, e.ranks)
# resave(all.ranks, file = "fname.RData")

# create data frame
cols.names <- c("item", "type", "mean", "sd", "min", "max", "mean", "total") # names of columns
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

cols.df <- c(1, 2, 4:9)

inter.df[1:10, 1:8] <- subset.c[1:10, cols.df]
inter.df[11:20, 1:8] <- subset.u[1:10, cols.df]
inter.df[21:30, 1:8] <- subset.e[1:10, cols.df]


### plots 
ggboxplot(subset.c, x = "items", y = "per", fill = "per")

sub.c <- subset.c %>%
  dplyr::filter(per < 10)

r <- ggplot(sub.c, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "CPGs", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

sub.u <- subset.u %>%
  dplyr::filter(per < 10)

n <- ggplot(sub.u, aes(x=rank, y=per)) + 
  geom_point(color="black") + 
  ggtitle("Percentage of the Items' Interactions by Type") +
  xlab("items") + ylab("percentage of interactions") + labs(subtitle = "utensils", caption = "Items with a percentage equal to or greater than ten are not displayed") + 
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5)

plot_grid(r, n, ncol =1)




# ================ 3 LISTING: GROUPING and UNIQUE ================
# -------~~ all --------

### grouping

# items presence in sessions
pivot <- df %>%
  dplyr::group_by(items, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items) %>%
  dplyr::count(items)

### unique

# number of items per session
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::group_by(p_corrected) %>%
  dplyr::count(p_corrected)

# -------~~ sessions --------

### grouping
# frequencies items sessions
pivot <- df %>%
  dplyr::group_by(items, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(session, participant) %>%
  dplyr::summarise(total = length(participant))

# order data frame
pivot <- pivot[order(pivot$session, decreasing = TRUE), ]
data_summary(pivot$total)

### unique
# frequencies items sessions
pivot <- df %>%
  dplyr::group_by(items, items_uniq, type, session, participant) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::group_by(session, participant) %>%
  dplyr::summarise(total = length(participant))

# check for normality (p should be greater than 0.05)
shapiro.test(pivot$total)

# visual inspection normality [dots should be withing the way range]
ggqqplot(pivot$total)

# normality was met 

# normality was met
model.t<- t.test(total ~ session, pivot, paired=TRUE)
report(model.t)

# visualize groups 
pivot %>% ggplot(aes(x=session, y=total, fill = session)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Total Items by Sessions")

# plot 
ggplot(pivot, aes(factor(participant), total,  fill = factor(session, levels=c("reg", "new")))) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Total number of items involved per session") +
  xlab("Participants") + ylab("Items involved per session session") + labs(fill = "Sessions", subtitle = "distinct")


t.summary <- pivot %>%                          # Specify data frame
  group_by(session) %>%                         # Specify group indicator
  summarise_at(vars(total),                     # Specify column
  list(mean = mean, sd = sd))                   # Specify function


# -------~~ type --------

### grouping
pivot <- df %>%
  dplyr::select(items, type, p_corrected) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::summarise(total = length(p_corrected))


### grouping type
pivot <- df %>%
  dplyr::select(items, type, session, participant) %>%
  dplyr::group_by(type, session, participant) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(type, session, participant) %>%
  dplyr::summarise(total = length(participant))


### unique
pivot <- df %>%
  dplyr::select(items, items_uniq, type, p_corrected) %>%
  dplyr::group_by(type, p_corrected) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::summarise(total = length(p_corrected))

### unique type 
pivot <- df %>%
  dplyr::select(items, items_uniq, type, session, participant) %>%
  dplyr::group_by(type, session, participant) %>%
  dplyr::distinct(items, items_uniq) %>%
  dplyr::group_by(type, session, participant) %>%
  dplyr::summarise(total = length(participant))

# re-order type
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))

all.aov<- one_anova(pivot, 1)

# plot proportions
library(ggplot2) #Main package for graph
library(ggthemes)

pivotF <- pivot %>%
  dplyr::filter(session == "new")

n <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) +  
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  ggtitle("Listin the Items Involved in each Session (distinct)") +
  xlab("Participants") + ylab("Proportion of Items Involved") + labs(fill = "Type", subtitle = "new sessions") 

pivotF <- pivot %>%
  dplyr::filter(session == "reg")

r <- ggplot(pivotF, aes(x = factor(participant), y = total,  fill = type)) +  
  geom_bar(stat="identity", position = "fill") +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  ggtitle("Listin the Items Involved in each Session (distinct)") +
  xlab("Participants") + ylab("Proportion of duration") + labs(fill = "Type", subtitle = "regular sessions") 

plot_grid(r, n, ncol =1)





# # anova
# aov.df <- aov(total ~ type, data = pivot)
# 
# # summary of the analysis
# summary(aov.df)
# 
# # check normatlity for anova
# # a. Homogeneity of variances
# plot(aov.df, 1)
## 
# # b. Levene test (if p less than 0.5 ---> violation of assumption)
# leveneTest(total~type, data = pivot)
# 
# # c. check for distributions
# ggplot(pivot, aes(x=total, fill=type)) + geom_density(alpha=.3)
# 
# # anova assumptions are not meet
# kruskal.test(total ~ type, data = pivot)
# 
# # find wich pairs are different
# pairwise.wilcox.test(pivot$total, pivot$type,
#                      p.adjust.method = "BH")
# # global test
# compare_means(total ~ type,  data = pivot)
# 
# # if significant do a dunn's test
# dunnTest(total~type, data = pivot)
# 
# # set comparisons
# my_comparisons <-  list(c("u", "e"), c("c", "e"))
# 
# ggboxplot(pivot, x = "type", y = "total",
#           color = "type", add = "jitter") +
#   stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 40) +                      # Add global p-value
#   ggtitle("Grouping Items and Type")
# 
# # mean and sd
# pivot %>% dplyr::group_by(type) %>%
#   dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
# cat("significant differences")

# -------~~ categories --------
# -------~~ categories c --------
cat("------------------ ", "categories", "c")

### grouping
pivot <- df %>%
  dplyr::distinct(items, category, p_corrected, type)  %>%
  dplyr::group_by(category, p_corrected, type) %>%
  dplyr::summarise(total = length(category))

### unique
pivot <- df %>%
  dplyr::distinct(items, items_unique, category, p_corrected, type)  %>%
  dplyr::group_by(category, p_corrected, type) %>%
  dplyr::summarise(total = length(category))


### remove items with only with ocurrence
pivot.rem <- pivot %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(count = n())

# get name of category to remove
to.rem <- pivot.rem %>%
  dplyr::filter(count == 1)

#remove item from data frame
pivot <- pivot %>% 
  dplyr::filter(!grepl(to.rem$category, category))

# subset c 
pivot.sub <- pivot %>%
  filter(type == "c")

# anova summary class
all.aov <- one_anova(pivot.sub, 1)

# -------~~ categories u --------
# subset u 
pivot.sub <- pivot %>%
  filter(type == "u")

# anova summary class
all.aov <- one_anova(pivot.sub, 1)

# count significants
sigCount(all.aov@dunn.test$res$P.adj)

# -------~~ categories e --------

# subset c 
pivot.sub <- pivot %>%
  filter(type == "e")

# anova summary class
all.aov <- one_anova(pivot.sub, 1)

# count significants
sigCount(all.aov@dunn.test$res$P.adj)

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

# -------~~ data most items --------

### grouping
pivot <- df %>%
  dplyr::group_by(items, session, p_corrected, type) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(total = length(items), per = round(total/40*100, digits =1))
# add rank 
pivot$ranks <- all.ranks$rank[match(pivot$items, all.ranks$items)]

### unique
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, p_corrected, type) %>%
  dplyr::distinct(items) %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(unique = length(items), total = n_distinct(p_corrected), mean = unique/total, min = min(items))

### unique
pivot <- df %>%
  dplyr::group_by(items, items_uniq, session, p_corrected, type) %>%
  dplyr::group_by(items, type, p_corrected) %>%
  dplyr::summarise(totalTemp = n_distinct(items_uniq)) %>%
  dplyr::group_by(items, type) %>%
  dplyr::summarise(unique = sum(totalTemp), total = n_distinct(p_corrected), mean = round(unique/total, digits = 1), min = min(totalTemp), max = max(totalTemp))
# add rank 
pivot$ranks <- all.ranks$rank[match(pivot$items, all.ranks$items)]

# filter items 
subset.c <- pivot %>%
  dplyr::filter(type == "c") %>%
  dplyr::arrange(ranks)

# filter items 
subset.u <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(ranks)

# filter items 
subset.e <- pivot %>%
  dplyr::filter(type == "e") %>%
  dplyr::arrange(ranks)

### grouping
# create data frame
inter.df <- data.frame()
cols.names <- c("item", "type", "sessions", "percentage") # names of columnsinter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

# set cols data frame
cols.df <- c(1:4)

inter.df[1:10, c(1:4)] <- subset.c[1:10, cols.df]
inter.df[11:20, c(1:4)] <- subset.u[1:10, cols.df]
inter.df[21:30, c(1:4)] <- subset.e[1:10, cols.df]


### distinct
# create data frame
inter.df <- data.frame()
cols.names <- c("item", "type", "unique",  "sessions", "mean") # names of columnsinter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 30,] <- NA #add empty NAs

# set cols data frame
cols.df <- c(1:5)

inter.df[1:10, c(1:5)] <- subset.c[1:10, cols.df]
inter.df[11:20, c(1:5)] <- subset.u[1:10, cols.df]
inter.df[21:30, c(1:5)] <- subset.e[1:10, cols.df]


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
# -------~~ data preparation --------

# preparation of data. Add relative position column to data frame
dfR <- df
dfR$start_rel <- rel_positions(df)
dfR$start_gr <- as.factor(ceiling(dfR$start_rel/33.34))


# -------~~ all --------

pivot <- dfR %>%
  dplyr::group_by(session, participant, start_gr) %>%
  dplyr::summarise(total = length(start_gr)) 
names(pivot)[3] <- "sections"
  

# sort data frame
pivot[order(pivot$session, decreasing = TRUE), ]

# all anova
all.aov <- one_anova(pivot, 3)


# -------~~ sessions --------

# frequencies start_gr sessions
pivot <- dfR %>%
  dplyr::group_by(session, participant, start_gr) %>%
  dplyr::summarise(total = length(start_gr))
names(pivot)[3] <- "sections"


# sort data frame
pivot <- pivot[order(pivot$session, decreasing = TRUE), ]

# table of frequencies
table(dfR$session, dfR$start_gr)

# all anova two way
two.aov <- two_anova(pivot, 1, 3)


# # two-way anova
# res.aov2 <- aov(total ~ session + start_gr, data = pivot)
# summary(res.aov2)
# 
# # two-way ANOVA with interaction effect
# res.aov3 <- aov(total ~ session * start_gr, data = pivot)
# summary(res.aov3)
# 
# # plot
# ggboxplot(pivot, x = "start_gr", y = "total", color = "session", add = "jitter") +
#   stat_compare_means() + ggtitle("start_gr")
# 
# # mean and sd
# pivot %>% dplyr::group_by(session, start_gr) %>%
#   dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))
# 
# # model summary
# model.tables(res.aov3, type="means", se = TRUE)
# 
# # [a] multiple pairwise comparisons
# TukeyHSD(res.aov3, which = "session")
# TukeyHSD(res.aov3, which = "start_gr")
# # comparing factors
# TukeyHSD(res.aov3, which = "session:start_gr")
# 
# # [b]
# summary(glht(res.aov2, linfct = mcp(start_gr= "Tukey")))
# summary(glht(res.aov2, linfct = mcp(session= "Tukey")))
# 
# # [c]
# pairwise.t.test(pivot$total, pivot$start_gr, p.adjust.method = "BH")
# 
# # check the normality assumption
# 
# # normality
# plot(res.aov3, 2)
# 
# # extract the residuals
# aov_residuals <- residuals(object = res.aov3)
# 
# # run Shapiro-Wilk test
# shapiro.test(x = aov_residuals )

# -------~~ type --------

# INTERACTIONS
pivot <- dfR %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))
names(pivot)[3] <- "sections"


# GROUPING
pivot <- dfR %>%
  dplyr::distinct(items, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# UNIQUE
pivot <- dfR %>%
  dplyr::distinct(items, items_uniq, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# ITEMS AVERAGE
pivot <- dfR %>%
  dplyr::group_by(items, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))

# -------~~~~ type anovas --------

# all anova two way
two.aov <- two_anova(pivot, 1, 3)

# all anova
all.aov <- one_anova(pivot, 1)
# all anova
all.aov <- one_anova(pivot, 3)

# -------~~ categories --------
# INTERACTIONS
pivot <- dfR %>%
  dplyr::filter(type =="u") %>% # comment for two way anova
  dplyr::group_by(category, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))

# INTERACTIONS PERCENTAGE
pivot <- dfR %>%
  dplyr::group_by(category, p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 0)))

# GROUPING
pivot <- dfR %>%
  dplyr::filter(type =="c") %>%
  dplyr::distinct(items, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# UNIQUE
pivot <- dfR %>%
  dplyr::filter(type =="c") %>%
  dplyr::distinct(items, items_uniq, type, p_corrected, start_gr) %>%
  dplyr::group_by(type, p_corrected, start_gr) %>%
  dplyr::summarise(total = length(start_gr))

# -------~~ categories anovas --------
names(pivot)[3] <- "sections"

# all anova two way
two.aov <- two_anova(pivot, 1, 3)


# -------~~ data most items --------

# INTERACTIONS - ITEMS
pivot <- dfR %>%
  dplyr::group_by(items, items_uniq, p_corrected, category, type, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr)) %>%
  dplyr::group_by(items, type, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = sum(total))
  
# INTERACTIONS - CATEGORIES
pivot <- dfR %>%
  dplyr::group_by(category, type, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = length(start_gr))

# INTERACTIONS PERCENTAGE - ITEMS
pivot <- dfR %>%
  dplyr::group_by(items, type, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(per = (round(total/sum(total)*100, 1)))

# INTERACTIONS PERCENTAGE - CATEGORIES
pivot <- dfR %>%
  dplyr::group_by(category, type, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(per = (round(n/sum(n)*100, 1)))

# ITEM
pivot <- dfR %>%
  dplyr::filter(items == "oil") %>%
  dplyr::group_by(p_corrected, start_gr) %>%  # remove type for two way anova
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 1)))

# subset data
subset.c <- pivot %>%
  dplyr::filter(type == "c")
# add ranks to subset.c from interactions 
subset.c <- addRank(subset.c, c.ranks)

subset.u <- pivot %>%
  dplyr::filter(type == "u")
# add ranks to subset.e from interactions 
subset.u <- addRank(subset.u, u.ranks)

subset.e <- pivot %>%
  dplyr::filter(type == "e")
# add ranks to subset.c
subset.e <- addRank(subset.e, e.ranks)

# create data frame
cols.names <- c("item", "type", "section", "total", "percentage")
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 90,] <- NA #add empty NAs
# set cols data frame
cols.df <- c(1, 2, 3, 4, 5)

inter.df[1:30, c(1:5)] <- subset.c[1:30, cols.df]
inter.df[31:60, c(1:5)] <- subset.u[1:30, cols.df]
inter.df[61:90, c(1:5)] <- subset.e[1:30, cols.df]

### change data frame to fit printing 
inter.df1 <- inter.df %>%
  dplyr::filter(section == 1)

inter.df2 <- inter.df %>%
  dplyr::filter(section == 2)

inter.df3 <- inter.df %>%
  dplyr::filter(section == 3)

inter.all <-  cbind(inter.df1[c(1, 3:5)], inter.df2[3:5], inter.df3[3:5])

# ================ 5 ITEMS SEQUENCE ================
# -------~~ data preparation --------

# load files
load(file='fname.RData')
bef <- bef.summ
conc <- in.summ
aft <- aft.summ

# change name df
names(bef)[1] <- "items"
names(conc)[1] <- "items"
names(aft)[1] <- "items"

# add ranks to data frames
bef <- (merge(all.ranks, bef, by = 'items'))
conc <- (merge(all.ranks, conc, by = 'items'))
aft <- (merge(all.ranks, aft, by = 'items'))

# add type and category to data frames
items.m <- items.list
names(items.m)[1] <- "items"
# vlookup like function in t
bef <- (merge(items.m, bef, by = 'items'))
conc <- (merge(items.m, conc, by = 'items'))
aft <- (merge(items.m, aft, by = 'items'))

# resave(items.m, file = "fname.RData")

### prepare data frames
prepare_df<- function(df){
  
  df %>% inner_join(items.m)
  df$set1C <- items.m$category[match(df$set1, items.m$items)]
  df$set2C <- items.m$category[match(df$set2, items.m$items)]
  df$set3C <- items.m$category[match(df$set3, items.m$items)]
  df$set4C <- items.m$category[match(df$set4, items.m$items)]
  
  df$set1t <- items.m$type[match(df$set1, items.m$items)]
  df$set2t <- items.m$type[match(df$set2, items.m$items)]
  df$set3t <- items.m$type[match(df$set3, items.m$items)]
  df$set4t <- items.m$type[match(df$set4, items.m$items)]
  
  return(df)
}

bef.cat <- prepare_df(bef)
aft.cat <- prepare_df(aft)
conc.cat <- prepare_df(conc)

# -------~~ summary statistics bef --------

# subset data frame [select time]
seq.df <- seq.df <- conc.cat[, c(1:8, 11:16)]
bef.cat[, c(1:8, 11:16)]
seq.df <- aft.cat[, c(1:8, 11:16)]

### summary type
pivot <- seq.df %>%
  dplyr::group_by(type) %>%
  dplyr::count(set1t) %>% # select the set
  dplyr::mutate(prop = round(prop.table(n)*100, digits =1))

pivot2 <- seq.df %>%
  dplyr::group_by(type) %>%
  dplyr::count(set2t) %>% # select the set
  dplyr::mutate(prop = round(prop.table(n)*100, digits =1))

pivot$n2 <- pivot2$n
pivot$prop2 <- pivot2$prop

### summary categories  
pivot <- seq.df %>%
  #dplyr::filter(type == "c") %>%
  dplyr::group_by(category, type) %>%
  dplyr::count(set1C) %>% # select the set
  dplyr::mutate(prop = round(prop.table(n)*100, digits =1))

pivot2 <- seq.df %>%
  #dplyr::filter(type == "c") %>%
  dplyr::group_by(category, type) %>%
  dplyr::count(set2C) %>% # select the set
  dplyr::mutate(prop = round(prop.table(n)*100, digits =1))

pivot$typeC <- items.m$type[match(pivot$category, items.m$category)]
pivot$type_set1 <- items.m$type[match(pivot$set1C, items.m$category)]
pivot2$typeC <- items.m$type[match(pivot2$category, items.m$category)]
pivot2$type_set2 <- items.m$type[match(pivot2$set2C, items.m$category)]



### summary all items
pivot <- seq.df %>%
  dplyr::filter(rank < 11) %>%
  dplyr::group_by(items, type, rank) %>%
  dplyr::select(set1, set2, set3) %>%
  dplyr::arrange(type, rank)


# ================ 6 PLACES ================
# -------~~ df places --------

# create df for places
df.places <- as.data.frame(table(places.list$item, places.list$place))
names(df.places)[1:3] <- c("item", "place", "count")
df.places <- dcast(places.list, item~place)

# get percentages
cols.mat <- length(df.places)
for (i in seq_along(df.places$item)){
  df.places[i, 2:cols.mat] <- round((df.places[i, 2:cols.mat]/sum(df.places[i, 2:cols.mat])*100), digits = 0)}

# add type
df.places$type <- df$type[match(df.places$item, df$items)]
# df.places back up
df.placesBU <- df.places

# add rank to places
df.places$rank <- all.ranks$rank[match(df.places$item, all.ranks$items)]

# sort data frame
df.places <- df.places %>%
  arrange(type, rank)

# -------~~ summary statistics --------

### places per item and mean 
pivot <-  places.list %>%
  dplyr::distinct(item, place, type) %>%
  dplyr::group_by(type, item) %>%
  dplyr::count(item)

# get means by type of the above pivot df
pivot %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(avg = mean(n))


### transform matrix into data fram
m.places <- melt(df.placesBU, id=(c("item", "type")))
names(m.places)[3:4] <- c("place", "value")
# remove rank column 

# frequency by place
pivot <- m.places %>%
  dplyr::group_by(type, place) %>%
  dplyr::summarise(avg = round(mean(value), digits = 1))

# ================ 7 FORMS ================
# -------~~ prepare data --------

# prepare data frame / get number of forms per category 
pivot <- forms.list %>%
  dplyr::distinct(type, item, form) %>%
  dplyr::group_by(item, type) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(mean = mean(total), sd = sd(total), min = min(total), max = max(total))

# add rank
pivot$rank <- all.ranks$rank[match(pivot$item, all.ranks$items)]

pivot <- pivot %>%
  dplyr::filter(rank < 11, type == "u") 
mean(pivot$total)
sd(pivot$total)

# add percentages to data frame
pivot <- forms.list %>%
  dplyr::group_by(type, item, form) %>%
  dplyr::summarise(total= n()) %>%
  dplyr::mutate(per = (round(total/sum(total)*100, 1)))

# add rank
pivot$rank <- all.ranks$rank[match(pivot$item, all.ranks$items)]

pivot <- pivot %>%
  dplyr::filter(type == "u") %>%
  dplyr::arrange(type, item, per, rank)

# filter items with a prominent variety (> 40%)
pivotF <- pivot %>%
  dplyr::filter(per >95)

length(unique(pivotF$item))


# ================ 8 ACTIVITIES ================
# -------~~ prepare data --------

load(all.ranks)

# create df for places
df.activities <- as.data.frame(table(activities.list$item, activities.list$activity))
names(df.places)[1:3] <- c("item", "activity", "count")
df.activities <- dcast(activities.list, item~activity)

# get percentages
cols.mat <- length(df.activities)
for (i in seq_along(df.activities$item)){
  df.activities[i, 2:cols.mat] <- round((df.activities[i, 2:cols.mat]/sum(df.activities[i, 2:cols.mat])*100), digits = 0)}

# add type
df.activities$type <- df$type[match(df.activities$item, df$items)]
# df.places back up
df.activitiessBU <- df.activities

# add rank to places
df.activities$rank <- all.ranks$rank[match(df.activities$item, all.ranks$items)]

# sort data frame
df.activities <- df.activities %>%
  arrange(type, rank)


# summary df
pivot <- activities.list %>%
  dplyr::group_by(type, item, activity) %>%
  dplyr::summarise(total= n())

pivot <- activities.list %>%
  dplyr::group_by(item, activity) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(total = (round(n/sum(n)*100, 0)))

# ================ [9] SITUATIONS [PROBLEMS AND REMARKABLE] ================
# ================ [10] PEOPLES' OBSERVATIONs ================
# ================ [11] CONSUMPTION ================
# prepare data frame
pivot <- df %>%
  dplyr::select(items, items_uniq, p_corrected) %>%
  dplyr::group_by(items, items_uniq) %>%
  dplyr::summarise(total = length(items))

# ================ [12] NETWORKS ================
# -------~~ prepare data Items --------

# prepare data frame
pivot <- df %>%
  dplyr::distinct(item.number, p_corrected)

# pivot <- df %>%
#   dplyr::distinct(category, type, p_corrected)


#create data frame to add data
edge.df <- data.frame()

for (i in 1: max(pivot$p_corrected)){
  
  pivotEdge <- pivot %>%
    dplyr::filter(p_corrected == i)
  
  #create edge list
  edgelist <- expand.grid(from = pivotEdge$item.number, to = pivotEdge$item.number, w =  1 / length(pivotEdge$item.number), p = i)

  # add edgelist to dataframe
  edge.df <- rbind(edge.df, edgelist)
  
  print(i)
}

#transfer pivot df to other df
edgelist <- edge.df
#add number to items.list.m
items.data <- items.list 
items.data$item.number <- c(1:length(items.data$unique))

# add item and category to data frame 
edgelist$fromI <- df$items[match(edgelist$from, df$item.number)]
edgelist$toI <- df$items[match(edgelist$to, df$item.number)]
edgelist$rank <- all.ranks$rank[match(edgelist$fromI, all.ranks$items)]
edgelist$fromT <- df$type[match(edgelist$fromI, df$items)]
edgelist$toT <- df$type[match(edgelist$toI, df$items)]
edgelist$fromC <- df$category[match(edgelist$fromI, df$items)]
edgelist$toC <- df$category[match(edgelist$toI, df$items)]
edgelist$equal <- edgelist$from-edgelist$to


# resave(edgelist, file = "fname.RData")
#load("fname.RData")

# -------~~ summary statistics Items --------

# frequency based on two columns 

edge.df2 <- edgelist

# filter equal 
edge.df2 <- edgelist %>%
  dplyr::filter(equal !=0)


# working pivot 
pivot <- edge.df2 %>%
  dplyr::group_by(fromI, toI) %>%
  dplyr::summarise(count = n()) %>% 
  #dplyr::filter(fromC %in% c("spice")) %>% 
  dplyr::filter(fromI %in% c("salt")) %>% 
  dplyr::mutate(rank = dense_rank(desc(count))) %>%  
  dplyr::filter(rank < 6)

# prepare for visualisation
pivot$count <- pivot$count/100
# capitalize from. This is necessary for the sankey diagram
library(Hmisc)
pivot$fromC <- capitalize(pivot$fromI)



# # working plot add ranks 
# pivot <- edge.df2 %>%
#   dplyr::group_by(fromI, toI) %>%
#   dplyr::summarise(count = n()) %>% 
#   dplyr::filter(count > 30) %>%  
#   dplyr::filter(fromI %in% c("salt", "cheese", "oil")) %>% 
#   dplyr::mutate(rank = dense_rank(desc(count)))
# 
# pivot <- edge.df2 %>%
#   dplyr::filter(rank < 11) %>% 
#   dplyr::group_by(fromI, toI, rank) %>%
#   dplyr::summarise(count = n()) %>% 
#   dplyr::filter(count > 32)




# -------~~ sankey diagram Items --------
# Package
library(networkD3)
library(tidyverse)

# # I need a long format
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# 
# data_long <- data %>%
#   rownames_to_column %>%
#   gather(key = 'key', value = 'value', -rowname) %>%
#   filter(value > 0)
# colnames(data_long) <- c("source", "target", "value")
# data_long$target <- paste(data_long$target, " ", sep="")

###
names(pivot)[1:3] <- c("source", "target", "value")
data_long <- pivot
data_long$value <- data_long$value^10


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'

my_color <- JS("d3.scaleOrdinal(d3.schemeCategory20);")

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=my_color, nodeWidth=20, fontSize=14, nodePadding=10)

# Check the current search path for fonts
font_paths()    
#> [1] "C:\\Windows\\Fonts"

# List available font files in the search path
font_files()    
#>   [1] "AcadEref.ttf"                                
#>   [2] "AGENCYB.TTF"                           
#> [428] "pala.ttf"                                    
#> [429] "palab.ttf"                                   
#> [430] "palabi.ttf"                                  
#> [431] "palai.ttf"

# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("Palatino", "pala.ttf")

font_families()
#> [1] "sans"         "serif"        "mono"         "wqy-microhei"
#> [5] "Montserrat"   "Roboto"       "Palatino"

## automatically use showtext for new devices
showtext_auto() 
myFont1 <- "Montserrat"
myFont2 <- "Roboto"




# -------~~ prepare data Category --------

# prepare data frame
pivot <- df %>%
  dplyr::distinct(category, type, p_corrected)

#create data frame to add data
edge.df <- data.frame()

# create data frame of concurrencies 
for (i in 1: max(pivot$p_corrected)){
  pivotEdge <- pivot %>%
    dplyr::filter(p_corrected == i)
  
  #create edge list
  edgelist <- expand.grid(from = pivotEdge$category, to = pivotEdge$category, w =  1 / length(pivotEdge$category), p = i)

  # add edgelist to dataframe
  edge.df <- rbind(edge.df, edgelist)
}

#transfer pivot df to other df
edgelist <- edge.df

#add number to items.list.m
items.data <- items.list 
items.data$item.number <- c(1:length(items.data$unique))

# add item and category to data frame 
edgelist$fromI <- df$items[match(edgelist$from, df$item.number)]
edgelist$toI <- df$items[match(edgelist$to, df$item.number)]
edgelist$rank <- all.ranks$rank[match(edgelist$fromI, all.ranks$items)]
edgelist$fromT <- df$type[match(edgelist$fromI, df$items)]
edgelist$toT <- df$type[match(edgelist$toI, df$items)]
edgelist$fromC <- df$category[match(edgelist$fromI, df$items)]
edgelist$toC <- df$category[match(edgelist$toI, df$items)]
edgelist$equal <- edgelist$from-edgelist$to


# resave(edgelist, file = "fname.RData")
# load("fname.RData")

# -------~~ summary statistics Category --------

# frequency based on two columns 

edge.df2 <- edgelist

# filter equal 
edge.df2 <- edgelist %>%
  dplyr::filter(equal !=0)


# working pivot 
pivot <- edge.df2 %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::filter(from %in% c("spice")) %>% 
  dplyr::mutate(rank = dense_rank(desc(count))) %>%  
  dplyr::filter(rank < 6)

# prepare for visualisation
pivot$count <- pivot$count/100
# capitalize from. This is necessary for the sankey diagram
pivot$from <- capitalize(as.character(pivot$from))


# # working plot add ranks 
# pivot <- edge.df2 %>%
#   dplyr::group_by(fromI, toI) %>%
#   dplyr::summarise(count = n()) %>% 
#   dplyr::filter(count > 30) %>%  
#   dplyr::filter(fromI %in% c("salt", "cheese", "oil")) %>% 
#   dplyr::mutate(rank = dense_rank(desc(count)))
# 
# pivot <- edge.df2 %>%
#   dplyr::filter(rank < 11) %>% 
#   dplyr::group_by(fromI, toI, rank) %>%
#   dplyr::summarise(count = n()) %>% 
#   dplyr::filter(count > 32)


# -------~~ sankey diagram Categories --------
# Package
library(networkD3)
library(tidyverse)

# # I need a long format
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# 
# data_long <- data %>%
#   rownames_to_column %>%
#   gather(key = 'key', value = 'value', -rowname) %>%
#   filter(value > 0)
# colnames(data_long) <- c("source", "target", "value")
# data_long$target <- paste(data_long$target, " ", sep="")

###
names(pivot)[1:3] <- c("source", "target", "value")
data_long <- pivot

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=20, fontSize=13, nodePadding=10)


# ================ OTHERS ================

# number of items in a category 

pivot <- items.list %>%
  dplyr::filter(type == "e") %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(per = (round(n/sum(n)*100, 0)))  %>%
  dplyr::arrange(-n)  

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}






