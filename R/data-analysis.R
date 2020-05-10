# ================ 0 LOAD LIBRARIES AND DATA SET ================ 
###
library(dplyr)
library(psych)
library(multcomp)
library(car)
library(yarrr)
library(FSA)
library(coin)
library(rcompanion)

# data frame
df <- reg.new.con.cat

# ================ 1 ALL interactions ================ 

cat("------------------ ", "sessions")

# prepare data frame
pivot <- df %>%
  dplyr::select(items, session, p_corrected) %>% 
  dplyr::group_by(session, p_corrected) %>% 
  dplyr::summarise(total = length(items))

# order data frame
pivot <- pivot[order(pivot$'session'), ]

# check for normality 
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
61.5/sqrt(40)

cat("------------------ ", "type")

pivot <- df %>%
  dplyr::group_by(session, participant, type) %>% 
  dplyr::summarise(total = length(items), percent = (length(items)/ length(df$items))*100)

# re-order type 
pivot$type <- ordered(pivot$type, levels = c("c", "u", "e"))

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
my_comparisons <-  list(c("c", "u"),  c("c", "e"))

ggboxplot(pivot, x = "type", y = "total",
          color = "type", add = "jitter") +
  stat_compare_means(comparisons = my_comparisons) +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 800) +                     # Add global p-value
  ggtitle("Interactions and Type")

# mean and sd 
pivot %>% dplyr::group_by(type) %>%
  dplyr::summarise(count = n(), mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), total = sum(total))


cat("------------------ ", "categories", "c")

# prepare data
pivot <- df %>%
  filter(type == "c") %>%
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

cat("------------------ ", "data frame most items")

pivot <- df %>%
  dplyr::group_by(items, type) %>% 
  dplyr::summarise(total = n()) %>% 
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
inter.df <- data.frame()
for (col in cols.names){inter.df[[col]] <- as.numeric()}
inter.df[nrow(inter.df)+ 10,] <- NA #add empty NAs

inter.df[, 1:2] <- c(subset.c$items[1:10], subset.c$total[1:10])
inter.df[, 3:4] <- c(subset.u$items[1:10], subset.u$total[1:10])
inter.df[, 5:6] <- c(subset.e$items[1:10], subset.e$total[1:10])
inter.df$item <- c(1:10)

# re-order columns
inter.df <- inter.df %>%
  dplyr::select(item, everything())

# save df to fname.RData
resave('inter.df', file='fname.RData')

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

# save df to fname.RData
resave('dur.df', file='fname.RData')

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
  dplyr::summarise(count = n(), mean = mean(duration, na.rm = TRUE), sd = sd(duration, na.rm = TRUE), total = sum(duration))

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

# save df to fname.RData
# resave('top.df', file='fname.RData')

# ================ 4 STAGE OF USE ================ 

# preparation of data [adding relative start position column]

# function to find relative position
rel_summary <- function(df, seqs){
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- which.min(abs(seqs - df[i]))
  }
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
    out <- append(out, rel_pos)
  }
  out
}

# add relative position column to data frame 
dfR <- df
dfR$start_rel <- rel_positions(df)
dfR$start_gr <- as.factor(ceiling(dfR$start_rel/25))
  
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