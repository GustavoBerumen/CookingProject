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

# ================ ALL interactions ================ 

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
  dplyr::select(items, type, session, p_corrected, duration) %>% 
  dplyr::group_by(type, p_corrected) %>% 
  dplyr::summarise(total = sum(duration), n = length(items))

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

# ================ DURATION ================ 

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
  dplyr::group_by(session, participants) %>% 
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
my_comparisons <-  list(c("c", "u"), c("u", "e"), c("c", "e"))

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