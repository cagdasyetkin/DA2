## Created by Group 31
# Peter, Paziczki
# Yetkin, Cagdas
# Niranjan, Nikitha

#Problem Set 3

rm(list=ls())
library(data.table)
# 1.	Use the amazon_compare.csv file and load it into R.
amazon <- read.csv("amazon_compare.csv")

# 2.	Do steps 2-3 exactly as you did in PS1. 
# The random number generator should be from the same distribution as in PS1. 
# What type of sampling is this?
sum(is.na(amazon$price)) # number of NAs in price variable
sum(is.na(amazon$price_online)) # number of NAs in price_online variable

# Dropping obersvations where price or price_online variables or both are missing
amazon2 <- amazon[which(!is.na(amazon$price) & !is.na(amazon$price_online)),]

# Dropping observations that are larger than the 99-percentile of price and price_online variable
amazon3 <- amazon2[which(amazon2$price <= quantile(amazon2$price, prob = 0.99) & 
                           amazon2$price_online <= quantile(amazon2$price_online, prob = 0.99)),]

# Createing a random number generator from a normal distribution setting the seed equal to our group number 31
#▼ and calling it id_rand
set.seed(31)
amazon3$id_rand <- rnorm(nrow(amazon3), mean = 0, sd = 1)
# Sorting the variable and selecting the first 1000 observations
amazon4 <- amazon3[order(amazon3$id_rand, decreasing = FALSE),]
amazon4 <- amazon4[1:1000,]
#This is simple random samlping.

# 3.	Copy-paste your answer in Q4 in PS1.
mean(amazon3$price)
sd(amazon3$price)
sd(amazon3$price)^2
quantile(amazon3$price, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(amazon3$price, probs = seq(from = 0, to = 1, by = 0.1))

mean(amazon3$price_online)
sd(amazon3$price_online)
sd(amazon3$price_online)^2
quantile(amazon3$price_online, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(amazon3$price_online, probs = seq(from = 0, to = 1, by = 0.1))

# 4. Use a t-test: compare the means of price and online price
# H0 = means are the same
# Ha = means are different, they are not equal

#t.test(amazon3$price == amazon3$price_online, alternative = c('two.sided')) # code provided by Arieda
t.test(amazon3$price, amazon3$price_online)
# p value = 0.4512 > 0.05. So we fail to reject the null hypothesis.

# Question 5

x <- mean(amazon3$price_online)
t.test(amazon3$price, mu=x) # gives p value > 0.05: the means are not different. Fail to reject the Null

y <- mean(amazon3$price)
t.test(amazon3$price_online, mu=y) # gives p value > 0.05: the means are not different. Fail to reject the Null

# Question 6.	Use once again the original dataset and drop observations 
# for that are above the 95 percentile of the price variable. Make summary statistics for the price variable
# (mean, standard deviations, quartiles, percenti-les).

amazon5 <- amazon[which(amazon$price <= quantile(amazon$price, prob = 0.95)),]
mean(amazon5$price)
sd(amazon5$price)
sd(amazon5$price)^2
quantile(amazon5$price, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(amazon5$price, probs = seq(from = 0, to = 1, by = 0.1))

#7.	Generate a dummy variable taking value one if price_online is missing and zero otherwise. 
# Call this variable missing_online. 
# Make histograms and boxplots of price and price_amazon for each value of this indicator variable. 
# What can you say with respect to these conditional distributions?

amazon5$missing_online <- ifelse(is.na(amazon5$price_online), 1, 0)
amazon5$missing_online <- factor(amazon5$missing_online)

library(ggplot2)
ggplot(amazon5, aes(price)) + geom_histogram() + facet_wrap(~missing_online) +
  labs(title = "Histogram of price variable", subtitle = "through missing_online binary variable", x = "price variable")
ggplot(amazon5, aes(price_amazon)) + geom_histogram() + facet_wrap(~missing_online) +
  labs(title = "Histogram of price_amazon variable", subtitle = "through missing_online binary variable", x = "price_amazon variable")
ggplot(amazon5, aes(missing_online, price)) + geom_boxplot() +
  labs(title = "Box-plot of price variable", subtitle = "through missing_online binary variable", x = "Missing_Online")
ggplot(amazon5, aes(missing_online, price_amazon)) + geom_boxplot() +
  labs(title = "Box-plot of price_amazon variable", subtitle = "through missing_online binary variable", x = "Missing_Online")

# Dropping the observations above the 95 percentile of price_amazon variable.
amazon5_1 <- amazon5[which(amazon5$price_amazon <= quantile(amazon5$price_amazon, prob = 0.95)),]

ggplot(amazon5_1, aes(price_amazon)) + geom_histogram() + facet_wrap(~missing_online) +
  labs(title = "Histogram of price_amazon variable", subtitle = "through missing_online binary variable", x = "price_amazon variable")
ggplot(amazon5_1, aes(missing_online, price_amazon)) + geom_boxplot() +
  labs(title = "Box-plot of price_amazon variable", subtitle = "through missing_online binary variable", x = "Missing_Online")

#8. Compare the means of the variables price and price_amazon variables across the two groups of the missing_online.

amazon5 <- data.table(amazon5) # creating a data.table object
amazon5[, lapply(.SD, mean), by = missing_online, .SDcols = c("price", "price_amazon")] # computing the means of
# price and price_amazon grouped by missing_online (dummy variable with 0s and 1s)

# Just as an experiment, I checked what would happen if I investigated the means after dropping the observations
# above the 95 percentile of price_amazon variable.
amazon5_1 <- data.table(amazon5_1) # creating a data.table object
amazon5_1[, lapply(.SD, mean), by = missing_online, .SDcols = c("price", "price_amazon")]

#9.	Compare the means of the variables price and  price_amazon and for each of the different retailers (or good categories).
# For which retailers (or good categories) you see no differences in the means of price and price_amazon?
# For which retailers (or good categories) do you see a difference in means the prices and price_amazon?

str(amazon5)
# Computing the mean of price and price_amazon grouped by category
amazon5[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
# Computing the mean of price and price_amazon grouped by retailer
amazon5[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
# Computing the mean of price and price_amazon grouped by both category and retailer
amazon5[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]

#10. Due to the knowledge from PS2_part2 and the above evidence, can you say anything wether the observations
# missing in the online price are missing at random?

# Please find our answer in the pdf.

# there are 1310 observations where online price is missing, loading those observations to amazon6
amazon6 <- amazon[is.na(amazon$price_online),]
amazon6 <- data.table(amazon6) # creating a data.table object

# computing the number of observation grouped by category and retailer among the 1310 observations
amazon6[,.N,by = list(category, retailer_s)]
# computing the number of observation grouped by category and retailer in the original data set
amazon <- data.table(amazon)
amazon[,.N,by = list(category, retailer_s)]
# proportion of missing variables
amazon6[,.N,by = list(category, retailer_s)] / amazon[,.N,by = list(category, retailer_s)] * 100

# computing the number of observation grouped by category among the 1310 observations
amazon6[,.N,by = category]
# computing the number of observation grouped by category in the original data set
amazon[,.N,by = category]
# proportion of missing variables
amazon6[,.N,by = category] / amazon[,.N,by = category] * 100

# summary statistics of observations where price_online was not missing
amazon2 <- data.table(amazon2)
#amazon2[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
#amazon2[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
amazon2[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]
#dropping the obs above 95 percentile of price variable and doing summary statistics
amazon2_2 <- amazon2[which(amazon2$price <= quantile(amazon2$price, prob = 0.95)),]
amazon2_2 <- data.table(amazon2_2)
#amazon2_2[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
#amazon2_2[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
amazon2_2[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]

# summary statistics of observations where price_online was missing
# amazon6[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
# amazon6[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
amazon6[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]
# dropping the observations above the 95 percentile of price variable and loading the rest amazon7
amazon7 <- amazon6[which(amazon6$price <= quantile(amazon6$price, prob = 0.95)),]
amazon7 <- data.table(amazon7) # creating a data.table object
# amazon7[, lapply(.SD, mean), by = category, .SDcols = c("price", "price_amazon")]
# amazon7[, lapply(.SD, mean), by = retailer_s, .SDcols = c("price", "price_amazon")]
amazon7[, lapply(.SD, mean), by = list(category, retailer_s), .SDcols = c("price", "price_amazon")]

#11. Do the same procedure as 10 in PS2_part2. What type of sampling is this?

set.seed(31)
amazon5$id_rand <- runif(nrow(amazon5), min = 0, max = 1)

#12.	Sort id_rand and select the first 100 observations in your dataset. Construct a 95 percent confidence interval (CI)
# for the mean of price. Interpret this confidence interval. What does a narrower confidence interval mean? Is the mean
# of price you got in 6 included in this interval? Should it be included?

# sorting by id_rand
amazon5 <- amazon5[order(amazon5$id_rand),]

# selecting the first hundred observation and computing mean and standard deviation
a <- 1
b <- 100
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q12 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q12.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q12.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

#13.	Repeat 11, sort id_rand and select and select the first 200 observations in your dataset make summary statistics for price.
#Construct a 95 percent (CI) for the mean of price. How does this confidence interval compare to the one in 12?
#Does this CI include the mean of price in 6? (Non mandatory!)

a <- 1
b <- 200
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q13 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q13.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q13.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

#14.	Repeat 11, sort id_rand and select and select the first and select the first 300 observations in your dataset make
#summary statistics for price. Construct a 95 percent (CI) for the mean of price. How does this confidence interval compare
# to the one in12 and 13? Does this CI include the mean of price in 6? (Non mandatory!)

a <- 1
b <- 300
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q14 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q14.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q14.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

#15.	Repeat 11, sort id_rand and select and select the first and select the first 1000 observations in your dataset make summary
# statistics for price. Construct a 95 percent (CI) for the mean of price. How does this confidence interval compare to the one
# in 12, 13 and 14? Does this CI include the mean of price in 6?

a <- 1
b <- 1000
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q15 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

CI_matrix <- matrix(c(Q12, Q13, Q14, Q15), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("sample size", "sample mean", "sample st dev", "interval_left", "interval_right")))
CI_matrix

Q15.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q15.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

QUA_0.25_matrix <- matrix(c(Q12.2, Q13.2, Q14.2, Q15.2), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "25%", "50%", "75%", "100%")))
QUA_0.25_matrix

QUA_0.1_matrix <- matrix(c(Q12.3, Q13.3, Q14.3, Q15.3), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")))
QUA_0.1_matrix

#16.	Add 1 as the last digit in your seed, example if your group number is 106 your seed should be set as 1061 and repeat 12-15.
# Have you answers changed? (Non mandatory for part-time students!)

set.seed(311)
amazon5$id_rand <- runif(nrow(amazon5), min = 0, max = 1)

# sorting by id_rand
amazon5 <- amazon5[order(amazon5$id_rand),]

# selecting the first hundred observation and computing mean and standard deviation
a <- 1
b <- 100
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q12 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q12.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q12.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 200
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q13 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q13.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q13.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 300
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q14 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q14.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q14.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 1000
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.975)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.975)*sd_dev/sqrt(sample_size)

Q15 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

CI_matrix <- matrix(c(Q12, Q13, Q14, Q15), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("sample size", "sample mean", "sample st dev", "interval_left", "interval_right")))
CI_matrix

Q15.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q15.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

QUA_0.25_matrix <- matrix(c(Q12.2, Q13.2, Q14.2, Q15.2), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "25%", "50%", "75%", "100%")))
QUA_0.25_matrix

QUA_0.1_matrix <- matrix(c(Q12.3, Q13.3, Q14.3, Q15.3), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")))
QUA_0.1_matrix

#17.	In your opinion how would your answers in points 12-15 change, if instead of using 95 percent (CI) you use a 90 or 99 percent (CI)?

#90% CI:
set.seed(31)
amazon5$id_rand <- runif(nrow(amazon5), min = 0, max = 1)

# sorting by id_rand
amazon5 <- amazon5[order(amazon5$id_rand),]

# selecting the first hundred observation and computing mean and standard deviation
a <- 1
b <- 100
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.95)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.95)*sd_dev/sqrt(sample_size)

Q12 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q12.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q12.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 200
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.95)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.95)*sd_dev/sqrt(sample_size)

Q13 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q13.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q13.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 300
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.95)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.95)*sd_dev/sqrt(sample_size)

Q14 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q14.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q14.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 1000
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.95)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.95)*sd_dev/sqrt(sample_size)

Q15 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

CI_matrix <- matrix(c(Q12, Q13, Q14, Q15), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("sample size", "sample mean", "sample st dev", "interval_left", "interval_right")))
CI_matrix

Q15.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q15.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

QUA_0.25_matrix <- matrix(c(Q12.2, Q13.2, Q14.2, Q15.2), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "25%", "50%", "75%", "100%")))
QUA_0.25_matrix

QUA_0.1_matrix <- matrix(c(Q12.3, Q13.3, Q14.3, Q15.3), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")))
QUA_0.1_matrix

#99% CI:
set.seed(31)
amazon5$id_rand <- runif(nrow(amazon5), min = 0, max = 1)

# sorting by id_rand
amazon5 <- amazon5[order(amazon5$id_rand),]

# selecting the first hundred observation and computing mean and standard deviation
a <- 1
b <- 100
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.995)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.995)*sd_dev/sqrt(sample_size)

Q12 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q12.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q12.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 200
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.995)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.995)*sd_dev/sqrt(sample_size)

Q13 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q13.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q13.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 300
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.995)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.995)*sd_dev/sqrt(sample_size)

Q14 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

Q14.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q14.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

a <- 1
b <- 1000
sample_size <- b-a+1
sd_dev <- amazon5[a:b, sd(price)]
mean <- amazon5[a:b, mean(price)]
CI_left <- mean-qnorm(0.995)*sd_dev/sqrt(sample_size)
CI_right <- mean+qnorm(0.995)*sd_dev/sqrt(sample_size)

Q15 <- c(sample_size, mean, sd_dev, CI_left, CI_right)

CI_matrix <- matrix(c(Q12, Q13, Q14, Q15), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("sample size", "sample mean", "sample st dev", "interval_left", "interval_right")))
CI_matrix

Q15.2 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.25))
Q15.3 <- quantile(amazon5[a:b,]$price, probs = seq(0,1,0.1))

QUA_0.25_matrix <- matrix(c(Q12.2, Q13.2, Q14.2, Q15.2), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "25%", "50%", "75%", "100%")))
QUA_0.25_matrix

QUA_0.1_matrix <- matrix(c(Q12.3, Q13.3, Q14.3, Q15.3), nrow = 4, byrow = TRUE, dimnames = list(c("Q12", "Q13", "Q14", "Q15"), c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")))
QUA_0.1_matrix

#18.	Many times in Data Analysis we need to generate new variables from the existing dataset as well as transform the existing ones.
# Create a new variable which takes on the number of times each date (“date” variable) is found in the dataset. Call this variable
# times_date. What type of variable is this? Do the following transformations:
# normalization (times_date-min(times_date)/max(times_date)-min(times_date)), standardization (times_date-mean(times_date)/sd(times_date))
# and logarithmic (log(times_date)). What can you say about these transformations?

amazon5[,times_date:=.N,by=date]
amazon5_norm<-amazon5[,.(normalization=times_date-min(times_date)/max(times_date)-min(times_date))]
ggplot(amazon5_norm,aes(normalization))+geom_histogram()

amazon5_standard<-amazon5[,.(standardization=times_date-mean(times_date)/sd(times_date))]
ggplot(amazon5_standard,aes(standardization))+geom_histogram()

amazon5_log<-amazon5[,.(log=log(times_date))]
ggplot(amazon5_log,aes(log))+geom_histogram()