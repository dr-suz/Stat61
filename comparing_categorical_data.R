# R Code for tests of categorical data 
library(tidyverse) ## This package allows me to use the pipe operator, making code easier to read
library(palmerpenguins) ## All examples will reference the Palmer Penguins data set 
penguin_dat_full <- penguins %>% na.omit()

penguin_dat_cat <- penguin_dat_full %>% select(c("species", "island", "sex")) 

penguin_dat_cat %>% table 


## Note that since this is observational data and there are no treatments to randomly assign, this data does not clearly lend itself to being analyzed by any of the following procedures. 
## For the purpose of illustration however, let's consider the following contexts and pretend the data we have resulted from the following experimental designs. 

set.seed(101) ## this just makes sure the random number generation starts from the same place so our random sampling processes are reproducible 



## 1) Fisher's exact test 
## It's difficult to imagine an experimental setting where we could use Fisher's exact test with this data as we'd need to introduce (at the very least) another binary variable
## For completeness however, you can conduct Fisher's exact test for a 2X2 contingency table with the following code
fisher_dat <- matrix(rpois(n=4, lambda=5), nrow=2, ncol=2)
fisher_dat
chisq.test(fisher_dat, simulate.p.value=TRUE)


## 2) Chi-squared test of independence 
# Researcher decides to collect a sample of, say, n=120 penguins from this region (that is, all three islands)
# They are interested in testing for an association between the number of male and female penguins and the island the penguins inhabit 
# This is a test of independence since neither the total number of male or female penguins is fixed, nor is the total number of penguins in each island fixed 
penguin_dat_cat %>% dim 
indx1 <- sample(1:length(penguin_dat_cat$sex), size=120, replace=FALSE)
data_ind <- penguin_dat_cat[indx1,] %>% select(-species)
## once the random sample of data has been obtained we now calculate the p-value of the test with: 
chisq.test(table(data_ind))


## 3) Chi-squared test of homogeneity 
# Researcher supposes that in a healthy penguin community, the proportion of males is the same as the propotion of females 
# They are interested in testing whether the distribution of male and female is the same on each of the three islands in this region, so a random sample of 40 penguins is collected from each island. 
# This is a test of homogeneity because the marginal total number of penguins from each island is fixed. 
penguin_dat_cat %>% select("island") %>% table
indx2 <- sample(1:163, size = 40, replace=FALSE)
indx3 <- sample(1:123, size = 40, replace=FALSE)
indx4 <- sample(1:47, size = 40, replace=FALSE)
data2 <- penguin_dat_cat %>% filter(island =="Biscoe") %>% select(-species)
data3 <- penguin_dat_cat %>% filter(island =="Dream") %>% select(-species)
data4 <- penguin_dat_cat %>% filter(island =="Torgersen") %>% select(-species)
data_homog <- rbind(data2[indx2,], data3[indx3,], data4[indx4,])
table(data_homog)
## once the random sample of data has been obtained we now calculate the p-value of the test with: 
chisq.test(table(data_homog), p = rep(c(0.5, 0.5), 3))


## 4) Chi-squared test of goodness of fit 
# (Suppose some catastrophic event impacted these islands and... ) Researcher has biological background info that a penguin community with 10% males or less is at risk of dying out 
# They are intested in testing whether the distribution of male and female penguins is at risk of extinction on any of the three islands
# This is a non-balanced version of the test above and so is a goodness-of-fit test. 
# Using the same "experimental data" as in the test for homogeneity we can switch to a test for goodness of fit with:
chisq.test(table(data_homog), p = rep(c(0.9, 0.1), 3))
## Note by default, R orders the levels of a categorial variable alpha-numerically. So the first probability goes with the female penguins and the second goes with males.