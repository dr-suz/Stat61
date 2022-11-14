# R Code for comparing the means of two or more independent populations 
library(tidyverse) ## This package allows me to use the pipe operator, making code easier to read
library(palmerpenguins) ## All examples will reference the Palmer Penguins data set 
penguin_dat_full <- penguins %>% na.omit()
penguin_dat_full


##1) 
## Let's compare the body mass of male and female penguins
two_samp_dat <- penguin_dat_full %>% select(body_mass_g, sex) %>% group_by(sex)

  ## Two-sample, independent t-test 
  t.test(body_mass_g ~ sex, data=two_samp_dat, paired=FALSE, conf.level=0.90)

  ## Non-parametric alternative: Mann-Whitney test (or Wilcoxon rank sum test)
  wilcox.test(body_mass_g ~ sex, data=two_samp_dat, paired=FALSE, conf.level=0.90)

  
##2) 
## Next, let's compare the bill length for penguins on each of the different islands  
anova_1way_dat <- penguin_dat_full %>% select(bill_length_mm, island) %>% group_by(island)
  
  ## One way ANOVA 
  mod <- lm(bill_length_mm~island, data = anova_1way_dat)
  anova(mod)
  
  ## Non-parametric alternative: Kruskal-wallis test
  kruskal.test(bill_length_mm~island, data=anova_1way_dat)


##3) 
## Multiple comparison adjustments 
  ## Tukey's HSD
  mod_aov <- aov(bill_length_mm~island, data = anova_1way_dat)
  TukeyHSD(mod_aov)
  ## To use other methods for correcting for multiple testing such as: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"
  pairwise.t.test(x=anova_1way_dat$bill_length_mm, g=anova_1way_dat$island, p.adjust.method = "BH") #output is the adjusted p-value for each paired test that needs only to be compared to your pre-determined overall significance level
  
  
##4)
## Next, let's compare the bill length for penguins on each of the islands and across the different species on each island
anova_2way_dat <- penguin_dat_full %>% select(bill_length_mm, island, species) %>% group_by(island, species)
  ## Two way ANOVA (additive model)
    mod2 <- aov(bill_length_mm~island+species, data = anova_2way_dat)
    summary(mod2)
    
  ## Two way ANOVA (interaction/multiplicative model)
    mod3 <- aov(bill_length_mm~island*species, data = anova_2way_dat)
    aov(mod3)
    summary(mod3)  
    
    

