################################################################################
# Bootstrap PCA analysis
# Giacomo Lemoli
# Quant II Spring 2022
################################################################################

## Clean workspace
rm(list=ls())

## Packages
library(tidyverse)
library(haven)
library(fixest)

## Directory
setwd("C:/Users/giaco/Dropbox/NYU/TA Work/Quant 2 Spring 2022/Lab material/lab9")

## Import data from Bautista et al
d <- read_dta("FinalDatasetForReplication.dta") 

# Prepare
d <- d %>% filter(MainSample == 1)

## Variables to summarize with PCA
vars <- c("Share_reg70_w2", "VoteShareNo", "VoteShareNo_pop70")

## Bootstrap iterations
nboot <- 1000

## Matrix where to store the regression estimates
ests <- rep(NA, nboot)

## Set seed
set.seed(123)

## Bootstrap loop
for (i in 1:nboot){
  # Random sample
  sampled <- sample(1:nrow(d), nrow(d), replace = T)
  sample <- d[sampled,]
  
  # Scale and center variables for PCA
  sample <- sample %>% mutate_at(vars, scale)
  
  # PCA
  pca <- princomp(sample[,vars], scores=T)
  
  # Scores
  sample$score <- pca$scores[,1]
  
  # Estimate regression on the score
  fit <- feols(score ~ DMilitaryPresence + share_allende70 + share_alessandri70 +
                 lnDistStgo + lnDistRegCapital + Pop70_pthousands + 
                 sh_rural_70 | IDProv, data = sample, weights = ~Pop70, vcov = "hetero")
  
  # Save estimate
  ests[i] <- coefficients(fit)["DMilitaryPresence"]
}

## Mean and CI quantiles
c(mean(ests), quantile(ests, c(0.025, 0.975)))
