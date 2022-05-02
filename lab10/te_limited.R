################################################################################
# Causal effects in limited dependent variable models
#
# Giacomo Lemoli
# Quant II Spring 2022
################################################################################

## Clean workspace
rm(list=ls())

## Packages to use
library(haven)
library(tidyverse)
library(margins)
library(marginaleffects)

## Set your path
path <- "C:/Users/giaco/Dropbox/NYU/TA Work/Quant 2 Spring 2022/Lab material/Working material"

## Import the data
dat <- read_dta(paste0(path, "/Censorship.dta"))

## Create a dichotomous outcome variable
dat <- dat %>% mutate(ak_dum = case_when(ak_rating >= 4 ~ 1,
                                         ak_rating < 4 ~ 0))

## Logit regression
fit <- glm(ak_dum ~ censorship + liberalization + sed + education, 
           family = binomial(link = "logit"),
           data = dat)

## AMEs with marginaleffects
mfx <- marginaleffects(fit)
summary(mfx)

## Predicted probability method with bootstrap SEs
ndata <- function(data, newval){
  data <- mutate(data, education = newval)
  data
}

set.seed(123)

nboot <- 1000

ate_2 <- ate_3 <- ate_4 <- rep(NA, nboot)

for (i in 1:nboot){
  # Random sample
  sampled <- sample(1:nrow(dat), nrow(dat), replace = T)
  sample <- dat[sampled,]
  
  # Fit logit model
  fit <- update(fit, .~., data=sample)
  
  # Compute ATE estimate
  ate_2[i] <- mean(predict(fit, newdata = ndata(sample,2), type="response") -
                     predict(fit, newdata = ndata(sample,1), type="response"), na.rm = T)
  
  ate_3[i] <- mean(predict(fit, newdata = ndata(sample,3), type="response") -
                     predict(fit, newdata = ndata(sample,2), type = "response"), na.rm = T)
  
  ate_4[i] <- mean(predict(fit, newdata = ndata(sample,4), type = "response") -
                     predict(fit, newdata = ndata(sample,3), type = "response"), na.rm = T)
  

}

ests_mfx <- cbind(c("AME"), summary(mfx)[2,c("estimate", "conf.low", "conf.high")]) %>%
  as.data.frame() %>%
  rename(type =`c("AME")`, coef = estimate, ll = `conf.low`, ul = `conf.high`)
  
ests_pp <- cbind(c("PP: From 1 to 2", "PP: From 2 to 3", "PP: From 3 to 4"),
              do.call("rbind", lapply(list(ate_2, ate_3, ate_4), function(x) c(mean(x), quantile(x, c(0.025, 0.975)))))) %>%
  as.data.frame() %>%
  rename(type = V1, coef = V2, ll = `2.5%`, ul = `97.5%`) %>%
  mutate_at(vars(coef, ll, ul), as.numeric)
  
  

ggplot(rbind(ests_pp, ests_mfx), aes(x=type, y=coef)) + geom_point() + geom_errorbar(aes(ymax = ll, ymin = ul)) +
  labs(x="Object", y="Estimate") + theme_bw()
