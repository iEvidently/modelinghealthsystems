# chapter 9 simulated trial data, data generating process
n = 10000
covs = 24
chars=c(130, 90, 1, 200, 50,
        30, 20, 20, 100, 0.5,
        12, 10, 30, 250, 0.5,
        0.5, 0.4, 0.5, 0.5, 0.5,
        0.5, 0.5, 0.5, 0.5)
sds = c(30, 9, .1, 20, 5,
        3, 2, 2, 10, 0.1,
        1, 1, 3, 25, 0.1,
        0.1, 0.1, 0.1, 0.1, 0.1,
        0.1, 0.1, 0.1, 0.1)
set.seed(100)
cormat = matrix(runif(covs*covs)/4,ncol=covs,byrow=TRUE) 
diag(cormat)=rep(1,covs)
cormat[upper.tri(cormat)] = 0
rev = t(cormat)
cormat[upper.tri(cormat)] = rev[upper.tri(rev)]
#install.packages('MASS')
#install.packages('MBESS')
library(MASS)  
library(MBESS)      
sigma   =cor2cov(cormat,sds)   
set.seed(200)
pop     =mvrnorm(n,chars,sigma)              
colnames(pop) = c("sbp", "dbp", "creat", "tchol", "hdl", 
                  "bmi", "ast", "alt", "hr", "hodm", 
                  "hgb", "pt", "ptt", "plts", "falls",
                  "homi", "hostr", "hochf", "asa", "thiazide",
                  "ccb", "acei", "bblocker", "lasix")
library(tidyverse)
pop=as.tibble(pop)
pop$hodm = round(pop$hodm)
pop$falls = round(pop$falls)
pop$homi = round(pop$homi)
pop$hostr = round(pop$hostr)
pop$hochf = round(pop$hochf)
pop$asa = round(pop$asa)
pop$thiazide = round(pop$thiazide)
pop$ccb = round(pop$ccb)
pop$acei = round(pop$acei)
pop$bblocker = round(pop$bblocker)
pop$lasix = round(pop$lasix)
set.seed(300)
pop$drug = rbinom(n,1,.5)
summary(pop)
xb = (.2*pop$sbp+30*pop$hodm+.2*pop$sbp*pop$hodm)
xb = 0.7*xb*pop$drug+xb*(1-pop$drug)
set.seed(400)
pop$stroke = rbinom(n,1,xb/max(xb))
xb2 = (3*pop$pt+20*pop$falls+.5*pop$pt*pop$falls)
xb2 = 1.3*xb2*pop$drug+xb2*(1-pop$drug)
set.seed(500)
pop$bleed = rbinom(n,1,xb2/max(xb2))
write_csv(pop,"09_Basu_sampledata.csv")
rm(list=ls())


# import and analyze data
library(readr)
setwd("~/Downloads")
trialdata = read_csv("09_Basu_sampledata.csv")
View(trialdata)
summary(trialdata)
table(trialdata$stroke,trialdata$bleed)
logisticmodel_stroke = glm(stroke ~ ., family = binomial(), data = trialdata)
summary(logisticmodel_stroke)
logisticmodel_bleed = glm(bleed ~ ., family = binomial(), data = trialdata)
summary(logisticmodel_bleed)


install.packages("Matching")
library(Matching)
pairs = Match(Tr=trialdata$drug,X=cbind(trialdata$sbp,trialdata$hodm,trialdata$falls,trialdata$pt))
strokediff = trialdata$stroke[pairs$index.control] - trialdata$stroke[pairs$index.treated]
bleeddiff = trialdata$bleed[pairs$index.control] - trialdata$bleed[pairs$index.treated]
score =  strokediff + bleeddiff
table(score)

install.packages('party')
library(party)
matchdata = cbind(score,
                  trialdata$sbp[pairs$index.control],
                  trialdata$hodm[pairs$index.control],
                  trialdata$falls[pairs$index.control],
                  trialdata$pt[pairs$index.control])
colnames(matchdata) = c("score","sbp","hodm","falls","pt")
fit <- ctree(score ~ sbp+hodm+falls+pt, data=as.data.frame(matchdata))
plot(fit, main="CART for new drug vs warfarin")




