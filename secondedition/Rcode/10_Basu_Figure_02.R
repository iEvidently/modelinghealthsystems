# let's create a dataset to analyze, including a variety of different types of variables
options(warn=-1)
pop = 2000
dummvars = 20 # binary dummy variables, e.g., yes/no for specific covariate values
secovars = 15 # secondary dummy variables, whose existence is influenced by the above-noted dummy variables, e.g., more likely to have a diagnosis if having other co-morbid conditions
catevars = 10 # categorical variables, values 1 through 5, e.g., income category, education, race/ethnicity, etc.
contvars = 5 # continuous variables, e.g., biomarker values
set.seed(100)
dummvarsmeans = runif(dummvars)/3 # producing dummy variables
set.seed(200)
x1=matrix(rbinom(pop*dummvars,1,dummvarsmeans),pop,dummvars, byrow=T) # putting dummy variables into matrix form
secovarselect = round(min(dummvars/2,10))
x2 = matrix(0,pop,secovars) # creating space for secondary dummy variables
set.seed(300)
x2[,1] = rbinom(pop,1,(x1[,1])/secovarselect)
for (i in 2:secovars){
  set.seed(400+i)
  x2[,i]=rbinom(pop,1,rowMeans(x1[,1:i])/secovarselect+runif(pop)/10) # producing secondary dummy variables with some noise to avoid perfect linear predictability
}
set.seed(500)
catevarsshape = round(10*runif(catevars)) # producing categorical variables
set.seed(600)
x3=matrix(round(5*rbeta(pop*catevars, 2, catevarsshape)),pop,catevars, byrow=T) # putting categorical variables into matrix form
set.seed(700)
contvarsshape = round(10*runif(contvars)) # producing con't variables by choosing shape parameters for beta distribution
set.seed(800)
contvarsmag = round(1000*runif(contvars)) # producing con't variables of various magnitudes to reflect claims' data variation in con't variable absolute numbers
set.seed(900)
x4=matrix(round(contvarsmag*rbeta(pop*contvars, 2, contvarsshape)),pop,contvars, byrow=T) # putting con't variables into matrix form
covars = cbind(x1,x2,x3,x4)
dim = dummvars+catevars+contvars+secovars
influencers = seq(1,dim,round(dim/10)) # choosing which variables are truly influential on the primary outcome
set.seed(1000)
coefs = runif(length(influencers))
set.seed(1100)
# simulating probability of the primary outcome based on influential variables, plus some complex interactions among other variables, producing positive and negative heterogeneity, and adding some noise
outprob = (rowMeans(coefs*covars[,influencers])/dim+((covars[,47]*covars[,48]*covars[,49]*covars[,50])/(3*max(covars[,47]*covars[,48]*covars[,49]*covars[,50])))+covars[,round(3*dim/20)]*covars[,round(4*dim/20)]/2-covars[,round(5*dim/20)]*covars[,round(7*dim/20)]/2+rnorm(pop, mean =0, sd = .1)) 
outprob[outprob>1]=1
outprob[outprob<0]=0
set.seed(1200)
y = rbinom(pop,1,outprob) # making the primary outcome variable binary and probabilistic
alldata = data.frame(cbind(x1,x2,x3,x4,y))
colnames(alldata) = c(paste("X",c(1:dim),sep=""),"y")
alldata$y = factor(alldata$y)
write_csv(alldata,"10_Basu_sampledata.csv")



# import and analyze data
library(readr)
setwd("~/Downloads")
alldata = read_csv("10_Basu_sampledata.csv")
View(alldata)
summary(alldata)

library(caret)
set.seed(1300)
splitIndex <- createDataPartition(alldata$y, p = .8, list = FALSE, times = 1) # randomly splitting the data into train and test sets
trainSplit <- alldata[ splitIndex,]
testSplit <- alldata[-splitIndex,]
prop.table(table(trainSplit$y))


library(h2o)
h2o.init()
# split up the data into training and testing subsets
train <- as.h2o(trainSplit)
test <- as.h2o(testSplit)
y <- "y"
x <- setdiff(names(train), y)
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  validation_frame = test,
                  max_runtime_secs = 300)
lb <- aml@leaderboard
lb
aml@leader

