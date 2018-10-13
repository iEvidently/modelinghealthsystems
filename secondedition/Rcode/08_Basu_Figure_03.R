n = 5000000
chars=c(75, 130, 25, 108)
sds = c(10, 25, 5, 20)
cormat = matrix(c( 1.000,	 0.302, 	0.571, 	 0.776,
                   0.302, 1.000,	 0.110, 	 0.587,
                   0.571,	 0.110, 	1.000 ,	 0.002,
                   0.776,	 0.587,	 0.002,	 1.000 ),ncol=4,byrow=TRUE) 
install.packages('MASS')
install.packages('MBESS')
library(MASS)  
library(MBESS)      
sigma   =cor2cov(cormat,sds)    
pop     =mvrnorm(n,chars,sigma)              
head(pop, 10)

#install.packages('matrixStats')
library(matrixStats)
colMeans(pop)
colSds(pop)
cor(pop)
pairs(~pop[1:1000,1]+pop[1:1000,2]+pop[1:1000,3]+pop[1:1000,4])
