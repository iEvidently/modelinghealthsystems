n = 10000
patients=rnorm(n, mean  = 2000, sd = (2000-1500)/1.96)
nursecaremanager = rnorm(n, mean = 100000, sd = (100000-75000)/1.96)
psychiatrist = rnorm(n, mean = 10000, sd = (10000-7500)/1.96)
inclinicpsychsw = rnorm(n, mean = 120000, sd = (120000-100000)/1.96)
spacecosts = rnorm(n, mean = 1200, sd = (12000-1000)/1.96)

ccmcosts = nursecaremanager + psychiatrist
pcbcosts = inclinicpsychsw + spacecosts

ccmcostperptperyr = ccmcosts/patients
pcbcostperptperyr = pcbcosts/patients

quantile(ccmcostperptperyr,c(.025,.975))
quantile(pcbcostperptperyr,c(.025,.975))

require(ggplot2)
require(reshape2)

set.seed(1)
df <- data.frame(ccmcostperptperyr,pcbcostperptperyr)

ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")
