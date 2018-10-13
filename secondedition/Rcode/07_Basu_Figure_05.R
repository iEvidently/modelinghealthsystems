# before intervention:

transition = matrix(c(0.95, 0.2, 0,
                      0.05, 0.7, 0.3,
                      0, 0.1, 0.7),ncol=3,byrow=TRUE)
timesteps = 120
h = rep(0,timesteps)
m = rep(0,timesteps)
s = rep(0,timesteps)
h[1]=1
priorstate =c(h[1],m[1],s[1])
for (t in 2:timesteps)
{
  priorstate =c(h[t-1],m[t-1],s[t-1])
  newstate= transition%*%priorstate 
  h[t]=newstate[1]
  m[t]=newstate[2]
  s[t]=newstate[3]
}
s[timesteps]
plot(s,xlab="Years",ylab="Severe malnutrition probability")


# after intervention:
transition_new = matrix(c(0.95, 0.2, 0.5,
                          0.05, 0.7, 0.5,
                          0, 0.1, 0),ncol=3,byrow=TRUE)
timesteps = 120
h_new = rep(0,timesteps)
m_new = rep(0,timesteps)
s_new = rep(0,timesteps)
h_new[1]=1
priorstate_new =c(h[1],m[1],s[1])
for (t in 2:timesteps)
{
  priorstate_new =c(h[t-1],m[t-1],s[t-1])
  newstate_new= transition_new%*%priorstate_new 
  h_new[t]=newstate_new[1]
  m_new[t]=newstate_new[2]
  s_new[t]=newstate_new[3]
}
s_new[timesteps]
plot(s_new,xlab="Years",ylab="Severe malnutrition probability")


# cost-effectiveness analysis:
costs = sum(100000*(m*10+s*(25+50)))
costs_new = sum(100000*(m_new*10+s_new*(25+50)))

qalys = sum(100000*(h*1+m*(1-0.1)+s*(1-0.3)))
qalys_new = sum(100000*(h_new*1+m_new*(1-0.1)+s_new*(1-0.3)))

icer = (costs_new-costs)/(qalys_new-qalys)

