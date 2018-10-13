
# first, simulate v = 2, tau = 0.1
N = 100000
beta = 0.001
mu = 1/75
gamma = 0.05
v = 2
kappa = 0.1
tau = 0.1

time = 20
dt = 0.01

S = 99999
E = 0
I = 1
R = 0
Deaths = 0

Svec = S
Evec = E
Ivec = I
Rvec = R


for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + (1-p)*mu*N*dt - beta*S*I*dt - mu*S*dt
    E = E + beta*S*I*dt - gamma*E*dt - mu*E*dt -tau*E*dt
    I = I + gamma*E*dt - v*I*dt - kappa*I*dt - mu*I*dt
    R = R + p*mu*N*dt + v*I*dt - mu*R*dt +tau*E*dt
    Svec = c(Svec, S)
    Evec = c(Evec, E)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    N = S+E+I+R
    Deaths = c(Deaths,kappa*I*dt)
  }
}
  

# now simulate v = 3, tau = 0
N = 100000
beta = 0.001
mu = 1/75
gamma = 0.05
v = 3
kappa = 0.1
tau = 0.

time = 20
dt = 0.01

S = 99999
E = 0
I = 1
R = 0
Deaths2 = 0

Svec = S
Evec = E
Ivec = I
Rvec = R

 
for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + (1-p)*mu*N*dt - beta*S*I*dt - mu*S*dt
    E = E + beta*S*I*dt - gamma*E*dt - mu*E*dt -tau*E*dt
    I = I + gamma*E*dt - v*I*dt - kappa*I*dt - mu*I*dt
    R = R + p*mu*N*dt + v*I*dt - mu*R*dt +tau*E*dt
    Svec = c(Svec, S)
    Evec = c(Evec, E)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    N = S+E+I+R
    Deaths2 = c(Deaths2,kappa*I*dt)
  }
}



plot(Deaths,lty=1,type="l",xlab="time steps",ylab="Deaths")
lines(Deaths2,lty=2)
legend(1500,0.5,c("Latent Rx","Active Rx"),lty=c(1,2))

