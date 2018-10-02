# before vaccination

N = 100000
mu = 1/(75*52)
w = 1/130
beta = 0.001
v = 1
d = 0.1

time = 52
dt = 0.01

S = 99999
I = 1
R = 0
D = 0

Svec = S
Ivec = I
Rvec = R
Dvec = D


for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + mu*N*dt + w*R*dt - beta*S*I*dt - mu*S*dt
    I = I + beta*S*I*dt - v*I*dt - mu*I*dt - d*I*dt
    R = R + v*I*dt - mu*R*dt - w*R*dt 
    D = D + d*I*dt 
    Svec = c(Svec, S)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    Dvec = c(Dvec, D)
    N = S+I+R
  }
}
  
max(Dvec)


# after vaccination

N = 100000
mu = 1/(75*52)
w = 1/130
beta = 0.001
v = 1
d = 0.1
f = 0.5
phi = 1/65

time = 52
dt = 0.01

S = 99999*(1-f)
I = 1
R = 0
D = 0
M = 99999*f

Svec = S
Ivec = I
Rvec = R
Dvec = D
Mvec = M


for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + (1-f)*mu*N*dt +phi*M*dt + w*R*dt - beta*S*I*dt - mu*S*dt
    I = I + beta*S*I*dt - v*I*dt - mu*I*dt - d*I*dt
    R = R + v*I*dt - mu*R*dt - w*R*dt 
    D = D + d*I*dt 
    M = M + f*mu*N*dt - mu*M*dt - phi*M*dt
    Svec = c(Svec, S)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    Dvec = c(Dvec, D)
    Mvec = c(Mvec, M)
    N = S+I+R
  }
}

max(Dvec)
