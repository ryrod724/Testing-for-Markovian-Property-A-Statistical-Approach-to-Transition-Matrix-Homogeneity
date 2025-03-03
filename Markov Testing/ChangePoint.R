######################################################
#  SIMULATING MARKOV CHAIN WITH A SINGLE CHANGE POINT
######################################################
nsteps1<- 200
nsteps2<- 200
nsteps<- nsteps1+nsteps2


#specifying first transition probability matrix
tm1<- matrix(c(0.7, 0.1, 0.2, 0.1, 0.5, 0.4, 0.5, 0.2, 0.3), nrow=3, 
ncol=3, byrow=TRUE)

#creating Markov chain object
library(markovchain)
mc1<- new("markovchain", transitionMatrix=tm1, states=c("1", "2", "3"))

#specifying initial probability
p0<- c(1/3, 1/3, 1/3)

MC.states1<- c()
set.seed(2443923)

#simulating trajectories
state0<- sample(1:3, 1, prob=p0)
MC.states1<- rmarkovchain(n=nsteps1-1, object=mc1, t0=state0, include.t0=TRUE)

#specifying second transition probability matrix
tm2<- matrix(c(0.2, 0.3, 0.5, 0.3, 0.6, 0.1, 0.2, 0.1, 0.7), nrow=3, ncol=3, 
byrow=TRUE)
  
#creating Markov chain object
library(markovchain)
mc2<- new("markovchain", transitionMatrix=tm2, states=c("1", "2", "3"))

MC.states2<- c()
set.seed(4995758)
  
#simulating trajectories
MC.states2<- rmarkovchain(n=nsteps2, object=mc2, t0=MC.states1[nsteps1], 
include.t0=FALSE)
   
MC<- c(MC.states1,MC.states2)
  
###############################
#  DETERMINING CHANGE-POINT
###############################
   
library(Hmisc)
MC.lag<- Lag(MC,shift=1)

ind11<- c()
ind12<- c()
ind13<- c()
ind21<- c()
ind22<- c()
ind23<- c()
ind31<- c()
ind32<- c()
ind33<- c()
ind1<- c()
ind2<- c()
ind3<- c()
  
  for (i in 2:nsteps) {
      ind11[i-1]<- ifelse(MC.lag[i]=="1" & MC[i]=="1",1,0)
      ind12[i-1]<- ifelse(MC.lag[i]=="1" & MC[i]=="2",1,0)
      ind13[i-1]<- ifelse(MC.lag[i]=="1" & MC[i]=="3",1,0)
      ind1[i-1]<- ind11[i-1] + ind12[i-1] + ind13[i-1]
      ind21[i-1]<- ifelse(MC.lag[i]=="2" & MC[i]=="1",1,0)
      ind22[i-1]<- ifelse(MC.lag[i]=="2" & MC[i]=="2",1,0)
      ind23[i-1]<- ifelse(MC.lag[i]=="2" & MC[i]=="3",1,0)
      ind2[i-1]<- ind21[i-1] + ind22[i-1] + ind23[i-1]
      ind31[i-1]<- ifelse(MC.lag[i]=="3" & MC[i]=="1",1,0)
      ind32[i-1]<- ifelse(MC.lag[i]=="3" & MC[i]=="2",1,0)
      ind33[i-1]<- ifelse(MC.lag[i]=="3" & MC[i]=="3",1,0)
      ind3[i-1]<- ind31[i-1] + ind32[i-1] + ind33[i-1]
  }
 
 
#estimating first transition probability matrix
  
  m1<- floor(0.2*nsteps)

  alpha11<- sum(ind11[1:m1])/sum(ind1[1:m1])
  alpha12<- sum(ind12[1:m1])/sum(ind1[1:m1])
  alpha13<- sum(ind13[1:m1])/sum(ind1[1:m1])
  alpha21<- sum(ind21[1:m1])/sum(ind2[1:m1])
  alpha22<- sum(ind22[1:m1])/sum(ind2[1:m1])
  alpha23<- sum(ind23[1:m1])/sum(ind2[1:m1])
  alpha31<- sum(ind31[1:m1])/sum(ind3[1:m1])
  alpha32<- sum(ind32[1:m1])/sum(ind3[1:m1])
  alpha33<- sum(ind33[1:m1])/sum(ind3[1:m1])
  
  print(c(alpha11, alpha12, alpha13, alpha21, alpha22, alpha23,
          alpha31, alpha32, alpha33))
 
  # estimating second transition probability matrix
  
  m2<- floor(0.8*nsteps)+1
  end<- nsteps-1
  beta11<- sum(ind11[m2:end])/sum(ind1[m2:end])
  beta12<- sum(ind12[m2:end])/sum(ind1[m2:end])
  beta13<- sum(ind13[m2:end])/sum(ind1[m2:end])
  beta21<- sum(ind21[m2:end])/sum(ind2[m2:end])
  beta22<- sum(ind22[m2:end])/sum(ind2[m2:end])
  beta23<- sum(ind23[m2:end])/sum(ind2[m2:end])
  beta31<- sum(ind31[m2:end])/sum(ind3[m2:end])
  beta32<- sum(ind32[m2:end])/sum(ind3[m2:end])
  beta33<- sum(ind33[m2:end])/sum(ind3[m2:end])
  
  print(c(beta11, beta12, beta13, beta21, beta22, beta23,
          beta31, beta32, beta33))
  
  
#computing likelihood function
lnL<- c()
  
m1<- floor(0.2*nsteps)+1
m2<- floor(0.8*nsteps)
  
for (m in m1:(m2-1)) {
  n11<- sum(ind11[m1:m])
  n12<- sum(ind12[m1:m])
  n13<- sum(ind13[m1:m])
  n1<- n11+n12+n13
  
  n21<- sum(ind21[m1:m])
  n22<- sum(ind22[m1:m])
  n23<- sum(ind23[m1:m])
  n2<- n21+n22+n23
  
  n31<- sum(ind31[m1:m])
  n32<- sum(ind32[m1:m])
  n33<- sum(ind33[m1:m])
  n3<- n31+n32+n33
  
  ns11<- sum(ind11[(m+1):m2])
  ns12<- sum(ind12[(m+1):m2])
  ns13<- sum(ind13[(m+1):m2])
  ns1<- ns11+ns12+ns13
  
  ns21<- sum(ind21[(m+1):m2])
  ns22<- sum(ind22[(m+1):m2])
  ns23<- sum(ind23[(m+1):m2])
  ns2<- ns21+ns22+ns23
  
  ns31<- sum(ind31[(m+1):m2])
  ns32<- sum(ind32[(m+1):m2])
  ns33<- sum(ind33[(m+1):m2])
  ns3<- ns31+ns32+ns33

lnL[m-m1+1]<- log(factorial(n1))-log(factorial(n11))-log(factorial(n12)) -log(factorial(n13))+n11*log(alpha11) + n12*log(alpha12)+ n13*log(alpha13)+log(factorial(n2))-log(factorial(n21))-log(factorial(n22))-log(factorial(n23))+ n21*log(alpha21) + n22*log(alpha22)+ n23*log(alpha23) + log(factorial(n3))-log(factorial(n31))-log(factorial(n32))-log(factorial(n33))+n31*log(alpha31)+n32*log(alpha32) + n33*log(alpha33)+ log(factorial(ns1))-log(factorial(ns11))-log(factorial(ns12)) -log(factorial(ns13))+ns11*log(beta11) + ns12*log(beta12)+ ns13*log(beta13) +log(factorial(ns2))-log(factorial(ns21))-log(factorial(ns22))-log(factorial(ns23))+ ns21*log(beta21) + ns22*log(beta22) + ns23*log(beta23) + log(factorial(ns3))-log(factorial(ns31)) -log(factorial(ns32))-log(factorial(ns33))+ns31*log(beta31) +ns32*log(beta32) + ns33*log(beta33)
}

l<- length(lnL)
less<- c()

for (i in 1:(l-1)) {
   less[i]<- 0
   for (j in (i+1):l) {
  if(lnL[i] > lnL[j]) { less[i]<- less[i]+1}  
     else break
   }
}

plot(lnL, type="l")
  cp<- which.max(less[1:(l-1)])+m1

print(cp)
