#Cartegorical Data Analysis
#lab1

#binomail distribution
# Y ~ B(n=10, pi=0.2)

dbinom(0:10, 10, 0.2) ## P(Y=0), P(Y=1),...,P(Y=10)

plot(0:10, dnbinom(0:10, 10, 0.2))
plot(0:10, dnbinom(0:10, 10, 0.2), 
     type = "h", xlab = "y", ylab = "P(y)")   
# x축은 관찰 횟수, y축은 그 때의 확률


?dbinom

#Likelihood
# statistical inference problem  : on observing Y=y, 
# what can we say about pi? 
'''
Definition: Assuming a statistical model with parameter π, the
likelihood is the probability of observed Y = y considered as a
function of π 
최대우도법은 어떤 확률변수에서 표집한 값들을 토대로 그 확률변수의 모수를 구하는 방법이다. 
어떤 모수가 주어졌을 때, 원하는 값들이 나올 가능도를 최대로 만드는 모수를 선택하는 방법이다. 
'''


y<-8 ; n<-10
#seq(0,1, len=100)
theta<- seq(0,1,len=100)    # pi 값을 0에서 부터 1까지 100개로 쪼개기 
like<- dbinom(y, n, theta)  #
like<- like/max(like)       #최댓값을 1로 scaling


plot(theta,like,type='l',xlab=expression(theta),
     ylab='Likelihood', main="Likelihood function when y=8")


y<-0; n<-10
theta<- seq(0,1,len=100)
like <- dbinom(y,n,theta)
like <- like/max(like)
plot(theta,like,type='l',xlab=expression(theta),
     ylab='Likelihood', main="Likelihood function when y=0")


par(mfrow=c(1,1))
x<-2; n<-10
theta<- seq(0.0,1,len=100)
like <- dbinom(x,10,theta)
like<- like/max(like)
plot(theta,like,type='n',xlab=expression(theta),
     ylab='Likelihood')
lines(theta,like,lwd=.3)                   #10번 시행 중 2번 관측. 모수를 추정하기 위해 여러 확률변수 로 계산.
title(expression('Likelihood functions'))

xx<- c(0,5,10)
for (i in 1:3){
  x<- xx[i]
  like <- dbinom(x,10,theta)
  like<- like/max(like)
  lines(theta,like,lwd=.3)
}

#0번, 2번, 5번, 10번 했을 경우 


#prop.test computes score test and CI
x<-0; n<-10
prop.test(x, n, correct=FALSE)

#Various methods
library(prevalence)
propCI(x=x, n=n)

#LRT
th<- seq(0,1,len=100)
x<- 0
n<- 10
like <- dbinom(x,n,th)
like<- like/max(like)
a<- exp(-0.5*qchisq(0.95,1))
plot(th,like,xlab=expression(theta),
     ylab='Likelihood',type='l')
abline(h=a,lwd=.3)
cat('cutoff=',a, 'Interval=',li(th,like,a),'\n')
## cutoff= 0.1465001 Interval= 0 0.1748713
title(expression('Likelihood intervals'))



