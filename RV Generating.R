#3.1
#동전던지기
sample(0:1, size = 10, replace = T)
#로또번호 생성기
sample(1:64, size = 6, replace = F)
#알파벳 생성기
sample(letters)
#다항분포에서 샘플링
x<- sample(1:3, size = 100, replace = TRUE, prob = c(.2,.3,.5))
table(x)

#3.2 역함수 방법으로
#3x^2를 따르는 분포 만들기
n <- 10000
u <- runif(n)
x <- u^(1/3)
hist(x, probability = T, main = expression(f(x) == 3*x^2))
y<-seq(0,1,.01)
lines(y, 3*y^2)

#지수분포 generating 하기
n<-10000
u<-runif(n)
lambda <- 5
x = -log(u)/lambda
hist(x, probability = T)

#이산형의 경우
#Two point distribution 중 하나인
#베르누이 확률변수
n<-1000
p<-0.4
u<-runif(n)
x<-as.integer(u<p) #TRUE의 경우 1, FALSE의 경우 0으로 바꿔라
mean(x)# = p
var(x) # = pq
hist(x)

rbinom(n,size = 1, prob = p)
sample(c(0,1), size = n, replace = T, prob = c(.6,.4))


#기하분포
n<-10000
p<-.25
q<-1-0.25
u<-runif(n)
k<-ceiling(log(1-u) / log(q))-1
hist(k)
k2<-floor(log(u) / log(q))
hist(k2)

#포아송분포
poi = rpois(n,5)
hist(poi)

#logarithmic distribution
rlogarithmic <- function(n, theta) {
  #returns a random logarithmic(theta) sample size n
  u<- runif(n)
  #set the initial length of cdf vector
  N<- ceiling(-16 / log10(theta))
  k<- 1:N
  a<- -1/log(1-theta)
  fk<- exp(log(a) + k * log(theta) - log(k))
  Fk<- cumsum(fk)
  x<- integer(n)
  for (i in 1:n) {
    x[i] <- as.integer(sum(u[i]>Fk)) #Fˆ{-1}(u)-1
    while (x[i] == N) {
      #if x==N we need to extend the cdf
      #very unlikely because N is large
      logf <- log(a) + (N+1)*log(theta) - log(N+1)
      fk<- c(fk, exp(logf))
      Fk<- c(Fk, Fk[N] + fk[N+1])
      N<- N+1
      x[i] <- as.integer(sum(u[i] > Fk))
    }
  }
  x+1
}

ceiling(-16 / log10(0.4))

n<-10000
thetaa<- 0.5
x<- rlogarithmic(n, thetaa)
#compute density of logarithmic(theta) for comparison
hist(x)
p<- -1 / log(1-thetaa) * thetaa^k/k # 진짜 로가디믹 분포의 값
se<- sqrt(p*(1-p)/n) #standard error

round(rbind(table(x)/n, p, se),3)# 반올림

unique(x)


#3.3 The Acceptance-Rejection Method
#승인-기각 방법으로 generating
n<- 1000
k<- 0#승인된 카운터
j<- 0#반복
y<- numeric(n)
while (k < n) {
  u<- runif(1)
  j<- j+1
  x<- runif(1) #random variate from g
  if (2 * (x * (1-x)) > u){
    #we accept x
    k<- k+1
    y[k]<- x
  }
}
j
k
y
hist(y, probability = T)
o<-seq(0,1,length = 100)
lines(o, 6 * o *(1-o))

#경험적인 퍼센트 vs 이론적인 퍼센트
p<- seq(.1,.9,.1)
Qhat<- quantile(y,p)
Q<- qbeta(p,2,2)#quantiles of sample
se<- sqrt(p * (1-p) / (n * dbeta(Q,2,2)^2))#theoretical quantiles
round(rbind(Qhat, Q, se), 3)

#Transformation Methods
#Beta distribution(베타분포)
n<-1000
a<-3
b<-2
u<- rgamma(n, shape = a, rate = 1)
v<- rgamma(n, shape = b, rate = 1)
x<- u / (u + v)
hist(x)

q<- qbeta(ppoints(n), a, b)
qqplot(q, x, cex=0.25, xlab = "Beta(3,2)", ylab = "Sample")
abline(0,1)

#Logarthmic dist. , more efficient generator
n<- 1000
theta <- .5
u<- runif(n)#generate logarithmic sample
v<- runif(n)
x<- floor(1 + log(v) / log(1-(1-theta)^u))
hist(x)
k<- 1:max(x)#calc. logarithmic probs.
p<- -1/log(1 - theta) * theta^k / k
se<- sqrt(p*(1-p)/n)
p.hat<- tabulate(x)/n
#확인
round(rbind(p.hat, p, se), 3)
#옛날에 만들었던거 efficient하게 바꾼 버전
rlogarithmic<- function(n, theta){
  stopifnot(all(theta>0 & theta <1))
  th<- rep(theta, length = n)
  u<-runif(n)
  v<-runif(n)
  x<- floor(1 + log(v) / log(1-(1-th)^u))
  return(x)
}

#Chisquare
n<-1000
nu<- 2
X<- matrix(rnorm(n*nu), n, nu)^2 #matrix of sq. normals
Y<- rowSums(X)
y<- apply(X, MARGIN = 1, FUN = sum)
mean(y)
mean(y^2)

#Convolutions and Mixtures
n<- 1000
x1<- rgamma(n,2,2)
x2<- rgamma(n,2,4)
s<- x1 + x2 #Convolutions
u<- runif(n)
k<- as.integer(u > 0.5)
x<- k * x1 + (1-k) * x2 #Mixture
par(mfcol = c(1,2))
hist(s, probability = T, xlim = c(0,5), ylim = c(0,1))
hist(x, probability = T, xlim = c(0,5), ylim = c(0,1))

#Mixture of several gamma dist.
par(mfcol = c(1,1))
n<- 5000
k<- sample(1:5, size = n, replace = T, prob = (1:5)/15)
rate<- 1/k
x<- rgamma(n, shape = 3, rate = rate)
plot(density(x), xlim = c(0,40), ylim=c(0,.3), lwd=3, xlab = "x", main = "")
for (i in 1:5) {
  lines(density(rgamma(n,3,1/i)))
}

#plot density of mixture
f<- function(x, lambda, theta){
  #density of the mixture at the point x
  sum(dgamma(x,3,lambda) * theta)
}
x<- seq(0,8,length=200)
dim(x) <- length(x)
lambda<- c(1,1.5,2,2.5,3)
p=c(.1,.2,.2,.3,.2)
y<- apply(x,1,f,lambda=lambda, theta=p)

plot(x,y,type = "l", ylim = c(0,.85), lwd=3, ylab = "Density")
for (j in 1:5) {
  y<- apply(x, 1, dgamma, shape=3, rate=lambda[j])
  lines(x,y)
}

#Poisson-Gamma mixture, continuous mixture
n<-1000; r<-4; beta<-3
lambda <- rgamma(n,r,beta)
x<-rpois(n, lambda)
mix<-tabulate(x+1)/n
negbin<-round(dnbinom(0:max(x), r, beta/(1+beta)),3)
se<- sqrt(negbin * (1-negbin)/n)
round(rbind(mix,negbin,se),3)

#Multivariate Normal Dist.
Z<- matrix(rnorm(n*d), nrow = n, ncol = d)
X<- Z %*% Q + matrix(mu,n,d,byrow = T)

#Spectral decomposition method for generating N_d samples
mu<-c(0,0)
Sigma<- matrix(c(1,.9,.9,1), nrow = 2, ncol = 2)
rmvn.eigen<- function(n, mu, Sigma) {
  d<- length(mu)
  ev<- eigen(Sigma, symmetric = T)
  lambda<- ev$values
  V<- ev$vectors
  R<- V %*% diag(sqrt(lambda)) %*% t(V)
  Z<- matrix(rnorm(n*d), nrow = n, ncol = d)
  X<- (Z %*% R) + matrix(mu, n, d, byrow = T)
  X
}
#generate the sample
X<- rmvn.eigen(1000, mu, Sigma)
plot(X, xlab = "x", ylab = "y", pch = 20)

#SVD Method of generating N_d samples
rmvn.svd<- function(n, mu, Sigma){
  d<- length(mu)
  S<- svd(Sigma)
  R<- S$u %*% diag(sqrt(S$d))
  Z<- matrix(rnorm(n*d), nrow = n, ncol = d)
  X<- Z %*% R + matrix(mu, n, d, byrow = T)
  X
}

#Choleski factorization method
rmvn.Choleski<- function(n, mu, Sigma){
  d<- length(mu)
  Q<- chol(Sigma)
  Z<- matrix(rnorm(n*d), nrow = n, ncol = d)
  X<- Z %*% Q + matrix(mu, n, d, byrow = T)
  X
}

y<- subset(x=iris, Species == "virginica")[, 1:4]
mu<- colMeans(y)
Sigma<- cov(y)
mu

Sigma

X<- rmvn.Choleski(200, mu, Sigma)
pairs(X)

#Comparing Performance of Generators
library(MASS); library(mvtnorm)
n<- 100
N<-2000
d=30
mu<- numeric(d)
#안됨ㅋㅋ

runif.sphere<- function(n, d){
  M<- matrix(rnorm(n*d), nrow = n, ncol = d)
  L<- apply(M, MARGIN = 1,
            FUN = function(x){sqrt(sum(x*x))})
  D<- diag(1/L)
  U<- D %*% M
  U
}

#Mixtures of Multivariate Normals
library(MASS)
loc.mix.0<- function(n,p,mu1,mu2,Sigma){
  X<- matrix(0,n,2)
  for (i in 1:n) {
    k<- rbinom(1, size = 1, prob = p)
    if(k)
      X[i,]<- mvrnorm(1, mu = mu1, Sigma) else
        X[i,]<- mvrnorm(1,mu=mu2, Sigma)
  }
}

loc.mix<- function(n,p,mu1,mu2,Sigma) {
  n1<- rbinom(1,size = n, prob = p); n2<- n - n1
  x1<- mvrnorm(n1, mu= mu1, Sigma)
  x2<- mvrnorm(n2, mu=mu2, Sigma)
  X<- rbind(x1,x2)
  return(X[sample(1:n),])
}



#Generating variate on a sphere
X<- runif.sphere(200,2)
par(pty="s")
plot(X, xlab = bquote(x[1]), ylab = bquote(x[2]))
par(pty = "m")
plot(X, xlab = bquote(x[1]), ylab = bquote(x[2]))

