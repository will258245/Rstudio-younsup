#통계학특강발표

#####정규분포

X<-seq(-3,3,length=200)
X2<-seq(-3,3,length=25)
A<-seq(0.01,0.99,length=100)
A2<-seq(0.01,0.99, length=1000)
A3<-seq(0.01,0.99,length=25)

#####mydnorm
plot(dnorm(X)) #내장함수
plot(dnorm(X), type = 'l')

mydnorm <- function(x, mean = 0, sd = 1) {
  (1 / (sd * sqrt(2*pi))) * exp( -((x - mean)^2 / 2*sd^2) )
}

cbind(dnorm=dnorm(X2), mydnorm=mydnorm(X2), 
      diff=(dnorm(X2) - mydnorm(X2)))
plot(mydnorm(X))#확인



#####mypnorm
plot(pnorm(X)) #내장함수

library(pracma)

mypnorm <- function(x, mean = 0, sd = 1) {
  (1/2)*(1 + erf((x-mean)/(sd*sqrt(2))))
}

cbind(pnorm=pnorm(X2),mypnorm=mypnorm(X2),
      diff=(pnorm(X2) - mypnorm(X2)))

plot(pnorm(X)) #내장함수
plot(mypnorm(X))#확인



#####myqnorm
qnorm(.55)
plot(qnorm(A)) #내장함수

#Newton-Rapson Method
myqnorm1 <- function(p, mean = 0, sd = 1, tol = 1e-8, max_iter = 100) {
  
  x <- mean
  iter <- 0
  diff <- Inf
  
  while (diff > tol && iter < max_iter) {
    iter <- iter + 1 #너무 많은 계산을 피하기 위해 max_iter를 설정
    old_x <- x
    cdf <- mypnorm(x, mean, sd)
    pdf <- mydnorm(x, mean, sd)
    x <- x - (cdf - p) / pdf #Newton-Raphson method
    #Newton-Raphson method는 근을 찾는 방법이다.
    #원하는 p값을 구하기 위해 f(x) = p - cdf 로 둔다.
    # cdf를 미분하면 pdf
    diff <- abs(x - old_x)
  }
  return(x)
}

cbind(qnorm=qnorm(A3), myqnorm=myqnorm1(A3),
      diff=(qnorm(A3)-myqnorm1(A3)))
plot(qnorm(A))
plot(myqnorm1(A))


#내장함수 이용
myqnorm2 <- function(p, mean = 0, sd = 1) {
  p = mean + sd * sqrt(2) * erfinv(2 * p - 1)
  p
}

cbind(qnorm=qnorm(A3), myqnorm=myqnorm2(A3),
      diff=(qnorm(A3)-myqnorm2(A3)))
plot(myqnorm2(A))


#Bisection Method
library(stats)
#Bisection Method
myqnorm3 <- function(p, mean = 0, sd = 1, tol = 1e-8){
  
  n <- rep(0,length = length(p))
  
  for (i in 1:length(p)) {
    f <- function(x) mypnorm(x, mean = mean, sd = sd) - p[i]
    a <- -4*sd
    b <- 4*sd
    while ((b - a) > tol) {
      c <- (a+b) / 2
      if (f(a) * f(c) < 0) {
        b <- c
      } else {
        a <- c
      }
    }
    n[i] <- (a + b) / 2
    if (p[i] == .5){
      n[i] = 0 #p=0.5 일 때 오류발생 수정
    }
  }
  return(n)
}


cbind(qnorm=qnorm(A3), myqnorm=myqnorm3(A3),
      diff=(qnorm(A3)-myqnorm3(A3)))
plot(myqnorm3(A))

#3개 다 확인
plot(qnorm(A), type = 'l') #내장함수
plot(myqnorm1(A), type = 'l')
plot(myqnorm2(A), type = 'l')
plot(myqnorm3(A), type = 'l')

#뭐가 더 빠르게 계산하는가?
system.time(myqnorm1(A2))
system.time(myqnorm2(A2))
system.time(myqnorm3(A2))

# myqnorm1, myqnorm2는 크게 차이가 없고, myqnorm3은 비교적 느리다.








#####myrnorm

hist(rnorm(1000, mean = 5, sd = 2))

#중심극한정리를 이용
myrnorm1 <- function(n, mean = 0, sd = 1) {
  random_samples <- numeric(n)
  for (i in 1:n) {
    successes = 0
    size = 500
    for (j in 1:size) {
      if (runif(1) <= 0.5) {
        successes <- successes + 1
      }
    }
    random_samples[i] <- successes
  }
  Z = (random_samples - (size * 0.5)) / sqrt(size * 0.5*0.5)
  R = (Z * sd) + mean
  return(R)
}

#확인
myrnorm1(10)
hist(myrnorm1(1000), probability = T)
x = seq(-4,4, length.out = 100)
y = dnorm(x)
lines(x, y)



#CDF의 역함수 이용 (Inverse Transform Method)
myrnorm2 <- function(n, mean = 0, sd = 1) {
  p <- runif(n)
  x <- myqnorm2(p,mean,sd)
  return(x)
}

#확인
myrnorm2(10)
hist(myrnorm2(1000), probability = T)
x = seq(-4,4, length.out = 100)
y = dnorm(x)
lines(x, y)



#Acceptance-rejection method
myrnorm3<-function(n,mean = 0,sd = 1){
  k <- 0 #counter for accepted
  j <- 0 #iterations
  y <- numeric(n)
  while (k < n) {
    u <- runif(1,0,0.4)
    j <- j + 1
    x <- runif(1, -4*sd, 4*sd)
    if (mydnorm(x, mean=mean, sd=sd) > u) {
      #we accept x
      k <- k + 1
      y[k] <- x
    }
  }
  return(y)
}

#확인
myrnorm3(10)
hist(myrnorm3(1000), probability = T)
x = seq(-4,4, length.out = 100)
y = dnorm(x)
lines(x, y)


#3개 다 확인
x = seq(-4,4, length.out = 500)
y = dnorm(x)

hist(rnorm(1000), probability = T)#내장함수
lines(x, y)
hist(myrnorm1(1000), probability = T)
lines(x, y)
hist(myrnorm2(1000), probability = T)
lines(x, y)
hist(myrnorm3(1000), probability = T)
lines(x, y)


#뭐가 더 빠르게 계산하는가?
system.time(myrnorm1(1000))
system.time(myrnorm2(1000))
system.time(myrnorm3(1000))

#myrnorm1이 비교적 느리고 myrnorm2, myrnorm3은 큰 차이가 없다.