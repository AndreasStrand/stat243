################
## Problem 1b ##
################
library(MASS)
library(stats)
library(mvtnorm)

# The parameters are named as in the answer to part 1a
# note: data[,,1] corresponds to outcomes

# Chosen values
  s = 1000
  n = 500
  m = 100
  data = array(0,dim=c(s,n,m))
  t = c(rep("normal",s/2),rep("cauchy",s/2))
  range = c(-100, 100)

# Lopp over all sets
  for(S in 1:s){
    # Creating distribution parameters randomly
    beta = runif(m,range[1],range[2])
    sigma = apply(matrix(1:m), 1, function(x){
      c(runif(x,range[1],range[2]), rep(0,m-x))})
    sigma = sigma*t(sigma)
  
    # Random draws from distribution
    if (t[S]=="normal"){
      data[S,,] = mvrnorm(n, beta, sigma)
    }else{ # cauchy
      data[S,,] = rmvt(n, sigma, delta=beta, df=1)
    }
    
    # Creating outliers
    outliers = sample(c(0,1), 100, replace=T, prob=c(.99,.01))
    data[S,outliers,1] = runif(sum(outliers),2,4)*sd(beta)    
  }
  data # The generated dataset

################
## Problem 2 ##
################
## Parameter specicfication 
  lLim = 10
  rLim = 2
  m = 10000
  h1 = seq(rLim,lLim, l=m)
  h2 = h1^2

## Functions specified by the problem
  # Pareto cdf
  dpar = function(x){
    # parameters: alfa=2, beta=3
    return(24*x^(-4))
  }
  
  # Exponential cdf
  dexp = function(x){
    # parameter=1, right shift of 2
    return(exp(2-x))
  }
  
  # Weights
  weights = function(h, f = "exp"){
    if (f == "exp"){
      return(dpar(h)/dexp(h))
    }else{
      return(dexp(h)/dpar(h))
    }
      
  }
  
  # Expectation
  expect = function(h, f = "exp"){
    return(sum(h*weights(h,f))/(length(h)-1))
  }

## Computations part a
  # Expectation estimates
  aEX = expect(h1)
  aEX2 = expect(h2)

  # Histograms
  hist(weights(h1), breaks=seq(0,ceiling(max(weights(h1))),l=20),
       main = "Histogram of weights (f is Exponential)", las=1, xlab="weight")
  hist(h1*weights(h1), breaks=seq(0,ceiling(max(h1*weights(h1))),l=20),
       main = "Histogram of h*weights (f is Exponential)", las=1, xlab="h*weight")

## Computations part b
  # Expectation estimates
  bEX = expect(h1, f= "par")
  bEX2 = expect(h2, f= "par")

  # Histograms
  hist(weights(h1, f="par"),breaks=seq(0,ceiling(max(weights(h1, f="par"))),l=20),
       main = "Histogram of weights (f is Pareto)", las=1, xlab="weight")
  hist(h1*weights(h1, f="par"),breaks=seq(0,ceiling(max(h1*weights(h1, f="par"))),
       l=20),main = "Histogram of h*weights (f is Pareto)",las=1,xlab="h*weight")


################
## Problem 3 ##
################

design = function(n,p,lower,upper){
  X = matrix(rep(1,n*p), ncol=p)
  for(col in 2:p){
    X[,col]= runif(n,lower,upper)
  }
  return(X)
}
test = function(){
  set.seed(200)
  beta0 = 0;beta1 = 0;beta2 = 0;beta3 = 0
  beta0t = seq(0,1,by=0.1)
  beta1t = beta0t
  n = 100
  best = 300
  y = rep(0,n)
  X = design(n, 4, -5, 5)
  for (i in 1:length(beta0t)){
    for (j in 1:length(beta1t)){
      beta = c(beta0t[i],beta1t[j], beta2, beta3)
      y = rbinom(n, 1, prob=pnorm(X%*%beta))
      data = data.frame(y,X)
      reg = summary(lm(y~X2+X3+X4,data))$coeff
      if(abs((reg[2,1]/reg[2,2])-2)< best){
        best = abs((reg[2,1]/reg[2,2])-2)
        beta0 = beta0t[i]
        beta1 = beta1t[j]
      }
    }
  }
  return(c(beta0,beta1,beta2,beta3))
}
test() # Proposed starting coefficients

# Function doing EM
em = function(X,y,test,tol=sqrt(.Machine$double.eps), maxI = 2e3){
  set.seed(300)
  change=Inf
  beta = test
  i = 0
  while(change>tol & i<maxI){
    i=i+1
    prev = beta
    mu = X%*%beta
    z = ifelse(y==1, mu+dnorm(mu,0,1)/pnorm(mu,0,1),mu-dnorm(mu,0,1)/pnorm(-mu,0,1))
    beta=unname(lm(z~X2+X3+X4,data.frame(z,X))$coeff)
    change=sum(abs(prev-beta))/sum(abs(prev))
  } 
  out = c(i,beta); names(out)=c("Iterations", "beta0", "beta1", "beta2", "beta3")
  return(out)
}

## Test of the EM algorithm
n = 100
p = 4
test = test()
X = cbind(1, matrix(rep(rnorm(n*(p-1),0,1)),ncol=p-1))
y = rbinom(n, size = 1, prob = pnorm(X%*%test))
em(X,y,test)


## Direct maximization of log-likelyhood
dirLikelihood =function(par, X, y) {
  npdf = pnorm(X%*%par, 0, 1, lower.tail = T, log.p = F)
  return(sum(y*log(npdf)+(1-y)*log(1-npdf)))
}

first = as.double(lm(y~X2+X3+X4,data.frame(y,X))$coeff)
out = optim(first, fn=dirLikelihood, gr=NULL, y=y, X=X, method="BFGS",
            control=list(trace=T,maxit=2e3,fnscale=-1), hessian=T)
out$par     # estimates
out$counts  # iterations
sqrt((-1)*diag(solve(out$hessian))) # se of estimates

################
## Problem 4 ##
################
library(fields)

## Helical valley function
hvf <- function(x) {
  a <- 10*(x[3] - (5/pi)*atan2(x[2],x[1]))
  b <- 10*(sqrt(x[1]^2+x[2]^2)-1)
  return(a^2+b^2+x[3]^2)
}

## Surface plots
testx3 = seq(-1,1,0.5)
for(i in testx3){
  x1 <- seq(-1, 1, length.out=50)
  x2 <- seq(-1, 1, length.out=50)
  z <- apply(as.matrix(expand.grid(x1, x2)), 1, function(x) hvf(c(x, i)))
  image(x1, x2, matrix(z, 50, 50), col = terrain.colors(100),
        axes = FALSE, main=paste("HVF with x3 =",i))
  contour(x1, x2, matrix(z, 50, 50),add = TRUE)
}

## Extrema by starting point
optim(c(-1,1,2), hvf)$value
optim(c(1,2,3), hvf)$value
nlm(hvf, c(-1,1,2))$minimum
nlm(hvf, c(1,2,3))$minimum





