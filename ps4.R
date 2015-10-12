# ps4, stat243

######## Problem 1 ########
# Modified code where tmp create a
# random number using the saved random seed index
set.seed(0)
runif(1)
save(.Random.seed, file = 'tmp.Rda')
runif(1)
load('tmp.Rda')
runif(1)
tmp <- function(){
  # Now load is modified to load tmp.Rda into the function
  # environment instead of the global environment
  load('tmp.Rda', envir = environment(tmp))
  runif(1)
}
tmp()

######## Problem 2 ########

#### Creating a function that computes the log of f
logF <- function(k, p=0.3, th=0.5, N=100) {
  # When taking the log  of f we need to consider special cases,
  # (0)*log(0) returns NaN but we want it to be 0. 
  if ((n-k) == 0){
    term = 0
  } else {
    term = (n-k)*log(n-k)
  } 
  return(log(choose(n,k)) + (th-1)*(n*log(n)-k*log(k)-term)+
    k*th*log(p) + (n-k)*th*log(1-p))
}

#### Calculating the denominator of P(Y=y)

# Using apply (The code is in a function in order to check timing later.)
denomApply <- function(tmp){
  assign("n", tmp, envir = .GlobalEnv)
  sum(exp(apply(matrix(1:n), 1, logF, N=n)))
}

# Using full vectorization
p=0.3; th=0.5  
denomVector <- function(n){
  k = c(1:n)
  # Using ifelse to avoid NaNs
  sum(exp(log(choose(n,k)) + (th-1)*(n*log(n)-k*log(k) -
   ifelse(n-k, (n-k)*log(n-k), 0))+k*th*log(p) + (n-k)*th*log(1-p)))
}

#### Comparing timing
times <- function(r){
  #assign("n", r, envir = .GlobalEnv)
  c(system.time(denomApply(r))[3],system.time(denomVector(r))[3])
}

range = matrix(seq(10, 2000, by = 10))
timeMat = apply(range, 1, times)
rownames(timeMat) = c("apply", "vector")
colnames(timeMat) = range


######## Problem 3 ########
setwd("C:/Users/Andreas/Documents/stat243")
load("C:/Users/Andreas/Downloads/mixedMember.Rda")

# Calculating the means of the normal distributions in group A and group B with timing
aApplyTime = unname(system.time(aMeans <- sapply(1:length(muA), function(i) sum(wgtsA[[i]]*muA[IDsA[[i]]])))[3])
bApplyTime = unname(system.time(bMeans <- sapply(1:length(muB), function(i) sum(wgtsB[[i]]*muB[IDsB[[i]]])))[3])

# Creating data frames
Ka = 1000
Kb = 10

aMax = max(sapply(1:Ka, function(x) length(IDsA[[x]])))
bMax = max(sapply(1:Kb, function(x) length(IDsB[[x]])))

aData = data.frame(t(sapply(1:Ka, function(x){
  c(muA[IDsA[[x]]], rep(0, aMax-length(IDsA[[x]])),
    wgtsA[[x]], rep(0, aMax-length(IDsA[[x]])))})))
bData = data.frame(t(sapply(1:Kb, function(x){
  c(muB[IDsB[[x]]], rep(0, bMax-length(IDsB[[x]])),
    wgtsB[[x]], rep(0, bMax-length(IDsB[[x]])))})))

# Vectorized computation of the means with timing for each data set
start <- proc.time()
aVec <- 1:Ka
aSums <- rowSums(aData[aVec, 1:aMax]*aData[aVec, (aMax+1):(2*aMax)])
aVecTime<- unname((proc.time() - start)[3])

start <- proc.time()
bVec <- 1:Kb
bSums <- rowSums(bData[bVec, 1:bMax]*bData[bVec, (bMax+1):(2*bMax)])
bVecTime<- unname((proc.time() - start)[3])

