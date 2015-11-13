#ps7

######## Problem 2 ########
setwd("C:/Users/Andreas/Documents/stat243")
library(pryr)
set.seed(0)

## Randomly generating a symmetric positive deminite matrix
spd = function(n=100,maxmin = 100){
  # Create nxn matrix with numeric values in {-maxmin, maxmin}
  X = matrix(runif(n^2, -maxmin, maxmin), ncol=n)
  # Make it symmetric positive definite
  return(X%*%t(X))
}
## Looking at memory usage in the Cholesky decomposition
cholMem = function(msize){
  X = spd(msize)
  memBefore = mem_used()
  C = chol(X)
  memAfter = mem_used()-memBefore
  return(memAfter)
}
cholMem(2000) # (32 Mb, 64 Mb)

## Memory usage and processing time by n
nvals = seq(1200,3000,200)
tandm=matrix(rep(0,2*length(nvals)), nrow=2)

# unname(system.time()[3]) returned elapsed time as numeric
tandm = function(){
  return(unlist(sapply(X=nvals, function(X){
  time = unname(system.time(mem <- cholMem(X))[3])
  return(c(time,mem))
  })))
}
r = 10 # number of replications
A = replicate(r, tandm())

# Gathering maxima
memMax = rep(0,length(nvals))
runMax = rep(0,length(nvals))
for (i in 1:length(nvals)){
  runMax[i]=max(A[1,i,])
  memMax[i]=max(A[2,i,])
}

## Plotting the result
# Create some extra space to the right
par(mar= c(5, 4, 4, 6) + 0.1)

# Plot the run times
plot(nvals, runMax, pch=16, axes=FALSE, xlab="", ylab="", new=TRUE,
     type="b",col="black", main="The Cholesky decomposition")
axis(2, ylim=c(0,max(runMax)),col="black",las=1)
mtext("Processing time (sec)",side=2,line=2.5)

# Create a box around the current graphics and allow for more drawing
box()
par(new=TRUE)

# Plot the memory use with axes on the right
plot(nvals, memMax, pch=15,  xlab="", ylab="", 
     axes=FALSE, type="b", col="red3")
axis(4, ylim=c(min(memMax),max(memMax)), col="red3",col.axis="red3",las=1)
mtext("Memory use (bytes)",side=4,col="red2",line=4) 

# Create the first axis and legens
axis(1,xlim=c(min(nvals),max(nvals)))
mtext("Height and width of the square matrix",side=1,col="black",line=2.5)  
legend("topleft",legend=c("Processing time","Memory usage"),
       text.col=c("black","red3"),pch=c(16,15),col=c("black","red3"))

######## Problem 3 #########
set.seed(314)
# Creating a function that returns the run time of 3 methods
  # of solving a dense linear system
triSolve = function(N){
  # Creating a random NxN matrix and Nx1 vector 
  X = spd(N, maxmin = 1000)
  y = matrix(runif(N, -1000, 1000), ncol=1)
    # note: unname(proc.time()[3]) return elapsed time as numeric
  
  # 1. Inverse
  start = proc.time()
  Xinv = solve(X)
  b1 = Xinv%*%y
  t1 = unname((proc.time()-start)[3])
  
  # 2. Solve augmented matrix
  t2 = unname(system.time(b2 <- solve(X,y))[3])
  
  # 3. Cholesky
  start = proc.time()
  C = chol(X)
  d = forwardsolve(t(C),y)
  b3 = backsolve(C,d)
  t3 = unname((proc.time()-start)[3])
  
  # Calculating the differences in the inf-norm between solutions
  d1 = max(abs(b2-b1))
  d2 = max(abs(b3-b1))
  d3 = max(abs(b3-b2))
  
  return(matrix(c(t1,t2,t3,d1,d2,d3),nrow=3))
}

triSolve(1000)
## Comparing methods of solving linear system
# Doing r replications and taking extracting max values
r=10
B=replicate(r, triSolve(5000)) # Approx half an hour
# Creating a summary using inf. norm
timings = rep(0,3)
diffs = rep(0,3)
for (i in 1:3){
  timings[i]=max(B[i,1,])
  diffs[i]=max(B[i,2,])
}
# Summary
timings
  # [1] 116.38  24.37  21.65
diffs
  # [1] 1.723066e-13 1.696592e-06 1.696592e-06


######## Problem 4 #########
# Y = X*beta + eps
# gls: betahat = inv(t(X)inv(S)X)t(X)inv(S)Y
# X is nxp, omega is nxn

gls = function(X,y,omega){
  C = chol(omega,pivot=T)
  d = forwardsolve(t(C),X, upper.tri=F)
  SinvX = backsolve(C,d, upper.tri=T)
  d = forwardsolve(t(C),y, upper.tri=F)
  Sinvy = backsolve(C,d, upper.tri=T)
  return(solve(t(X)%*%SinvX,t(X)%*%Sinvy)) 
}

