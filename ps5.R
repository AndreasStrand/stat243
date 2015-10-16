# ps5 stat243

######## Problem 1 ########

# Sum of 1 + 1e-16 +... + 1e-16 with sum()
x = c(1, rep(1e-16, 1e4))
xSum = sum(x) # 1.000000000001

# Sum of 1 + 1e-16 +... + 1e-16 as a for loop
bigFirst = 0
for(i in 1:length(x)) {
  bigFirst = bigFirst + x[i]
}
sprintf("%.20f",bigFirst) # 1.00....0

# Sum of 1e-16 +... + 1e-16 + 1 as a for loop
smallFirst = 0
for(i in length(x):1) {
  smallFirst = smallFirst + x[i]
}
sprintf("%.20f",smallFirst) # 1.00000000000100008890


######## Problem 2 ########

# Initializing a numeric vector and an integer vector in order to do timing of
# operations on these
bf = rep(2, 1e8)
bi = as.integer(rep(2, 1e8))

# Doing vectorized divisions
divFtime = unname(system.time(cf <- bf/2)[3]) # 0.64
divItime = unname(system.time(cFromI <- bi/2)[3]) # 11.11

# Creating subsets of the vectors
subsFtime = unname(system.time(df <- bf[seq(1, length(bf), 50)])[3]) # 0.14
subsItime = unname(system.time(di <- as.integer(bf[seq(1, length(bi), 50)]))[3]) # 0.12

# Changing some elements in the vectors
changeFtime = unname(system.time(df[seq(1, length(df), 5)] <- 3)[3]) # 0.02
changeItime = unname(system.time(di[seq(1, length(di), 5)] <- as.integer(3))[3]) # 0.03

# Adding vectors together
addFtime = unname(system.time(ef <- bf + bf)[3]) # 14.04
addItime = unname(system.time(ei <- bi + bi)[3]) # 4.64

# Scalar multiplied with vector
multFtime = unname(system.time(ff <- 10*bf)[3]) # 0.33
multItime = unname(system.time(fi <- as.integer(10*bi))[3]) # 8.02
