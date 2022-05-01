# --------------------------------------
# Visualization of Exponential Distribution
# Keigo Tanabe (tamu)
# --------------------------------------

# Purpose 
    # show exp dist under different lambda
    # show an example of MLE


# ------
# exponential distribution
# ------

df.fy <- data.frame(y = seq(-1,6,by=0.1));  head(df.fy)

# density of exp under diff lambda.  (lambda = 0.5, 1.5, 3)
# add them as columns
df.fy$fy <- 0.5*exp(-0.5*df.fy$y)  # lambda = 0.5 <-- the coded operation is the PDF on the pset0
df.fy$fy2 <- 1.5*exp(-1.5*df.fy$y) # lambda = 1.5
df.fy$fy3 <- 3*exp(-3*df.fy$y)     # lambda =  3


# PDF = 0 when y < 0 
df.fy$fy[df.fy$y<0] <- 0
df.fy$fy2[df.fy$y<0] <- 0
df.fy$fy3[df.fy$y<0] <- 0



# Plot
  par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  
  plot(x = df.fy$y, y = df.fy$fy, type="l",  ylim=c(0,3), main = "lambda = 0.5", xlab = "y",ylab="density")
  plot(x = df.fy$y, y = df.fy$fy2, type="l", ylim=c(0,3), main = "lambda = 1.5", xlab = "y",ylab="")
  plot(x = df.fy$y, y = df.fy$fy3, type="l", ylim=c(0,3), main = "lambda = 3", xlab = "y",ylab="")



# ------
# generate data
# ------
N <- 10000  # number of observations
lambda <- 2 #  lambda = 2
data <- y <- rexp(N, rate = lambda)  # generate data
c.lambda <- seq(0.1, 5, by = 0.1) # a vector of lambda
hist(data, breaks=50,xlab ="",main="",ylim=c(0,2000),xlim=c(0,6))


# ------
# MLE
# ------

# we know is sampled from exponential dist 
# with lambda = 2. Let's see if we can 
# recover the value of lambda using MLE.
# You can repeat the same exercise using other 
# values of lambda (see the bottom of this file)


# Save values of lambda and 
# corresponding log-likelihood (LL) here
c.lambda <- seq(0.1, 5, by = 0.1)
df.L <- data.frame(lambda = c.lambda, LL = NA)

# compute LL for each lambda
for(i in 1:length(c.lambda)){
    # -- eq taken from math camp notes --
    df.L$LL[i] <- N*log(c.lambda[i]) - sum(c.lambda[i]*data) 
}


# log-likelihood 
plot(x = df.L$lambda, y = df.L$LL, type = "l",
     xlab = "Lambda", ylab = "Log-likelihood")

dev.off()
  

# At the maximum, slope is flat, and the rate of change is negative. 
# We are taking the derivative of this slope.

# hence, FOC is that first derivative = 0 and SOC is that second derivative < 0


# ------
# Data 
# ------

# Sample observations from exp dist
# Lambda is set to vary between 0 and 5. 

N <- 10000  # number of y 
list.y <- list() # the place to hold 
c.lambda <- seq(0.1, 5, by = 0.1) # a vector of lambda

# generate a hypothetical data
for(i in 1:length(c.lambda)){
    set.seed(20211001)
    list.y[[i]] <- y <- rexp(N, rate = c.lambda[i])   
}

# when lambda = 2
c.lambda[20] 
# ten thousand ys are distributed as the histogram below
hist(list.y[[20]], breaks=20) 

