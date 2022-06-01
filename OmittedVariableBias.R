# --------------------------------------------
# Recitation (pols602): Omitted variable bias
# Keigo Tanabe (tamu)
# 2021-Oct-28
# --------------------------------------------

# A simple illustration of Clarke 2005

library(stargazer)

# --------
# TRIAL 1
# --------

# DGP 
A <- cbind(rnorm(1000,0,1), rnorm(1000,0,1), rnorm(1000,0,1))
mu <- c(1,2,3)
eps <- rnorm(1000,0,1)
r34 <- 0.25
Omega <- matrix(c(1  , 0.5, 0.5,
                  0.5, 1  , r34,
                  0.5, r34  , 1), 3)
X <- t(mu + t(A %*% chol(Omega)))

# delta = 2 (omitted X4 is related to y)
delta <- 2
beta <- c(2,1,1, delta)
y <- cbind(rep(1,1000), X)%*%beta + eps

# Setting up my dataset
my_data <- data.frame(cbind(y, X))
names(my_data) <- c("y","x2","x3","x4")
    
# Fit models
m.true <- lm(y~x2+x3+x4, my_data)
m.s <- lm(y~x2, my_data)
m.l <- lm(y~x2+x3, my_data)

# ------------
# Results
# ------------
stargazer(m.true,m.s,m.l,type="text",
          keep.stat = c("n", "adj.rsq"))


# --------
# TRIAL 2
# --------

# The omitted variable takes another 
# value for its coefficient

delta <- -2  # the same magnitude, opposite sign
beta <- c(2,1,1, delta)
y <- cbind(rep(1,1000), X)%*%beta + eps
my_data <- data.frame(cbind(y, X))
names(my_data) <- c("y","x2","x3","x4")

# Fit models again
m.s2 <- lm(y~x2, my_data)
m.l2 <- lm(y~x2+x3, my_data)
m.true2 <- lm(y~x2+x3+x4, my_data)


# Results.
stargazer(m.true2,m.s2,m.l2,type="text",
          keep.stat = c("n", "adj.rsq"))


# everything put together
stargazer(m.s,m.l,
          m.s2,m.l2,type="text",
          keep.stat = c("n", "adj.rsq"),
          column.labels= c("beta4 = 2","beta4 = -2"),
          column.separate = c(2,2))



    # Kevin Clarke 2005 and 2009.
        # 2005:``Phantom Menace"
        # 2009: ``Return of the Phantom"

    # Aronow & Samii. 2016. 
    # “Does Regression Produce Representative Estimates of Causal Effects? (ajps) 

    # Samii. 2015. ``Causal Empiricism in Quantitative Research" (jop)

