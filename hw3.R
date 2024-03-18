# 1a
alpha <- 0.05
p <- 0.3
n_values <- c(50, 200, 300) # Sample sizes for a,b,c


for (n in n_values) {
  SE <- sqrt(p*(1-p)/n) # Standard error 
  z_star <- qnorm(alpha/2, lower.tail = FALSE) # CV
  ME <- z_star*SE # Margin of Error 
  cat("Margin of Error for n =", n, "is", ME, "\n") 
}



# 1b
alpha <- 0.01
p_values <- c(0.3, 0.5, 0.7) # true proportion for a,b,c
n <- 200 


for (p in p_values) {
  SE <- sqrt(p*(1-p)/n) # Standard error 
  z_star <- qnorm(alpha/2, lower.tail = FALSE) # CV
  ME <- z_star*SE # Margin of Error 
  cat("Margin of Error for p =", p, "is", ME, "\n") 
}

# 1h
rr<-qchisq(0.05,df=5,lower.tail=F)
rr
# 1i
rr<-qchisq(0.01,df=10,lower.tail=F)
rr


# 2a
alpha <- 0.10
n <- 600
x <- 240
p_hat <- x/n # sample proportion
cat("Sample proportion (p_hat):", p_hat, "\n")


SE <- sqrt(p_hat*(1-p_hat)/n) # Standard error
cat("Standard Error (SE):", SE, "\n")


z_star <- qnorm(1-alpha/2, lower.tail = TRUE) # CV
ME <- z_star*SE # Margin of Error
cat("Margin of Error (ME):", ME, "\n")

cat("95% Confidence Interval: [", p_hat+ME, ",", p_hat-ME, "]", "\n")

# 2b
n <- 600
x <- 240
prop.test(x,n,alternative = "two.sided",conf.level = 0.95,correct = F)

# 2c
n <- 600
x <- 240
prop.test(x, n, p = 0.30, alternative = "greater", correct = F)


# 2d
alpha<-0.05
n <- 600
x <- 240
p_hat <- x/n # sample proportion
z_star<-qnorm(alpha/2,lower.tail = F)
z_star

# 2e
alpha<-0.10
p <- 0.5
z_star<-qnorm(alpha/2,lower.tail = F)
z_star



# 3a
# counts of successes (bars exceeding the limit) for each type
x1 <- 110 # X
x2 <- 85  # Y

# total bars tested for each type
n1 <- 130 # X
n2 <- 140 # Y

prop.test(x=c(x1,x2), n=c(n1,n2), alternative="two.sided", conf.level=0.95, correct=FALSE)


# 3c
# counts of successes (bars exceeding the limit) for each type
x1 <- 110 # X
x2 <- 85  # Y

# total bars tested for each type
n1 <- 130 # X
n2 <- 140 # Y

prop.test(x=c(x1,x2), n=c(n1,n2), alternative="greater", conf.level=0.95, correct=FALSE)



# 4c
observed <- c(Ham=80, Turkey=70, Veggie=90, Cheese=60)
expected <- c(Ham=75, Turkey=75, Veggie=75, Cheese=75)

chisq.test(x=observed, p=expected/sum(expected))


# 4d
df <- 3 # from 4c
qchisq(1 - 0.05, df)


