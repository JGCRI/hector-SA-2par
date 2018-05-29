# Purpose: I am having lots of problems wiht the Dn stuff... I think it is becasue I jummped the gun and jsut started coding.. 
# Here I am going to look into the different ways to figure out the gamma distribtuion. 

Dn <- seq(0, 20, length.out = 1000)
a <- 3 
b <- 1/2
R_gamma <- dgamma(Dn, shape = a, scale = b)

My_gamma <- sapply(Dn, function(x){( x ^ (a - 1) * exp(-x/b) ) / ( gamma(a) * b ^a ) }) 



plot(Dn, R_gamma)
lines(Dn, My_gamma, col = "red")


# Okay so part of the problem is the gamma distribtuions are different from one another.... which is annoying 
# that I wasted so much time not checking that out. This was an important reminder that you should never blindly 
# turst anything. Not even R. 

 
# Compare the percentiles??? 
R_Dc <- qgamma(.95, shape = a,  scale = b, lower.tail = TRUE)

CDF <- cumsum((My_gamma * diff(Dn)[1] ))
plot(Dn, CDF)
index <- which.min(abs(CDF - .95))
Dn[index]



# Oh wait yes they are the same thing 





x <- 1:100
y <- 10 + .4 * x

sd(y)
