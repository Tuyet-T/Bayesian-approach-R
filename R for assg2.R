data <- read.csv("Assignment2Data_2024.csv")
View(data)
Price <- data$QAUR628BIS
Rt <- diff(log(Price))

sigma2 <- 3.020554
mu <- 0.3682817
tau <- 1.812332
n <- length(Rt)

p_tau <- 1/(n/sigma2 + 1/tau)
p_mu <- p_tau*(mu/tau + (n*mean(Rt))/sigma2)

p_tau
p_mu

n

#Normal distribution: mean = median = mode

#b) Quartile
Q1 <- qnorm(0.25, mean = p_mu, sd = sqrt(p_tau))
Q3 <- qnorm(0.75, mean = p_mu, sd = sqrt(p_tau))

Q1
Q3

#c) plot posterior distribution (Apply emperical rule)
x_axis<-seq(p_mu-3*sqrt(p_tau),
            p_mu+3*sqrt(p_tau), 0.01*sqrt(p_tau))
pos_pdf<-dnorm(x_axis, p_mu, sqrt(p_tau))
plot(x_axis, pos_pdf, col="orange",xlab="",
     ylab="Prob. Density", type="l", lwd = "2.5",
     main="PDF of the Posterior")

#d) Using "cm"
#install.packages("actuar")
library(actuar)
theta <- cm("bayes",Rt, likelihood = "normal", mean = mu , sd = sqrt(tau), sd.lik = sqrt(sigma2))
summary(theta)

#e)Compute a 98% Bayesian credible interval
z_up <- qnorm(0.02/2,lower.tail=F)
z_down <- qnorm(0.02/2,lower.tail=T)
Upper <- p_mu + z_up*sqrt(p_tau)
Upper
Lower <- p_mu + z_down*sqrt(p_tau)
Lower
z_up
z_down

