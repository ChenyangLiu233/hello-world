install.packages("effects")
install.packages("corrgram")
install.packages("car")

library(corrgram)
# plot a correlation diagram between 7 variables
corrgram(fitness, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)


# fit a linear model
fit1 <- lm(Oxygen ~ Weight + MaxPulse + RunPulse + RestPulse + RunTime + Age, data = fitness )
summary(fit1)
obsT <- coef(fit1)[-1]

# model diagnose:
anova(fit1)

# remedial action : delete variable Weight
fit2 <- lm(Oxygen ~ MaxPulse + RunPulse + RestPulse + RunTime + Age, data = fitness )
summary(fit2)

# compare fit1 fit2
anova(fit2, fit1)

# randomlization
rtest <- function (k = 1e4, data){
  covariates <- matrix(0, nrow = k, ncol = 6)
  N <- length(data$Oxygen)
  counter <- rep(0, 6)
  p <- rep(0,6)
  set.seed <- 190017612
  i = 1
  for (i in 1 : k) {
    rand <- sample(data$Oxygen, N, replace = FALSE)
    newfit <- lm(rand ~ Weight + MaxPulse + RunPulse + RestPulse + RunTime + Age, data = fitness)
    newT <-  coef(newfit)
    covariates[i,] <- newT[2:7]
    for (j in 1:6){
      if (abs(covariates[i,j]) > obsT[j]) counter[j] = counter[j] + 1
    }
    i = i + 1
  }
  p <- counter / k
  return(p)
}
rtest(data=fitness)


# model diagnose: residuals plot
par(mfcol = c(2,2), cex = 0.7)
plot(fit2)

install.packages("psych")
library(psych)

# model diagnose: collinearity
corr.test(fitness[-3], use = "complete")
install.packages("car")
library(car)
vif(fit2)

# remedial action: stepwise
col <- step(fit2)
fit3 <- lm(Oxygen ~ MaxPulse + RunPulse + RunTime + Age, data = fitness )
summary(fit3)

# compare model fit2 fit3
anova(fit3, fit2)

# model diagnose: residuals plot
par(mfcol = c(2,2), cex = 0.7)
plot(fit3)

# model diagnose: multicilinearity
vif(fit3)

# remedial action: 
install.packages("lars")
library(lars)
fitness <- data.matrix(fitness)
x <- fitness[,-3]
x <- x[, -2]
x <- x[, -3]
y <- fitness[,3]
lar = lars(x,y)




# remedial action: remove some sample points
glm2 <- lm(Oxygen ~ Weight + MaxPulse + RunPulse + RunTime + Age, data = fitness[-c(15,17),])
summary(glm2)
par(mfcol = c(1,2), cex = 0.7)
plot(glm2, which = 1)
plot(glm2, which = 2)

install.packages("ISLR")