library(HH)
library(scatterplot3d)
library(MASS)
library(car)
library(leaps)

BostonHousing<-read.csv(file="./BostonHousing.csv", header=T)
head(BostonHousing)

#Boston <- BostonHousing[,1]
MEDV <- BostonHousing[,1]
CRIM <- BostonHousing[,2]
ZN <- BostonHousing[,3]
INDUS <- BostonHousing[,4]
CHAS <- BostonHousing[,5]
NOX <- BostonHousing[,6]
RM <- BostonHousing[,7]
AGE <- BostonHousing[,8]
DIS <- BostonHousing[,9]
RAD <- BostonHousing[,10]
TAX <- BostonHousing[,11]
PTRATIO <- BostonHousing[,12]
LSTAT <- BostonHousing[,13]

Boston.lm <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + 
                  LSTAT )
summary(Boston.lm)

# Scatter plot matrix
pairs(BostonHousing[, c(13, 1:6)], pch = 20, lower.panel = NULL)
pairs(BostonHousing[, c(13, 7:12)], pch = 20, lower.panel = NULL)

# 1.
pairs(BostonHousing)

# 2.
vif(Boston.lm)

# 3. 
#Variable Selection using best subset regression
model.subset <- regsubsets(MEDV ~ ., data = BostonHousing, nbest = 1, nvmax = 13)
summary(model.subset)
plot(model.subset, scale = "bic")

#Variable selection using stepwise regression
nullmodel <- lm(MEDV ~ 1, data = BostonHousing)
fullmodel <- lm(MEDV ~ ., data = BostonHousing)

#forward selection
model.step.f <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
#Backward selection
model.step.b <- step(fullmodel, direction = "backward")
#stepwise selection
model.step <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
AIC(model.step)
BIC(model.step)
summary(model.step)

# 4.
#refit the model
Boston.lm2 <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + TAX + PTRATIO + 
                 LSTAT)
summary(Boston.lm2)

# 5.
#Residual Diagnostic
par(mfrow = c(2,2))
plot(Boston.lm2)

