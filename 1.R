
library(AppliedPredictiveModeling)
library(tidyverse)
library(lattice)
library(caret)
library(earth)


data(FuelEconomy)
colnames(cars2010)



cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2011$EngDispl),]

cars2010$Year <- '2010'
cars2011$Year <- '2011'

cars_all <- rbind(cars2010, cars2011)


xyplot(FE ~ EngDispl|Year, cars_all,
       xlab = "engine displacement",
       ylab = "fuel efficiency (mpg)",
       between = list(x=1.2))


lm1 <- train(FE ~ EngDispl,
             data = cars2010,
             method = "lm",
             trControl = trainControl(method = "cv"))

lm1
summary(lm1)


par(mfrow=c(1,2))
plot(cars2010$EngDispl, cars2010$FE, xlab = "engine displacement", ylab= "fuel eff")
lines(cars2010$EngDispl, fitted(lm1), col=2, lwd=2)

Observed = cars2010$FE
Predicted = fitted(lm1)
plot(Observed, Predicted, ylim = c(12,70))


#quadratic model

displacement = cars2010$EngDispl
cars2010$displacement2 <- cars2010$EngDispl^2
cars2011$displacement2 <- cars2011$EngDispl^2

set.seed(1)

lm2 <- train(FE ~ EngDispl + displacement2,
             data = cars2010,
             method = "lm",
             trControl = trainControl(method = "cv"))

lm2
summary(lm2)



par(mfrow=c(1,2))
plot(cars2010$EngDispl, cars2010$FE, xlab = "engine displacement", ylab= "fuel eff")
lines(cars2010$EngDispl, fitted(lm2), col=2, lwd=2)

Observed = cars2010$FE
Predicted = fitted(lm2)
plot(Observed, Predicted, ylim = c(12,70))




# mars model

set.seed(1)
marsfit <- train(FE ~ EngDispl,
                 data = cars2010,
                 method = "earth",
                 tuneLength = 15,
                 trControl = trainControl(method="cv"))

marsfit
summary(marsfit)


par(mfrow=c(1,2))
plot(cars2010$EngDispl, cars2010$FE, xlab = "engine displacement", ylab= "fuel eff")
lines(cars2010$EngDispl, fitted(marsfit), col=2, lwd=2)

Observed = cars2010$FE
Predicted = fitted(marsfit)
plot(Observed, Predicted, ylim = c(12,70))


cars2011$lm1 <- predict(lm1, cars2011)
cars2011$lm2 <- predict(lm2, cars2011)
cars2011$marsfit <- predict(marsfit, cars2011)


postResample(pred = cars2011$lm1, obs = cars2011$FE)
postResample(pred = cars2011$lm2, obs = cars2011$FE)
postResample(pred = cars2011$marsfit, obs = cars2011$FE)















