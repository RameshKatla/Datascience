set.seed(500)
library(MASS)
data <- Boston

apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]

test <- data[-index,]

lm.fit <- glm(medv~., data=train)

summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

pr.lm <- predict(lm.fit,test)

MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data,2,max)
mins <- apply(data,2, min)

scaled <- as.data.frame(scale(data,center=mins, scale = maxs - mins ))

train <- scaled[index,]

test <- scaled[-index,]
