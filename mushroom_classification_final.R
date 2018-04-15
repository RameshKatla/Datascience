setwd("C:\\RAMESH\\DSLA")
mushroom_dataset <- read.csv("mushrooms.csv",header=TRUE)

#mushroom_dataset <- mushroom

library(Amelia)
library(mlbench)
library(caret)
library(skimr)

#Exploratory data analysis

#View(mushroom_dataset)
head(mushroom_dataset)
str(mushroom_dataset)
summary(mushroom_dataset)


dim(mushroom_dataset)

names(mushroom_dataset)

dim(mushroom_dataset)

str(mushroom_dataset)

levels(mushroom_dataset$class)

summary(mushroom_dataset)

for (i in 2:23) {
  mushroom_dataset[,i] <- as.numeric(mushroom_dataset[,i])
  
}


mushroom_dataset[,1] <- as.factor(mushroom_dataset[,1])

#mushroom_dataset$class <- ifelse(mushroom_dataset$class == "e",1,0)

# visualizing Data

par(mfrow=c(2,11))

for ( i in 2:23){
  hist(mushroom_dataset[,i], main=names(mushroom_dataset)[i])
}


par(mfrow=c(2,11))

for ( i in 2:23){
  boxplot(mushroom_dataset[,i], main=names(mushroom_dataset)[i])
}


missmap(mushroom_dataset, col=c("blue","red"), legend=FALSE)

# Correlation between each variables

library(corrplot)
correlations <- cor(mushroom_dataset[,2:23])
corrplot(correlations, method="circle")


pairs(mushroom_dataset, col=mushroom_dataset$Class)


x <- mushroom_dataset[,2:23]
y <- mushroom_dataset[,1]

scales <- list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x, y=y, plot="density",scales=scales
)


index <- createDataPartition(mushroom_dataset[,1], p=0.80, list=FALSE)

testset <- mushroom_dataset[-index,]
trainset <- mushroom_dataset[index,]

dim(trainset)

names(trainset)

dim(testset)

str(trainset)

summary(trainset)

levels(trainset$class)


x<- trainset[,2:23]
y <- trainset[,1]

featurePlot(x=x,
            y=y, 
            plot="density", 
            scales=scales)

featurePlot(x = x, 
            y = y, 
            plot = "pairs")


skimmed <- skim_to_wide(trainset)
skimmed
anyNA(skimmed)

table(trainset$Class)


attach(trainset)

mod_fit_one <- glm(trainset$class ~ trainset$cap.shape,
                   data = trainset,
                   family="binomial")

print(mod_fit_one)
summary(mod_fit_one)

mod_fit_two <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface,
                   data = trainset,
                   family="binomial")

print(mod_fit_two)
summary(mod_fit_two)

mod_fit_three <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color,
                   data = trainset,
                   family="binomial")

mod_fit_four <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises,
                   data = trainset,
                   family="binomial")

mod_fit_five <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor,
                   data = trainset,
                   family="binomial")

mod_fit_six <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment,
                   data = trainset,
                   family="binomial")

mod_fit_seven <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing,
                   data = trainset,
                   family="binomial")

mod_fit_eight <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size,
                     data = trainset,
                     family="binomial")



mod_fit_nine <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color,
                   data = trainset,
                   family="binomial")

mod_fit_ten <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape,
                   data = trainset,
                   family="binomial")

mod_fit_11 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root,
                   data = trainset,
                   family="binomial")

mod_fit_12 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring,
                   data = trainset,
                   family="binomial")

mod_fit_13 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring,
                  data = trainset,
                  family="binomial")

mod_fit_14 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    ,
                  data = trainset,
                  family="binomial")

mod_fit_15 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring,
                  data = trainset,
                  family="binomial")

mod_fit_16 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring+trainset$veil.type,
                  data = trainset,
                  family="binomial")

mod_fit_17 <- glm(trainset$class ~ trainset$cap.shape
                  +trainset$cap.surface
                  +trainset$cap.color
                  +trainset$bruises
                  +trainset$odor
                  +trainset$gill.attachment
                  +trainset$gill.spacing
                  +trainset$gill.size
                  +trainset$gill.color
                  +trainset$stalk.shape
                  +trainset$stalk.root
                  +trainset$stalk.surface.above.ring
                  +trainset$stalk.surface.below.ring
                  +trainset$stalk.color.above.ring
                  +trainset$stalk.color.below.ring
                  +trainset$veil.type
                 # +trainset$veil.color
                  ,
                  data = trainset,
                  family="binomial")

mod_fit_18 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring+trainset$veil.type
                  #+trainset$veil.color
                  +trainset$ring.number,
                  data = trainset,
                  family="binomial")

mod_fit_19 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring+trainset$veil.type
                  # +trainset$veil.color
                  +trainset$ring.number+trainset$ring.type,
                  data = trainset,
                  family="binomial")

mod_fit_20 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size
                  +trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring+trainset$veil.type
                  #+trainset$veil.color
                  +trainset$ring.number+trainset$ring.type
                  #+trainset$ring.spore.print.color
                  ,
                  data = trainset,
                  family="binomial")

mod_fit_21 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                    +trainset$stalk.color.below.ring+trainset$veil.type
                  # +trainset$veil.color
                  +trainset$ring.number+trainset$ring.type
                  #+trainset$ring.spore.print.color
                  +trainset$population,
                  data = trainset,
                  family="binomial")

mod_fit_22 <- glm(trainset$class ~ trainset$cap.shape+trainset$cap.surface+trainset$cap.color+trainset$bruises+trainset$odor+trainset$gill.attachment+trainset$gill.spacing+trainset$gill.size+trainset$gill.color+trainset$stalk.shape+trainset$stalk.root+trainset$stalk.surface.above.ring+trainset$stalk.surface.below.ring+trainset$stalk.color.above.ring
                  +trainset$stalk.color.below.ring+trainset$veil.type
                  # +trainset$veil.color
                  +trainset$ring.number+trainset$ring.type
                  #+trainset$ring.spore.print.color
                  +trainset$population
                 # +trainset$habitat
                 ,
                  data = trainset,
                  family="binomial")





anova(mod_fit_one, 
      mod_fit_two,
      mod_fit_three, 
      mod_fit_four, 
      mod_fit_five, 
      mod_fit_six, 
      mod_fit_seven, 
      mod_fit_eight, 
      mod_fit_nine, 
      mod_fit_ten, 
      mod_fit_11, 
      mod_fit_12, 
      mod_fit_13, 
      mod_fit_14, 
      mod_fit_15, 
      mod_fit_16, 
      mod_fit_17, 
      mod_fit_18, 
      mod_fit_19,
      mod_fit_20, 
      mod_fit_21, 
      mod_fit_22, 
      test="Chisq")

model.glm <- train(x=trainset[,2:23],y = trainset$class, method="glm")
print(model.glm)
plot(varImp(model.glm))

model.glm <- train(x=testset[,2:23],y = testset$class, method="glm")
print(model.glm)

varImp(model.glm)




model.glm <- train(x=trainset$gill.size,y = trainset$class, method="glm")
print(model.glm)

predictors<-c("spore.print.color", "odor", "gill.size" , "gill.color" , "stalk.shape" )

model.glm <- train(x=trainset[,predictors],y = trainset$class, method="glm")
print(model.glm)


model.glm <- train(x=testset[,predictors],y = testset$class, method="glm")
print(model.glm)



#1. Feature selection using Caret

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      number =5,
                      verbose = FALSE)
# outcomeName<-'Class'
# predictors<-names(trainset)[!names(trainset) %in% outcomeName]
mushroom_Profile <- rfe(trainset[,2:23], trainset[,1],
                         rfeControl = control)
mushroom_Profile

predictors(mushroom_Profile)
print(mushroom_Profile)

plot(mushroom_Profile,type=c("g","0"))

#2. Rank Features by importance

#train the model
data(mushroom_dataset)
#model <- train(trainset$Class~ ., data=mushroom_dataset, method="lvq", preProcess="scale", trControl=control)

# estimate variable importance

importance <- varImp(mushtoom_Profile, scale=FALSE)

#summarize importance
print(importance)

# plot importance

plot(importance)

# Remove Redundant Features

set.seed(7)

# calculate the correlation matrix
correlationMatrix <- cor(trainset[,2:16,18:23])
#summarize the  correlation matrix
print(correlationMatrix)

#find attributes that are higly correlated (ideally > 0.75)
higlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)


#print indexes of higly correlated attributes
print(higlyCorrelated)


#




#Decision Trees


library(caret)
set.seed(1000)
?rpart

model.rpart <- train(x=trainset[,2:23],y = trainset[,1], method="rpart", metric="Accuracy")

print(model.rpart)

plot(model.rpart$finalModel)

model.rpart$finalModel

#rattle package to product some pretty tree plots
library(rattle)
library(rpart)
library(rpart.plot)
rpart.plot(model.rpart$finalModel)
#Predictions on train dataset





pred <- table(predict(object= model.rpart$finalModel,newdata = trainset[,2:23],type="class"))

confusionMatrix(predict(object= model.rpart$finalModel,newdata = trainset[,2:23],type="class"),trainset$class)

pred_test1 <- predict(object= model.rpart$finalModel,newdata = testset[,2:23],type="class")

confusionMatrix(pred_test1,testset$class)

#variable importance

varImp(model.rpart)

plot(varImp(model.rpart))


#random forest algorithm

#library(caret)

model.rf <- train(x=trainset[,2:23],y = trainset[,1], method="rf", metric="Accuracy")

print(model.rf)

pred <- predict(object=model.rf$finalModel,newdata = trainset[,2:23],type="class")

confusionMatrix(pred,trainset$class)

pred <- predict(object=model.rf$finalModel,newdata = testset[,2:23],type="class")

confusionMatrix(pred,testset$class)

varImp(model.rf)

plot(varImp(model.rf))


# gradient boost algorithm

library(gbm)
set.seed(1000)

#fit the model 

model.gbm <- train(x=trainset[,2:23],y = trainset[,1], method="gbm", metric="Accuracy",verbose=FALSE)


print(model.gbm)

summary(model.gbm)

pred <- predict(object=model.gbm,newdata=trainset[,2:23])
confusionMatrix(pred,trainset$class)

#performance on the test set

pred <- predict(object=model.gbm,newdata=testset[,2:23])
confusionMatrix(pred,testset$class)

model_correct <- testset$class == pred_test1

print(model_correct)
plot(model_correct)

plot(varImp(model.gbm))

# misclassify <- ggplot(testset) +
#              geom_point(aes(Petal.Length,Sepal.Length,colour=model_correct,shape=class,size=2.1))+
#              labs(x="Petal.Length",y="Sepal.Length")
#              
# print(misclassify) 


# K Means Clusetring Model

set.seed(20)

#mushroomCluster <- kmeans(mushroom[,2:23],centers = 2, nstart =20)
#print(mushroomCluster)

table(mushroomCluster$cluster,mushroom$class)
# plot(mushroom[c("Sepal.Length","Sepal.Width")],col=mushroomCluster$cluster)
# points(mushroomCluster$centers[,c("Sepal.Length","Sepal.Width")],col=1:3, pch=8, cex=2)


# Linear Discriminant Analysis
# library(caret)
# #install.packages("MASS")
# library(MASS)
# 
# set.seed(1000)
# model.lda<- train(x=trainset[,2:23],y=trainset[,1],method="lda",metric="Accuracy")
# 
# print (model.lda)
# 
# pred <- predict(object=model.lda,newdata=trainset[,2:23])
# confusionMatrix(pred,trainset$class)

results <- resamples(list(Tree=model.rpart,RandomForest=model.rf,GBM=model.gbm, GLM=model.glm))
summary(results)

dotplot(results)


#Performance of Logisitc Regression Model
#AIC(Akaike Information Criteria) -> Analogous of Adjusted R2 in Logistic Regression
#Null deviance and Residual Deviance - model predicted with nothing but an intercept , lower value better the model
#Confusion Matrix
#ROC Curve - summarizes the model performance by evaluating the trade offs between true postivite rate
#(sensitivity()) and false positivee rate(1- specificity())
# maximum likelihood 


#Taking only the top 5 predictors


model_glm<-train(trainset[,predictors],trainset[,1],method='glm')
model_rpart<-train(trainset[,predictors],trainset[,1],method='rpart')
model_rf<-train(trainset[,predictors],trainset[,1],method='rf')
model_gbm<-train(trainset[,predictors],trainset[,1],method='gbm')

#parameter tuning 

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

# Using tuneGrid

modelLookup(model='gbm')

#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))


model_gbm<-train(trainset[,predictors],trainset[,1],method='gbm', trControl = fitControl, tuneGrid = grid)

plot(model_gbm)

#variable importance

varImp(object=model_glm)

varImp(object=model_rf)

varImp(object=model_rpart)

varImp(object=model_gbm)


#Predictiona using caret

#Predictions
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)