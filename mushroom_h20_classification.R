setwd("C:\\RAMESH\\DSLA")

library(data.table)
#library(h2o)
library(caret)
#set.seed(1000)

mushroom <- read.csv("mushrooms.csv",header=TRUE)

for (i in 2:23) {
  mushroom[,i] <- as.numeric(mushroom[,i])
}

mushroom[,1] <- as.factor(mushroom[,1])

#mushroom_dataset <- fread("mushrooms.csv",header=TRUE)

mushroom_dataset <- as.data.table(mushroom)

# index <- createDataPartition(mushroom_dataset[,1], p=0.75, list=FALSE)
# 
# test_mushroom <- mushroom_dataset[-index,]
# trai_mushroom <- mushroom_dataset[index,]
# 
# write.csv(test_mushroom)
# write.csv(train_mushroom)

mushroom_dataset[,prop.table(table(class))]
mushroom_dataset[,prop.table(table(`cap-shape`))]
mushroom_dataset[,prop.table(table(`cap-surface`))]
mushroom_dataset[,prop.table(table(`cap-color`))]
mushroom_dataset[,prop.table(table(bruises))]
mushroom_dataset[,prop.table(table(odor))]
mushroom_dataset[,prop.table(table(`gill-attachment`))]
mushroom_dataset[,prop.table(table(`gill-spacing`))]
mushroom_dataset[,prop.table(table(`gill-size`))]
mushroom_dataset[,prop.table(table(`gill-color`))]
mushroom_dataset[,prop.table(table(`stalk-shape`))]
mushroom_dataset[,prop.table(table(`stalk-root`))]
mushroom_dataset[,prop.table(table(`stalk-surface-above-ring`))]
mushroom_dataset[,prop.table(table(`stalk-surface-below-ring`))]
mushroom_dataset[,prop.table(table(`stalk-color-above-ring`))]
mushroom_dataset[,prop.table(table(`stalk-color-below-ring`))]
mushroom_dataset[,prop.table(table(`veil-type`))]
mushroom_dataset[,prop.table(table(`veil-color`))]
mushroom_dataset[,prop.table(table(`ring-number`))]
mushroom_dataset[,prop.table(table(`ring-type`))]
mushroom_dataset[,prop.table(table(`spore-print-color`))]
mushroom_dataset[,prop.table(table(`population`))]
mushroom_dataset[,prop.table(table(`habitat`))]

# Unique Value in ID variables
length(unique(mushroom_dataset$class))
length(unique(mushroom_dataset$odor))
length(unique(mushroom_dataset$`spore-print-color`))

# missing values
colSums(is.na(mushroom_dataset))

#bivariate analysis

library(ggplot2)

# odor vs sport-print-color
g <- ggplot(data=mushroom_dataset,aes(x=odor,fill=class)) 
g + geom_bar(stat="count")

g <- ggplot(data=mushroom_dataset,aes(x=odor,fill=class)) 
g + geom_bar()

#Cross Tables

library(gmodels)

CrossTable(mushroom_dataset$odor,mushroom_dataset$`spore-print-color`)

names(mushroom_dataset)

# convert

mushroom_dataset[,"Class" := ifelse("Class" =='e',1,0)]

#check classes of all variables

sapply(mushroom_dataset,class)

nrow(mushroom_dataset)

c.train <- mushroom_dataset[1:5124,]
c.test <- mushroom_dataset[-(1:5124),]

# loading h20 library
library(h2o)
localH2o <- h2o.init(nthreads = -1)
h2o.init()

# transfer the data from R to h2o instance

train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)

# check column index number

colnames(train.h2o)

# dependent variable (Class)

y.dep <- 1

# independent variables 
x.indep <- c(2:23)

# logistic regression 

model.glm <- h2o.glm(y=y.dep, x=x.indep, 
                     training_frame = train.h2o, 
                     family="binomial")

h2o.performance(model.glm)

#It means that only 94.1% of the variance in the dependent variable is explained by independent variable

# predictions

pred.glm <- as.data.frame(h2o.predict(model.glm, test.h2o))

pred.glm$predict

# Dropping bad and constant columns: [gill.attachment, veil.type, veil.color]

#Decision Trees

system.time(
  model.rf <- h2o.decc(y=y.dep, x=x.indep,
                               training_frame = train.h2o,
                               ntrees=1000,
                               mtries=3,
                               max_depth = 10,
                               seed=1122)
  
)

#model performance
h2o.performance(model.rf)


#Random Forest - Do well non-linear relationships
#Mtries default value for classification is 1/3 columns, regression 
# is square root of the no of columns

system.time(
  model.rf <- h2o.randomForest(y=y.dep, x=x.indep,
                              training_frame = train.h2o,
                              ntrees=1000,
                              mtries=3,
                              max_depth = 10,
                              seed=1122)

)

#model performance
h2o.performance(model.rf)

#hit ratio table tells you if you give the model n number of shots at guessing the output
# variable's class, how likely is it going to get it correct. Thus, 
# the first row of the hit_ratop table is basically the accuracy of the classification

h2o.hit_ratio_table(model.rf,valid=TRUE)


#model predictions
h2o.varimp(model.rf)
h2o.varimp_plot(model.rf)

#Gains/Lift Table:
h2o.gainsLift(model.rf,train.h2o)
h2o.gainsLift(model.rf,test.h2o)

pred.rf <- h2o.predict(model.rf, test.h2o)

pred.rf$predict

# GBM 

system.time(
  model.gbm <- h2o.gbm(y=y.dep, x=x.indep,
                               training_frame = train.h2o,
                               ntrees=1000,
                               max_depth = 4,
                               learn_rate = 0.01,
                               seed=1122)
  
)


#model performance
h2o.performance(model.gbm)

#model predictions
h2o.varimp(model.gbm)
h2o.varimp_plot(model.gbm)

#Gains/Lift Table:
h2o.gainsLift(model.gbm,train.h2o)
h2o.gainsLift(model.gbm,test.h2o)

pred.rf <- h2o.predict(model.rf, test.h2o)

pred.rf$predict


#h2o deep learning

system.time(
  model.dlearning <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122
  ))
