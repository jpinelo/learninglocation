library(caret)

#https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

inTrain <- createDataPartition(y = df$activity,  # the outcome variable that I'm trying to predict
                               p = 0.75,         # 75% of data for training
                               list = FALSE)
training <- df[inTrain, ]
testing <- df[-inTrain, ]
nrow(training)
nrow(testing)

ctrl <- trainControl(method = "repeatedcv",               # repeated K–fold cross–validation
                     repeats = 3,                         # no. of repetitions (default = 10)
                     classProbs = TRUE,                   #  include the calculations below (measures f perf.)
                     summaryFunction = twoClassSummary)   # which measures of perf. to include

plsFit <- train(activity ~ . ,
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))   ## Center and scale the predictors for the training
plsFit
plot(plsFit)

plsActivities <- predict(plsFit, newdata = testing)
str(plsActivities)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

confusionMatrix(data = plsActivities, testing$activity)
