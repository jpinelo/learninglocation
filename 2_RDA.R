library(caret)

#https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
df <- df[ ,c(1:2,5)]

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

rdaGrid = data.frame(gamma = (0:10)/4, lambda = 1/4)
set.seed(123)
rdaFit <- train(activity ~ . ,
                  data = training,
                  method = "rda",
                  tuneGrid = rdaGrid,
                  trControl = ctrl,
                  metric = "ROC")
rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$activity)
