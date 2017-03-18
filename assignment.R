##### Download data #####

if (!file.exists("pml-training.csv"))
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "pml-training.csv")
if (!file.exists("pml-testing.csv"))
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "pml-testing.csv")

training <- read.csv("pml-training.csv",
                     na.strings = c("NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv")

summary(training)

training3 <-
  training %>%
  filter(new_window == "yes") %>%
  select(-(1:7))

summary(training3)

dim(training3)

excludePredictors <- 
  !is.na(apply(training3[, -153], 2, mean, na.rm = T)) &
  (apply(training3[, -153], 2, var, na.rm = T) != 0)

training3 <- training3[, c(excludePredictors, T)]

dim(training3)

summary(training3)

library(caret)
library(dplyr)

featurePlot(training3[, 1:100], y = training3$classe)


model1 <- train(classe ~ ., method = "treebag",
                trainControl = "cv", preProcess = "BoxCox",
                data = training3, na.action = na.omit)
print(model1)

model2 <- train(classe ~ ., method = "gbm",
                trainControl = "cv", preProcess = "BoxCox",
                data = training3, verbose = F,
                na.action = na.omit)
print(model2)

model3 <- train(classe ~ ., method = "rf",
                na.action = na.omit,
                trainControl = "cv", preProcess = "BoxCox",
                data = training3)
print(model3)
