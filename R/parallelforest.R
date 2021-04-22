# Speed comparison between randomForest, ranger and Rborist
# We use STDCE classif5 with 11286 items and 40 variables
train <- readRDS("~/pCloudDrive/plankton/stdce/trainingsets/stdcetrain3.rda")
# Eliminate unwanted variables
train$Id <- NULL
train$Label <- NULL
train$Item <- NULL
train$XStart <- NULL
train$YStart <- NULL
train$Dil <- NULL
# Elimiunate the empty Class '.'
train$Class <- as.factor(as.character(train$Class))
table(train$Class)

# Usual randomForest
library(randomForest)
system.time(class3 <- randomForest(data = train, Class ~ .))
# 11sec on EN-Shark
summary(class3) # OOB error rater = 11.54%

# TODO: ranger & Rborist versions for comparison
