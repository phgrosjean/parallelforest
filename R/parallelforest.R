# Speed comparison between randomForest, ranger and Rborist
# We use STDCE classif5 with 11286 items and 40 variables
pcloud <- function(...) {
  if (svMisc::is_win()) {
    root <- "P:" # TODO: check this
  } else if (svMisc::is_mac()) {
    root <- "~/pCloud\ Drive"
  } else {# On Linux
    root <- "~/pCloudDrive"
  }
  file.path(root, ...)
}
train <- readRDS(pcloud("plankton", "stdce", "trainingsets", "stdcetrain3.rda"))
# TODO: corresponding code for Windows
# Eliminate unwanted variables
train$Id <- NULL
train$Label <- NULL
train$Item <- NULL
train$XStart <- NULL
train$YStart <- NULL
train$Dil <- NULL
# Eliminate the empty Class '.'
train$Class <- as.factor(as.character(train$Class))
table(train$Class)

# Usual randomForest
library(randomForest)
system.time(class3 <- randomForest(data = train, Class ~ .))
# 11sec on EN-Shark (Ryzen 9 5950X, 16 cores, 32 threads)
# 16sec on EN-Cuttlefish (MacBook Pro 15'' i9, 8 cores, 16 threads)
summary(class3) # OOB error rate = 11.54%

# Ranger, similar code to randomForest() with same default values
library(ranger)
system.time(class3 <- ranger(data = train, Class ~ .)) # num.threads = max_by_def))
# 2sec on EN-Shark using all 32 threads
# 10.2sec on EN-Cuttlefish using all 16 threads
summary(class3) # OOB error rate = 11.54%

# Rborist
library(Rborist)
system.time(class3 <- Rborist(train[-34], train$Class, nTree = 500))
# 4.4sec on EN-Shark, using all 32 threads
# 6.7sec on EN-Cuttlefish using all 16 threads
class3$validation$oobError # OOB error rate = 12.08%

# => In our cases, ranger seems to be the best one on Linux,
# but Rborist does better on MacOS.
