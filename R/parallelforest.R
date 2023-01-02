# Speed comparison between randomForest, ranger and Rborist
# We use STDCE classif5 with 11286 items and 40 variables
pcloud <- svMisc::pcloud
train <- readRDS(pcloud("plankton", "stdce", "trainingsets", "stdcetrain3.rda"))
# Also save a .csv version for Python
readr::write_csv(train, pcloud("plankton", "stdce", "trainingsets", "stdcetrain3.csv"))

# Eliminate unwanted variables
train$Id <- NULL
train$Label <- NULL
train$Item <- NULL
train$XStart <- NULL
train$YStart <- NULL
train$Dil <- NULL

# Rework Class (eliminate the empty Class '.')
train$Class <- as.factor(as.character(train$Class))
table(train$Class)

mtry <- sqrt(ncol(train) - 1) # Default value (indicated explicitly to be sure)
ntree <- 500L
min.node <- 1L
nthreads <- 32L # Adapt this!

# Usual randomForest
library(randomForest)
system.time(class3 <- randomForest(data = train, Class ~ .,
  ntree = ntree, mtry = mtry, nodesize = min.node, importance = FALSE,
  localImp = FALSE, proximity = FALSE))
# 11sec on EN-Shark (Ryzen 9 5950X, 16 cores, 32 threads), idem on EN-Hammerhead
# 16sec on EN-Cuttlefish (MacBook Pro 15'' i9, 8 cores, 16 threads)
summary(class3) # OOB error rate = 11.54%

# Ranger, similar code to randomForest() with same default values
library(ranger)
system.time(class3 <- ranger(data = train, Class ~ .,
  num.trees = ntree, mtry = mtry, importance = "none", min.node.size = min.node,
  num.threads = nthreads))
# 2sec on EN-Shark using all 32 threads, 2.3sec on EN-Hammerhead
# 10.2sec on EN-Cuttlefish using all 16 threads
summary(class3) # OOB error rate = 11.54%

# Rborist
library(Rborist)
system.time(class3 <- Rborist(train[-34], train$Class,
  nTree = ntree, predFixed = mtry, minNode = min.node, noValidate = TRUE,
  nThread = nthreads))
# 4.4sec on EN-Shark, using all 32 threads, 5.1sec on EN-Hammerhead (new params)
# 6.7sec on EN-Cuttlefish using all 16 threads
class3$validation$oobError # OOB error rate = 12.08%

# cuda.ml, using the GPU
library(cuda.ml)
has_cuML() # Must be TRUE
system.time(class3 <- cuda_ml_rand_forest(data = train, Class ~ .,
  trees = ntree, mtry = mtry, min_samples_leaf = min.node,
  max_batch_size = 128L, n_streams = 8L,
  cuML_log_level = "off")) # Use "off" or "trace" for instance
# max_batch_size = maximum number of nodes processed per batch (128L by default)
# Using a higher or lower value produces worth timings
# n_streams is the number of CUDA streams to use for building trees (8L by default)
# Increasing or decreasing this value (up to 32L), has no impact on the results
# 0.95sec with 128L/8L and 1.01 with 256L/16L on EN-Hammerhead

# Cannot get OOB predictions here?

# What about a training set 10x larger (112860 items)?
train10 <- dplyr::bind_rows(train, train, train, train, train,
                            train, train, train, train, train)

system.time(class3 <- randomForest(data = train10, Class ~ .,
  ntree = ntree, mtry = mtry, nodesize = min.node, importance = FALSE,
  localImp = FALSE, proximity = FALSE))
# 118sec = almost 2min => barely useable!

system.time(class3 <- ranger(data = train10, Class ~ .,
  num.trees = ntree, mtry = mtry, importance = "none", min.node.size = min.node,
  num.threads = nthreads))
# 16.6sec on EN-Hammerhead

system.time(class3 <- Rborist(train10[-34], train10$Class,
  nTree = ntree, predFixed = mtry, minNode = min.node, noValidate = TRUE,
  nThread = nthreads))
# 21.5sec on EN-Hammerhead

system.time(class3 <- cuda_ml_rand_forest(data = train10, Class ~ .,
  trees = ntree, mtry = mtry, min_samples_leaf = min.node,
  max_batch_size = 256L, n_streams = 16L,
  cuML_log_level = "off"))
# 2.33sec with batch 128L/8L and 2.02sec with 256L/16L on EN-Hammerhead

# => In our cases, ranger seems to be the best one on Linux on CPU,
# but Rborist does better on MacOS.
# The GPU version is already 2.5x faster than ranger, but with 10x more data,
# it blows it out of the water being 8x faster (and 58x faster than the original
# randomforest() implementation)!

# What about only less threads with ranger() on train and train10?
system.time(class3 <- ranger(data = train, Class ~ .,
  num.trees = ntree, mtry = mtry, importance = "none", min.node.size = min.node,
  num.threads = 16L))
# randomForest() was 11sec
# time (threads): 26.68sec (1), 13.44sec (2), 6.87sec (4), 3.56sec (8),
# 1.9sec (16), 1.95sec (24), 2.2sec (32)
# Thus randomForest() is faster than ranger() withg low number of threads.
# We need >= 2 threads for ranger(), and 16 threads = 16 CPU is the fastest

system.time(class3 <- ranger(data = train10, Class ~ .,
  num.trees = ntree, mtry = mtry, importance = "none", min.node.size = min.node,
  num.threads = 16L))
# randomForest was 118sec
# time (threads): 151sec (1), 77.00sec (2), 39.68sec (4), 22.20sec (8),
# 12.86sec (16), 13.27sec (24), 17.20sec (32)
# Here again, 16 threads is the fastest, but now, 2 threads is enough to get
# better results than randomForest()

# Conclusions: cuda.ml is the best (where GPU/CUDA is available), otherwise
# ranger() is second best (also because Rborist() has a different interface)
# For 100,000+ items, cuda.ml (RTX 3090) is 6.5x times faster than ranger() (Ryzen 5950x)

# Now, try with more than 1 million items (randomForest max is 1 million? not tested)
train100 <- dplyr::bind_rows(train10, train10, train10, train10, train10,
                             train10, train10, train10, train10, train10)

system.time(class3 <- cuda_ml_rand_forest(data = train100, Class ~ .,
  trees = ntree, mtry = mtry, min_samples_leaf = min.node,
  max_batch_size = 512L, n_streams = 8L,
  cuML_log_level = "off"))
# 8.86sec (18% RAM max 24GB) on EN-Hammerhead

system.time(class3 <- ranger(data = train100, Class ~ .,
  num.trees = ntree, mtry = mtry, importance = "none", min.node.size = min.node,
  num.threads = 16L))
# 175sec on EN-Hammerhead

# With > 1,000,000 items, cuda.ml is almost 20x faster than ranger

# What about Python version in scikit learn?
reticulate::repl_python()
# Copy-paste this to the console
import pandas as pd
df = pd.read_csv("~/pCloudDrive/plankton/stdce/trainingsets/stdcetrain3.csv")
df.sample(5, random_state = 44)
df.info()
#df = df.dropna()
X = df.drop(["Id", "Label", "Item", "XStart", "YStart", "Dil", "Class"], axis = 1)
y = df["Class"]
#from sklearn.model_selection import train_test_split
#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 44)
X_train = X
y_train = y
from sklearn.ensemble import RandomForestClassifier
rf_model = RandomForestClassifier(n_estimators = 500, max_features = "sqrt", random_state = 0)
import time
start_time = time.time()
rf_model.fit(X_train, y_train)
elapsed_time = time.time() - start_time
print(f"Elapsed time to compute random forest with scikit-learn: {elapsed_time:.3f} seconds")
# This makes 17.91sec on En-Hammerhead (xas 11 sec for R's randomForest())
# So, the Python version is much slower

# Using cuML
from cuml.ensemble import RandomForestClassifier as cuRF
curf_model = cuRF(n_estimators = 500, random_state  = 0)
import time
start_time = time.time()
curf_model.fit(X_train, y_train)
elapsed_time = time.time() - start_time
print(f"Elapsed time to compute random forest with cuML: {elapsed_time:.3f} seconds")
# Cannot run this because I cannot install cuML
