dir.create("~/R", showWarnings = FALSE)
unlink("~/R/cluster.out")

library(doSNOW)
setDefaultClusterOptions(type = "SOCK", outfile = "") # "~/R/cluster.out")
cl <- makeCluster(rep("localhost", 8L), type = "SOCK")
#cl <- makeCluster(rep("localhost", 8L), type = "NWS")
#cl <- makeCluster(8L, type = "MPI") # MPI by default
registerDoSNOW(cl)

# or...

library(doParallel)
nworkers <- 16L
cl <- makeCluster(rep("localhost", nworkers), type = "PSOCK",
  outfile = "~/R/cluster.out")
registerDoParallel(cl)

iterations <- 100
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = rbind) %dopar% #,
  #.options.snow = opts) %dopar%
  {
    s <- summary(rnorm(1e6))[3]
    cat("\npid", Sys.getpid(), ": Median =  ", s, "\n", sep = "")
    return(s)
  }
close(pb)
try(stopCluster(cl), silent = TRUE)
# unlink("~/R/cluster.out")

# Use touch ~/R/cluster.out && clear && tail -f ~/R/cluster.out
# to monitor the outfile messages
cat(readLines("~/R/cluster.out"), sep = "\n")

# Another option adapted from the doRedis, but id dispays the progressbar only
# after zall calculations are done in doParallel
# Should work too with doMPI, doNWS
library(foreach)
library(utils)
library(iterators)
library(doParallel)
#library(snow)

#Choose number of iterations
n <- 1000L

# Progress combine function
f <- function() {
  pb <- txtProgressBar(min = 1, max = n - 1L, style = 3)
  count <- 0L
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    Sys.sleep(0.01)
    flush.console()
    c(...)
  }
}

# Start a cluster
cl <- makeCluster(8L, type = 'PSOCK', outfile = "")
registerDoParallel(cl)

# Run the loop in parallel
k <- foreach(i = icount(n), .final = sum, .combine = f()) %dopar% {
  log2(i)
  cat("\npid", Sys.getpid(), ": i =  ", i, "\n", sep = "")
}

head(k)

# Stop the cluster
stopCluster(cl)


# Another one, with doSNOW
# loading parallel and doSNOW package and creating cluster ----------------
library(parallel)
library(doSNOW)

numCores <- 8L
cl <- makeCluster(numCores, type = "SOCK")
registerDoSNOW(cl)

# progress bar ------------------------------------------------------------
library(progress)

iterations <- 100                               # used for the foreach loop

pb <- progress_bar$new(
  format = "letter = :letter [:bar] :elapsed | eta: :eta",
  total = iterations,    # 100
  width = 60)

progress_letter <- rep(LETTERS[1:10], 10)  # token reported in progress bar

# allowing progress bar to be used in foreach -----------------------------
progress <- function(n){
  pb$tick(tokens = list(letter = progress_letter[n]))
}

opts <- list(progress = progress)

# foreach loop ------------------------------------------------------------
library(foreach)

foreach(i = 1:iterations, .combine = rbind, .options.snow = opts) %dopar% {
  summary(rnorm(1e6))[3]
}

stopCluster(cl)


## Utilisation de doRedis

# Install and configuration of Redis server here: https://www.digitalocean.com/community/tutorials/how-to-install-and-secure-redis-on-ubuntu-20-04-fr
#sudo apt upgrade
#sudo apt install redis-server
#sudo nano /etc/redis/redis.conf
# Make sure we have "supervised systemd" and "timeout 0" to disalbe disconnecting clients
# (Re)start
#sudo systemctl restart redis.service
# Test
#sudo systemctl status redis
#redis-cli
#ping
#set test "It's working!"
#get test
#exit

#remotes::install_github("bwlewis/doRedis")
# Start several R processes in terminal and:
#library('doRedis')
#redisWorker('jobs')
#For a worker on another computer:
#redisWorker('jobs', host='Cazart', port=6379)
# For more control and debugging possibilities:
#options(error=recover); system("clear"); doRedis::redisWorker('jobs', loglevel = 1, timeout = 1)

# In the master R process:
library(doRedis)
library(foreach)

registerDoRedis('jobs')
setProgress(TRUE)
foreach(j = 1:100, .combine = sum, .multicombine = TRUE) %dopar% {
  doRedis::logger(paste("Processing iteration", j))
  Sys.sleep(0.1) # Artificially increase the calculation time
  4 * sum((runif(1000000) ^ 2 + runif(1000000) ^ 2) < 1) / 10000000
}

# Determine how many workers we have
getDoParWorkers()

# Parallel version of the boot() function, from the vignette of {doRedis}

bootForEach <- function(data, statistic, R, sim = "ordinary", stype = "i",
strata = rep(1, n), L = NULL, m = 0, weights = NULL, ran.gen = function(d, p) d,
mle = NULL, simple = FALSE, chunks = 1, verbose = FALSE, ...) {
  thisCall <- match.call()
  n <- if (length(dim(data)) == 2) nrow(data) else length(data)
  if (R < 2)
    stop("R must be greater than 1")
  Rm1 <- R - 1
  RB <- floor(Rm1 / chunks)

  combo <- function(...) {
    al <- list(...)
    out <- al[[1]]
    t <- lapply(al, "[[", "t")
    out$t <- do.call("rbind", t)
    out$R <- R
    out$call <- thisCall
    class(out) <- "boot"
    out
  }
  # We define an initial bootstrap replicate locally. We use this
  # to set up all the components of the bootstrap output object
  # that don't vary from run to run. This is more efficient for
  # large data sets than letting the workers return this information.
  boot <- boot::boot
  logger <- doRedis::logger
  binit <- boot(data, statistic, 1, sim = sim, stype = stype,
    strata = strata, L = L, m = m, weights = weights,
    ran.gen = ran.gen, mle = mle, ...)
  foreach(j = icount(chunks), .inorder = FALSE, .combine = combo,
    .init = binit, .packages = c("boot","foreach"),
    .multicombine = TRUE, .verbose = verbose)  %dopar% {
    logger(paste("Boot chunk", j))
    if (j == chunks) RB <- RB + Rm1 %% chunks
    res <- boot(data, statistic, RB, sim = sim, stype = stype,
      strata = strata, L = L, m = m, weights = weights,
      ran.gen = ran.gen, mle = mle, ...)
    list(t = res$t)
  }
}

# Example
# Bootstrap 95% CI for R-Squared
#library(boot)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}
# bootstrapping with 1000 replications
setProgress(FALSE)
results <- bootForEach(data = mtcars, statistic = rsq,  R = 1000,
 formula = mpg~wt+disp, chunks = 25)

# view results
results
plot(results)

# get 95% confidence interval
boot::boot.ci(results, type = "bca")

# Comparison between boot and bootForEach
set.seed(584)
res <- bench::mark(
  boot = boot::boot(data = mtcars, statistic = rsq,  R = 1000,
    formula = mpg~wt+disp),
  redis = bootForEach(data = mtcars, statistic = rsq,  R = 1000,
    formula = mpg~wt+disp, chunks = 8L),
  check = FALSE # Obviously, we always got different results!
)
# The redis version is 3.2x faster with 4 workers and 4x faster with 8 workers
# ... but these calculations are a little bit too slow for this system!

# Monitoring can be done from a different R session:
# - jobs() returns a data frame that lists all running jobs and information about them
# - tasks() returns a data frame of running tasks
# - removeQueue() remove Redis keys associated with a doRedis work queue
# - removeJob() remove all remaining tasks associated with a specified job in the work queue
# In case the master crashes during a calculation, use redisClose() before reconnecting to the cluster
# Redis limits values to less than 512Mb =# advanced topics in the vignette for larger data.

# This is the equivalent to closeCluster(cl)
removeQueue('jobs')

# Advantages of doRedis are elasticity, fault-tolerance and portability to different OSes. One can add more workers in the middle of a calculation!
# It is also possible to define batch processing of several iterations in
# one worker for performance enhancement. The drawback is that one need a
# configured Redis server. Machine crash or scaling back are automatically
# detected and re-submitted, so, it is partly fault-tolerant
# foreach results are incrementally aggregated in or out-of-order, thus reducing
# the memory required to hold temporary results on the master.
# parameter ftinterval (30sec by default) check if workers are responding
# The default value is generally OK. Increasing it increases overhead!!!
#

# I would need a tmuxinator configuration to get htop + n workers on a machine
