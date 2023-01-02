(redux::redis_available())
library(doRedis)
registerDoRedis(queue = "RJOBS", progress = TRUE)
nworkers <- 32L
startLocalWorkers(nworkers, queue = "RJOBS", linger = 1)

library(doFuture)
registerDoFuture()
plan(multisession, workers = nworkers)
plan(multicore, workers = nworkers)
plan(sequential)



library(future.redis)
(redux::redis_available())
plan(redis, queue = "EcoNum R cluster")
nworkers <- 32L
startLocalWorkers(nworkers, queue = "EcoNum R cluster", linger = 1, log = "/dev/null")

system.time(startLocalWorkers(64L, queue = "EcoNum R cluster", linger = 1, log = "~/Desktop/EcoNum_R_cluster.log")) # This is 1.9x faster than 32 workers, although I have only 16 core and 32 threads! Check with more complex calculations too!
# It takes less than 1sec to start, but we should use less than 2Gb per worker with 128GB RAM!

library(progressr)
handlers(global = TRUE)


my_fcn <- function(xs) {
  do_calc <- function(time) {
    start <- Sys.time()
    while (difftime(Sys.time(), start, units = "secs") < time) {
      1 + 1
    }
  }

  p <- progressor(along = xs)
  foreach(x = xs) %dopar% {
    do_calc(1)
    p(sprintf("x=%g", x))
    Sys.getpid()
  }
}

system.time(res <- my_fcn(1:1000))
unique(unlist(res))

r <- redux::hiredis()
r

# For future.redis
removeQ("EcoNum R cluster")
# For doRedis
removeQueue("RJOBS")

