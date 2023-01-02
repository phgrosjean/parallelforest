library(randomForest)
library(ranger)

data(meuse, package = "sp")
meuse$logZn <- log10(meuse$zinc)

n <- 1000
rf.stats <- data.frame(rep = 1:n, rsq = as.numeric(NA), mse = as.numeric(NA))
system.time(
  for (i in 1:n) {
    model.rf <- randomForest(data = meuse,
      logZn ~ ffreq + x + y + dist.m + elev + soil + lime,
      importance = TRUE, na.action = na.omit, mtry = 5)
    summary(model.rf$rsq)
    summary(model.rf$mse)
    rf.stats[i, "mse"] <- median(summary(model.rf$mse))
    rf.stats[i, "rsq"] <- median(summary(model.rf$rsq))
  }
)

library(foreach)
cl <- parallel::makePSOCKcluster(15L)
doParallel::registerDoParallel(cl, cores = 2L)

rf.stats <- data.frame(rep = 1:n, rsq = as.numeric(NA), mse = as.numeric(NA))
ra.stats <- data.frame(rep = 1:n, rsq = as.numeric(NA), mse = as.numeric(NA))
ra.stats2 <- data.frame(rep = 1:n, rsq = as.numeric(NA), mse = as.numeric(NA))
microbenchmark::microbenchmark(times = 10L,
  rf = for (i in 1:n) {
    model.rf <- randomForest(data = meuse,
      logZn ~ ffreq + x + y + dist.m + elev + soil + lime,
      importance = TRUE, na.action = na.omit, mtry = 5)
    summary(model.rf$rsq)
    summary(model.rf$mse)
    rf.stats[i, "mse"] <- median(summary(model.rf$mse))
    rf.stats[i, "rsq"] <- median(summary(model.rf$rsq))
  },
  ranger = for (i in 1:n) {
    model.ra <- ranger(data = meuse, num.threads = 32L,
      logZn ~ ffreq + x + y + dist.m + elev + soil + lime,
      importance = "none", mtry = 5, write.forest = FALSE)
    ra.stats[i, "mse"] <- model.ra$prediction.error
    ra.stats[i, "rsq"] <- model.ra$r.squared
  },
  ranger2 = foreach(i = 1:n) %dopar% {
    model.ra <- ranger::ranger(data = meuse, num.threads = 2L,
      logZn ~ ffreq + x + y + dist.m + elev + soil + lime,
      importance = "none", mtry = 5, write.forest = FALSE)
    ra.stats2[i, "mse"] <- model.ra$prediction.error
    ra.stats2[i, "rsq"] <- model.ra$r.squared
  }
)

parallel::stopCluster(cl)

# With 15 clusters of 2 threads for ranger:
#Unit: seconds
#   expr        min         lq       mean     median         uq        max neval cld
#     rf 115.765523 115.905376 115.985664 116.017704 116.062737 116.105317    10   c
# ranger   5.709917   5.716316   5.739647   5.740903   5.750848   5.789124    10  b
#ranger2   2.392355   2.396576   2.486530   2.398491   2.403363   3.269549    10 a

# With a Ryzen 5950x,
# randomForest: single threaded, 116 sec
# ranger2 with 32 clusters of 1 takes 2.47sec, while ranger with 32L takes 5.76sec
# ranger2 with 30 clusters of 1 takes 2.48sec, while ranger with 32L takes 5.73sec
# ranger2 with 16 clusters of 2 takes 2.45sec, while ranger with 32L takes 5.79sec
# ranger2 with 15 clusters of 2 takes 2.41sec, while ranger with 32L takes 5.74sec
# ranger2 with  8 clusters of 4 takes 2.54sec, while ranger with 32L takes 5.77sec
# ranger2 with  7 clusters of 4 takes 2.68sec, while ranger with 32L takes 5.73sec
# ranger2 with  4 clusters of 4 takes 3.76sec, while ranger with 16L takes 5.86sec

# With 64Gb RAM, 7 clusters of 4 threads with 8Gb RAM seem a good compromise,
# as well as 15 clusters of 2 threads with 4Gb RAM
# while leaving 4/2 threads and 8/4Gb RAM for the system and other tasks...

# ranger with foreach is 46x faster than randomForest in this config
