# assuming that dat.RData contains
# 
# - dat : a data frame with a target called VARIABLE_CIBLE
# - best_feat : ordered list of the best features to predict the target
#
# this script set up a backward features selection thanks to the wraper backward.search {FSelector}
#
# the script is supposed to be run on a cluster see http://www.bioconductor.org/help/bioconductor-cloud-ami/#parallel
# for how to quickly set up an R cluster
#

# launch and test cluster -------------------------------------------------

# cluster set up
library(parallel)
lines <- readLines("/usr/local/Rmpi/hostfile.plain")
hosts <- character()
for (line in lines) {
  x <- (strsplit(line[[1]], " "))
  hosts <- c(hosts, rep.int(x[[1]][1], as.integer(x[[1]][2])))
}

# outfile='' enable to have slave output in the master console
cl <- makePSOCKcluster(hosts, master=system("hostname -i", intern=TRUE), outfile='') 

# cluster test - simple call
system.time(clusterCall(cl, Sys.sleep, 1))

# register the cluster
library(doParallel)
registerDoParallel(cl)

# check if foreach knows the cluster
getDoParWorkers()
getDoParName()
getDoParVersion()
getDoParRegistered()

# cluster test - dummy foreach call
m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) %dopar% (m[i,] / mean(m[i,]))

# backward selection logic -------------------------------------------------

# load data 
load("dat.RData")

# set up the input dataset
train_cases <- which(!is.na(dat["VARIABLE_CIBLE"]))
target <- which(names(dat) == "VARIABLE_CIBLE")
dat_train_cases <- dat[train_cases, c(which(names(dat) %in% best_feat), target)]

#prepare the cluster
clusterExport(cl, "dat_train_cases")
clusterEvalQ(cl, {library(randomForest)})
clusterEvalQ(cl, {(.packages())})

# load required packages on the master
library(FSelector)
library(ROCR)

# to avoid to pass it as an argument in the evaluator function
attach(dat_train_cases)

evaluator <- function(subset) {
  k <- 3 # 3-folds cross validation
  p <- 3 # 3 trees per worker ie 120
  nvar <- 5 # 5 features randomly selected at each node
  splits <- runif(nrow(dat_train_cases))
  results <- sapply(1:k, function(i) {
    test_idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train_idx <- !test_idx  
    clusterExport(cl, c("train_idx", "test_idx", "subset", "p", "nvar"), envir=environment())
    votes <- clusterEvalQ(cl, {
      to_keep <- which(names(dat_train_cases) %in% subset)
      target <- which(names(dat_train_cases) == "VARIABLE_CIBLE")
      xx <- dat_train_cases[train_idx, to_keep]
      yy <- dat_train_cases[train_idx, target]
      rf <- randomForest(x=xx, y=yy, ntree=p, mtry=nvar)
      predict(rf, newdata=dat_train_cases[test_idx, to_keep], type="vote", mtry=nvar, norm.votes=FALSE)
    })
    probs <- votes[[1]]
    for(j in 2:length(votes)) probs <- probs + votes[[j]]
    probs <- probs/(p * getDoParWorkers())
    target <- which(names(dat_train_cases) == "VARIABLE_CIBLE")
    y_new <- dat_train_cases[test_idx, target]
    actual_y <- as.numeric(y_new == "GRANTED")
    probs_y_equals_1 <- as.numeric(probs[, 1])  
    pred <- prediction(probs_y_equals_1, actual_y)
    roc <- performance(pred, "auc")
    aroc <- as.numeric(roc@y.values)
    print(paste("fold", paste(i, aroc, sep=" : "), sep=" "))
    return(aroc)
  }
  )
  print(length(subset))
  print(mean(results))
  return(mean(results))
}

splits_var <- runif(length(best_feat)); m <- 5
subset_list <- vector(mode="list", length=m)

for(l in 1:m){
  indices <- (splits_var >= (l - 1) / m) & (splits_var < l / m)
  system.time(
    subset_list[[l]] <- backward.search(best_feat[indices], evaluator)
  )
}

best_features_backward <- do.call("c", subset_list)

# write output and close the cluster --------------------------------------

save(subset_list, splits_var, best_features_backward, file="backward_selection.RData")

stopCluster(cl)
