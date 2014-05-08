# assuming that dat.RData contains
# 
# - dat : a data frame with a target called VARIABLE_CIBLE
# - best_feat : ordered list of the best features to predict the target
#
# this script set up a grid optimization strategy to get the best couple 
# (nb variables, nb trees) for a random forest model (based on the auc criteria)
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

# grid search logic ----------------------------------------------------------------

library(ROCR)

evaluator <- function(theta, k=3) {
  nvar <- theta[1]
  p <- theta[2] 
  splits <- runif(nrow(dat_train_cases))
  results <- sapply(1:k, function(i) {
    test_idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train_idx <- !test_idx  
    clusterExport(cl, c("train_idx", "test_idx", "p", "nvar"), envir=environment())
    votes <- clusterEvalQ(cl, {
      target <- which(names(dat_train_cases) == "VARIABLE_CIBLE")
      xx <- dat_train_cases[train_idx, -target]
      yy <- dat_train_cases[train_idx, target]
      rf <- randomForest(x=xx, y=yy, ntree=p, mtry=nvar)
      predict(rf, newdata=dat_train_cases[test_idx, -target], type="vote", mtry=nvar, norm.votes=FALSE)
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
  print(mean(results)) 
  return(mean(results))
}

# launch the grid search, save result, close the cluster --------------------------------------

load("dat.RData") 

# set cran mirror
clusterEvalQ(cl, { options(repos=structure(c(CRAN="http://cran.fhcrc.org/"))) })

# set lib path to install packages
clusterEvalQ(cl, { .libPaths( c('/home/ubuntu/R/library', .libPaths()) ) })

# install required packages on the cluster
clusterEvalQ(cl, { install.packages("ROCR") })

clusterEvalQ(cl, { library(ROCR) })
clusterEvalQ(cl, { library(randomForest) })

p_list <- seq(from=10,to=30,by=4)
nvar_list <- seq(from=5,to=25,by=2)
grid <- cbind(rep(nvar_list,length(p_list)), rep(p_list,each=length(nvar_list)))

# variable selection
target <- which(names(dat) == "VARIABLE_CIBLE")
to_keep <- which(names(dat) %in% best_feat)

# prepare train cases
train_cases <- which(!is.na(dat["VARIABLE_CIBLE"]))
dat_train_cases <- dat[train_cases, c(to_keep, target)]

clusterEvalQ(cl, {rm(list=ls())})
clusterExport(cl, "dat_train_cases")

system.time(
  res <- apply(grid, 1, evaluator)
)

save(res, grid, file="parameter_optimization.RData")

stopCluster(cl)
