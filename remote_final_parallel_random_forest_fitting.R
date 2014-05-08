# assuming that dat.RData contains
# 
# - dat : a data frame with a target called VARIABLE_CIBLE
# - best_feat : ordered list of the best features to predict the target
#
# this script fit a random forest model and get the predictions on the test set
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

# fit a random forest and retrive predictions ----------------------------------------------------------------

# load data 
load("dat.RData")

# variable selection
target <- which(names(dat) == "VARIABLE_CIBLE")
to_keep <- which(names(dat) %in% best_feat)

# prepare train cases
train_cases <- which(!is.na(dat["VARIABLE_CIBLE"]))
x <- dat[train_cases, to_keep]
y <- dat[train_cases, target]

# prepare test cases
test_cases <- which(is.na(dat["VARIABLE_CIBLE"]))
new <- dat[test_cases, to_keep]

# how many tree per worker?
p <- 30

# .. and how many features to select at each tree node
nvar <- 5

# export data to the cluster
clusterEvalQ(cl, {rm(list=ls())})
clusterExport(cl, c("x", "y", "new", "p", "nvar"))

# grow trees on each nodes
system.time(
  votes <- clusterEvalQ(cl, {
    library("randomForest")
    rf <- randomForest(x, y, ntree=p, mtry=nvar)
    predict(rf, newdata=new, type="vote", mtry=nvar, norm.votes=FALSE)
  })
)

# calculate probs
probs <- votes[[1]]
for(i in 2:length(votes)) probs <- probs + votes[[i]]
probs <- probs/(p * getDoParWorkers())

# write output and close the cluster --------------------------------------

# write output for submission
write.table(probs[, 1], file="submission.txt", col.names=FALSE, row.names=FALSE)

# stop the cluster
stopCluster(cl)