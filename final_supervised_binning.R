source("final_utils.R")
load("../data/final_input.RData")

library(discretization); library(foreach)

# set cran mirror
clusterEvalQ(cl, { options(repos=structure(c(CRAN="http://cran.fhcrc.org/"))) })

# set lib path to install packages
clusterEvalQ(cl, { .libPaths( c('/home/ubuntu/R/library', .libPaths()) ) })

# install required packages on the cluster
clusterEvalQ(cl, { install.packages("discretization") }) 
clusterEvalQ(cl, { library(discretization) })
clusterEvalQ(cl, { (.packages()) })

# select feature to bin
class_by_col_num <- as.vector(matrix(data=sapply(dat, GetClass), ncol=ncol(dat)))
dat_feat <- which(class_by_col_num == "POSIXlt")

dat2 <- dat[, -dat_feat]
class_by_col_num <- as.vector(matrix(data=sapply(dat2, GetClass), ncol=ncol(dat2)))
num_feat <- which(class_by_col_num %in% c("integer", "numeric"))
feat_wth_na <- which(sapply(dat2, function(y) any(is.na(y))))
fact_to_num <- grep("fact_to_num", names(dat2))
age_feat <- grep("age", names(dat2))
pct_feat <- grep("pct", names(dat2))
to_keep <- setdiff(num_feat, c(1, feat_wth_na, fact_to_num, age_feat, pct_feat))
target <- which(names(dat2) == "VARIABLE_CIBLE")
num_to_bin <- names(which(GetNbOfDistinctValues(x=dat2[, to_keep])>1))

ds <- dat2[, c(which(names(dat2) %in% num_to_bin), target)]

clusterExport(cl, "ds")

discretize <- function(theta) {
  feat <- theta[1]
  type <- theta[2]
  train_idx <- which(!is.na(ds$VARIABLE_CIBLE))
  data <- ds[, c(which(names(ds) == feat), which(names(ds) == "VARIABLE_CIBLE"))]
  disc <- switch(type,
                 chi2 = chi2(data),
                 topdown1 = disc.Topdown(data[train_idx, ], method=1),
                 topdown2 = disc.Topdown(data[train_idx, ], method=2),
                 topdown3 = disc.Topdown(data[train_idx, ], method=3),
                 extendchi2 = extendChi2(data[train_idx, ]),
                 modchi2 = modChi2(data[train_idx, ]),
                 mdlp = mdlp(data[train_idx, ]),
                 chim = chiM(data[train_idx, ]))
  m <- min(data[, feat])
  M <- max(data[, feat])
  brk <- c(m - 1, as.numeric(disc$cutp[[1]]), M + 1)
  bin_feat <- cut(as.numeric(data[, feat]), breaks=brk, include.lowest=FALSE)
  str_part_1 <- paste(feat, type, sep=" - ")
  str_part_2 <- paste(str_part_1, nlevels(bin_feat), sep=" - ")
  print(str_part_2)
  out <- vector(mode='list', length=1)
  new_col_name <- paste(paste("bin", type, sep="_"), feat, sep="_")
  names(out) <- new_col_name
  out[[1]] <- if(nlevels(bin_feat) > 1 && nlevels(bin_feat) < 33) bin_feat else NULL
  return(out)
}

#meth_list <- c("chi2", "topdown1", "topdown2", "topdown3", "extendchi2", "modchi2", "mdlp", "chim")
meth_list <- "chim"
grid <- cbind(rep(num_to_bin, length(meth_list)), rep(meth_list,each=length(num_to_bin)))

grid_list <- vector(mode="list", length=nrow(grid))
for(i in 1:length(grid_list)) grid_list[[i]] <- grid[i, ]

# test
a <- vector(mode="list", length=2)
a[[1]] = grid_list[[3]]
a[[2]] = grid_list[[1]]
bin_feat_list <- parLapplyLB(cl, a, discretize)

system.time(
  bin_feat_list_chim <- parLapplyLB(cl, grid_list, discretize)
)

bin_feat_list <- bin_feat_list_chim[which(sapply(bin_feat_list_chim, function(x) length(x)>0))]
bin_feat <- do.call("data.frame", bin_feat_list)
