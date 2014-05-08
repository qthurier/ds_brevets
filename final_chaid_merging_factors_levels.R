library(CHAID); library(multicore)

# get the class for each feature
class_by_col_num <- as.vector(matrix(data=sapply(dat, GetClass), ncol=ncol(dat)))

to_group <- {}
maxlevels <- 600 # exclude MAIN_IPC and FIRST_CLASSE which take too long or raise errors
minlevels <- 32 # randomforest() limit
for(i in 1:ncol(dat)) {
  if(class_by_col_num[i] == 'factor'){
    if(nlevels(dat[, i]) > minlevels && nlevels(dat[, i]) < maxlevels) {
      print(paste(i, nlevels(dat[, i]), collapse=" - "))
      to_group <- c(to_group, i)
    }
  }
}

y_index <- which(names(dat) == "VARIABLE_CIBLE")
ctrl <- chaid_control(stump=TRUE)
seq_list <- vector(mode="list", length=length(to_group))
seq_list[1:length(to_group)] <- to_group
new_fact_list <- mclapply(seq_list, 
                          FUN=GetNewFactor, z=dat, 
                          y=y_index, chaid_ctrl=ctrl)

# add reduced factors into dat
for(i in 1:length(to_group)){
  if(length(new_fact_list[[i]]) > 0) {
    new_col_name <- paste("chaid_merge", names(dat)[to_group[i]], sep="_")
    dat[new_col_name] <- new_fact_list[[i]]
  }
}
