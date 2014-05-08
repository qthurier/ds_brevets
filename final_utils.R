MakePosixCompliant <- function(x) { 
  with_day <- paste("01", x, sep="/"); 
  if(!is.na(x)) paste(with_day, "00:00:00", sep=":") 
  else NA 
}

GetClass  <- function(x) {
  class(x)[1]
}

GetFeaturesWthNA <- function(x){
  names(x)[sapply(x, function(y) any(is.na(y)))]  
}

GetNumFeaturesWthNA <- function(x){
  which(sapply(x, function(y) any(is.na(y))))
}

GetNbOfDistinctValues <- function(x){
  #sapply(x, function(y) if(class(y)[1]=='factor') nlevels(y) else length(unique(y)))
  sapply(x, function(y) length(unique(y)))
}

GetSomeFactors <- function(x, min, max){
  class_by_col_num <- as.vector(matrix(data=sapply(x, GetClass), ncol=ncol(x)))
  to_keep <- {}
  for(i in 1:ncol(x)) {
    if(class_by_col_num[i] == 'factor'){
      if(nlevels(x[, i]) >= min && nlevels(x[, i]) <= max) {
        to_keep <- c(to_keep, i)
      }
    }
  }
  return(to_keep)
}

#SplitCases <- function(n_bags, m){
#  bag_size <- m %/% n_bags
#  if(m %% n_bags > 0) n_bags <- n_bags + 1
#  lapply(1:n_bags, function(x) 1 + ((x-1)*bag_size):(min(m, x*bag_size)-1))
#}

RandomSplitCases <- function(n_fold, seq){
  #seq <- 1:m
  m <- length(seq)
  v <- vector(mode="numeric", length=n_fold)
  seq_list <- NULL
  for(i in 1:n_fold) {
    if(length(seq) > round(m/n_fold)) indices <- sample(seq, size=round(m/n_fold))
    else indices <- seq
    seq <- setdiff(seq, indices)
    if(i == n_fold && length(seq)>0) indices <- c(indices, seq)
    seq_list[[i]] <- indices
  }
  return(seq_list)
}

LocateReferences <- function(x, ref) {
  where <- which(row.names(x) %in% ref)
  return(x[-where, ])
}


# parallel functions

# notes on the yai constructor :
# - it doesn't expect factors (but we had keep them to define the targets cases)
# - x argument shouldn't content any missing values (because similarity is calculated on x)
# - k is the number of neighbors
KnnImputation <- function(x, ref, zz, yy, k, na, fact_z, fact_y) {
  to_skip_z <- union(na, fact_z)
  knn <- yai(x=zz[c(x, ref), -to_skip_z], y=yy[, -fact_y], method="mahalanobis", k=k)
  impute(knn, k=k, method='median', observed=FALSE)
}

GetKnnValue <- function(x, nghb, fact, fact_name) {
  neighbors_val <- t(apply(nghb[x, ], 1, function(y) fact[as.numeric(y)]))
  if(class(fact) == 'factor'){
    out <- data.frame(apply(neighbors_val, 1, function(z) names(which.max(table(z)))))  
  } else {
    out <- data.frame(apply(neighbors_val, 1, function(z) median(z)))  
  }
  names(out) <- fact_name
  return(out)
}

# print(i)
# feat_name <- names(z)[i]
# f_reordered <- z[order(as.integer(row.names(z))), i]
# imp_fact_list <- lapply(RandomSplitCases(n_fold=detectCores(), 1:nrow(neighbors)), 
#                         FUN=GetKnnFactValue, nghb=neighbors, 
#                         fact=f_reordered, fact_name=feat_name)
# toto <- RandomSplitCases(n_fold=detectCores(), 1:nrow(neighbors))[[1]]
# neighbors_val <- t(apply(neighbors[toto, ], 1, function(y) f[as.numeric(y)]))
# 
# out <- data.frame(apply(neighbors_val, 1, function(z) names(which.max(table(z)))))

GetMergedLevels <- function(x, grouping, old_levels){
  return(old_levels[which(grouping %in% x)])  
}

GetNewFactor <- function(x, z, y, chaid_ctrl) {
  print(x)
  chaid_merging <- chaid(VARIABLE_CIBLE ~ . , data=z[, c(x, y)], control=chaid_ctrl)
  out <- NULL
  if(depth(chaid_merging) > 0){
    chaid_nodes <- node_party(chaid_merging)
    chaid_split_nodes <- split_node(chaid_nodes)
    levels_grouping <- index_split(chaid_split_nodes)
    old_fact <- z[, x]
    n_new_levels <- max(levels_grouping)
    seq_list <- vector(mode="list", length=n_new_levels)
    seq_list[1:n_new_levels] <- 1:n_new_levels
    merged_levels <- lapply(seq_list, FUN=GetMergedLevels, old_levels=levels(old_fact), grouping=levels_grouping)
    complete <- complete.cases(old_fact)
    new_fact <- vector(mode="character", length=length(old_fact))
    new_fact[complete] <- fitted_node(chaid_nodes, data=z[complete, c(x, y)])
    new_fact[!complete] <- NA
    new_fact <- as.factor(new_fact)
    levels(new_fact) <- sapply(merged_levels, function(x) paste(x, collapse=", "))
    out <- new_fact
  }
  return(out)
}

