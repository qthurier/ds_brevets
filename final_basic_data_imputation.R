# BASIC DATA IMPUTATION #
#########################

#options(warn=-1) colinearity: NB_BACKWARD_PL,NB_BACKWARD ligne 91

# simple imputation with impute() {Hmisc} 
# default imputed value is median or the most frequent value for factors
library(Hmisc)
for(i in 1:ncol(dat)){
  if(any(is.na(dat[, i]))) {
    new_col_name <- paste("simp_imp", names(dat)[i], sep="_")
    if(class(dat[, i])[1] == 'integer') dat[, new_col_name] <- as.integer(impute(dat[, i]))
    else if(class(dat[, i])[1] == 'numeric') dat[, new_col_name] <- as.numeric(impute(dat[, i]))
    else if(class(dat[, i])[1] == 'factor' && names(dat)[i] != 'VARIABLE_CIBLE'){
      dat[, new_col_name] <- as.factor(impute(dat[, i]))
      class(dat[, new_col_name]) <- "factor"
    }
  }  
}

# nearest neighbor imputation with impute() {yaImpute} 
library(yaImpute)

# get the class for each feature
class_by_col_num <- as.vector(matrix(data=sapply(dat, GetClass), ncol=ncol(dat)))

# distinguish each feature type
fact_feat <- which(class_by_col_num == 'factor')
date_feat <- which(class_by_col_num == "POSIXlt")
imp_feat <- grep("imp", names(dat))
target <- which(names(dat) == "VARIABLE_CIBLE")

# this feature need to be exclude because its
# variance is zero, which will raise error when
# impute() is calculating normalized distance
cited_nmiss <- which(names(dat) %in% "cited_nmiss")
var_null <- which(GetNbOfDistinctValues(dat)<2)

# indices of the features to remove, 1 corresponds to the id
remove <- unique(c(1, cited_nmiss, date_feat, imp_feat, target, var_null))

# z is dat without the exluded features
z <- dat[, -remove]; row.names(z) <- row.names(dat)

# which are features with missing values in z ?
feat_wth_na <- as.numeric(which(sapply(z, function(x) any(is.na(x)))))

# impute() expect two types of cases: 
# - the references (cases without any missing values)
# - the targets (cases which are not complete)
references <- row.names(z)[which(complete.cases(z))]

if(length(references) > 0) {
  
  print(paste("nb of references ", length(references), sep=""))
  print("start knn imputation")
  
  targets <- row.names(z)[which(!complete.cases(z))]
  
  # get the class for each feature in z, and get the indices of the factors
  class_by_col_num_z <- as.vector(matrix(data=sapply(z, GetClass), ncol=ncol(z)))
  fact_feat_z <- which(class_by_col_num_z == 'factor')
  
  # y contains the complete cases for the features with missing values in z
  y <- z[which(complete.cases(z)), feat_wth_na]
  
  # check for which feature knn imputation is a poor method
  # note : only 1 value among targets for SOURCE_BEGIN_MONTH
  GetNbOfDistinctValues(y) 
  
  # get the class for each feature in zy, and get the indices of the factors
  class_by_col_num_y <- as.vector(matrix(data=sapply(y, class), ncol=ncol(y)))
  fact_feat_y <- which(class_by_col_num_y == 'factor')
  
  library(multicore); library(parallel)
  
  to_skip_z <- union(feat_wth_na, fact_feat_z)
  k <- 7
  knn <- yai(x=z[, -to_skip_z], y=y[, -fact_feat_y], method="mahalanobis", k=k)
  
  # make a dataframe with the k neighbors for each targets
  neighbors <- data.frame(row.names=row.names(z))
  for(i in 1:k){
    #best <- foruse(knn, kth=i, targetsOnly=TRUE)
    best <- foruse(knn, kth=i)
    neighbors <- merge(neighbors, best[, "use", drop=FALSE], by=0)
    row.names(neighbors) <- neighbors[, 'Row.names']
    neighbors <- neighbors[, -1, drop=FALSE]
    names(neighbors)[i] <- paste("nn", i, sep="")
  }
  
  for(i in feat_wth_na){
    feat_name <- names(z)[i]
    print(feat_name)
    if(class(z[, i]) == 'factor') f <- factor(levels=levels(z[, i]))
    if(class(z[, i]) == 'numeric') f <- vector(mode='numeric', length=nrow(database))
    if(class(z[, i]) == 'integer') f <- vector(mode='integer', length=nrow(database))
    f[as.integer(references)] <- z[complete.cases(z), i]
    imp_fact_list <- lapply(RandomSplitCases(n_fold=detectCores(), 1:nrow(neighbors)), 
                            FUN=GetKnnValue, nghb=neighbors, 
                            fact=f, fact_name=feat_name)
    imp_fact <- do.call("rbind", imp_fact_list)
    imp_fact <- imp_fact[order(as.integer(row.names(imp_fact))), ]
    new_col_name <- paste("knn_imp", feat_name, sep="_")
    dat[new_col_name] <- dat[feat_name]
    missing <- which(is.na(dat[feat_name]))
    dat[missing, new_col_name] <- imp_fact[missing]
  }

} else {
  
  print("no references - no knn imputation")

}
