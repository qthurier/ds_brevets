library(Hmisc)

# distinguish each types of feature
class_by_col_num <- as.vector(matrix(data=sapply(dat, GetClass), ncol=ncol(dat)))
feat_wth_na <- which(sapply(dat, function(y) any(is.na(y))))
train_cases <- which(!is.na(dat$VARIABLE_CIBLE))
y <- as.numeric(dat[train_cases, "VARIABLE_CIBLE"] == 'GRANTED')
fact_to_turn <- setdiff(GetSomeFactors(dat, min=33, max=100000), feat_wth_na)

for(i in fact_to_turn){
  feat_name <- names(dat)[i]
  fact <- factor(dat[train_cases, feat_name]) # to get rid of the levels from the test cases
  fact_to_num <- tapply(y, fact, mean)
  num_feat <- as.numeric(sapply(list(dat$knn_imp_FISRT_INV_COUNTRY), function(x) fact_to_num[x]))
  new_col_name <- paste("fact_to_num", feat_name, sep="_")
  dat[, new_col_name] <- as.numeric(impute(num_feat)) # median imputation for levels which exist only in test cases
}