
# to avoid format error on dates we filter dat
class_by_col_num <- as.vector(matrix(data=sapply(dat, GetClass), ncol=ncol(dat)))
dat_filtered <- dat[, -which(class_by_col_num %in% "POSIXlt")]

# distinguish each types of feature
class_by_col_num <- as.vector(matrix(data=sapply(dat_filtered, GetClass), ncol=ncol(dat_filtered)))
num_feat <- which(class_by_col_num %in% c("integer", "numeric"))
fact_feat <- which(class_by_col_num %in% 'factor')
feat_wth_na <- which(sapply(dat_filtered, function(y) any(is.na(y))))
target <- which(names(dat_filtered) == "VARIABLE_CIBLE")

# which features do we need to keep ?
fact_to_keep <- GetSomeFactors(dat_filtered, 2, 700)
fact_to_keep <- setdiff(fact_to_keep, c(target, feat_wth_na))
num_to_keep <- setdiff(num_feat, c(1, feat_wth_na))
to_keep <- union(fact_to_keep, num_to_keep)

# select only the cases from the test set
train_cases <- which(!is.na(dat_filtered["VARIABLE_CIBLE"]))

# we skip features without information
var_null <- which(GetNbOfDistinctValues(dat_filtered) < 2)
to_keep <- setdiff(to_keep, var_null)
x <- dat_filtered[, c(to_keep, target)]

# we calculate the information gain criteria thanks to the package {FSelector} 
# (wich is based on Weka) for each features and we keep the best ones
library(FSelector)

# split the process to avoid java heap space
# note : behind FSelector Weka java library is running
x_part1 <- x[, c(1:30, ncol(x))]
x_part2 <- x[, c(31:60, ncol(x))]
x_part3 <- x[, c(61:90, ncol(x))]
x_part4 <- x[, c(91:120, ncol(x))]
x_part5 <- x[, 121:ncol(x)]

system.time(infoGain_part1 <- information.gain(VARIABLE_CIBLE ~ . , x_part1))
system.time(infoGain_part2 <- information.gain(VARIABLE_CIBLE ~ . , x_part2))
system.time(infoGain_part3 <- information.gain(VARIABLE_CIBLE ~ . , x_part3))
system.time(infoGain_part4 <- information.gain(VARIABLE_CIBLE ~ . , x_part4))
system.time(infoGain_part5 <- information.gain(VARIABLE_CIBLE ~ . , x_part5))

# merge all the parts of infoGain
infoGain <- rbind(infoGain_part1, infoGain_part2, infoGain_part3, infoGain_part4, infoGain_part5)

plot(x=1:nrow(infoGain), 
     infoGain$attr_importance[order(infoGain$attr_importance, decreasing=TRUE)],
     type='l',
     xlab="features rank in terms of information gain",
     ylab='information gain values',
     main="Information gain decrease along with features")

best_ones <- which(infoGain$attr_importance[order(infoGain$attr_importance, decreasing=TRUE)]>0)
best_features_info_gain <- row.names(infoGain)[order(infoGain$attr_importance, decreasing=TRUE)][best_ones]
