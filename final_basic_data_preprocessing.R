# BASIC DATA PRE-PROCESSING #
#############################

# this configuration makes sense and doesn't rise an error with scan()
classes <- c("integer","factor","factor","factor","numeric","numeric","numeric","factor","factor","factor",
             "factor","integer","integer","integer","integer","factor","factor","factor","numeric","numeric",
             "numeric","factor","factor","numeric","numeric","integer","numeric","integer","numeric","numeric",
             "factor","integer","integer","integer","integer","integer","integer","numeric","numeric","numeric",
             "numeric","numeric","numeric","factor","numeric","factor","factor","factor","factor","factor","factor")

# data import with the previous configuration
train <- read.csv("data/brevets_train.csv", sep=";", colClasses=classes, na.strings=c("", "(MISSING)"))
test <- read.csv("data/brevets_test.csv", sep=";", colClasses=classes[-which(names(train) == "VARIABLE_CIBLE")], na.strings=c("", "(MISSING)"))

# few workaround to get the target as the last column and to make the two sets mergeable
target_index <- which(names(train) == "VARIABLE_CIBLE")
permutation <- c((1:ncol(train))[-target_index], target_index)
train <- train[, permutation]
test["VARIABLE_CIBLE"] <- NA

# horizontally merge test and train sets
database <- rbind(train, test)

# little workaround, some features make more sense as integer objects
# but they can't be set up as integer in the previous step due to the raw
# databasea format (ex: '1.0' raises an error when scan() is reading an integer feature)
database$APP_NB <- as.integer(database$APP_NB)
database$APP_NB_PAYS <- as.integer(database$APP_NB_PAYS)
database$APP_NB_TYPE <- as.integer(database$APP_NB_TYPE)
database$INV_NB <- as.integer(database$INV_NB)
database$INV_NB_PAYS <- as.integer(database$INV_NB_PAYS)
database$INV_NB_TYPE <- as.integer(database$INV_NB_TYPE)
database$cited_n <- as.integer(database$cited_n)
database$cited_nmiss <- as.integer(database$cited_nmiss)

# databasees formating
database$PRIORITY_MONTH <- strptime(sapply(database$PRIORITY_MONTH, MakePosixCompliant), format='%d/%m/%Y:%H:%M:%S')
database$FILING_MONTH <- strptime(sapply(database$FILING_MONTH, MakePosixCompliant), format='%d/%m/%Y:%H:%M:%S')
database$PUBLICATION_MONTH <- strptime(sapply(database$PUBLICATION_MONTH, MakePosixCompliant), format='%d/%m/%Y:%H:%M:%S')
database$BEGIN_MONTH <- strptime(sapply(database$BEGIN_MONTH, MakePosixCompliant), format='%d/%m/%Y:%H:%M:%S')

# get the class for each feature
class_by_col_num <- as.vector(matrix(data=sapply(database, GetClass), ncol=ncol(database)))

# split databasees in month / year
for(i in 1:ncol(database)){
  if(class_by_col_num[i] == "POSIXlt"){
    month <- format(database[, i], "%b"); 
    new_col_name <- gsub("MONTH", "M", names(database)[i])
    database[new_col_name] <- factor(month)
    new_col_name <- gsub("MONTH", "Y", names(database)[i])
    year <- format(database[, i], "%Y"); #
    database[new_col_name] <- factor(year)
  }
}

# numeric / integer features with missing values
names(database)[sapply(database, function(x) if((class(x)[1] == 'numeric' || class(x)[1] == 'integer'))  any(is.na(x)) else FALSE)]

# if there are to few distinct values numerical features may be 
# turned to factors specially if we want to do imputation by random forest
length(unique(database$APP_NB)) # ok
length(unique(database$APP_NB_PAYS)) # ok
length(unique(database$APP_NB_TYPE)) # only 5 distinct values, we can convert to factor for random forest imputation
length(unique(database$INV_NB)) # ok
length(unique(database$INV_NB_PAYS)) # ok
length(unique(database$INV_NB_TYPE)) # only 5 distinct values, we can convert to factor for random forest imputation
length(unique(database$cited_n)) # ok
length(unique(database$cited_nmiss)) # only NA or 0, this feature will be convert as a factor with NA as a level in the next section
length(unique(database$cited_age_std)) # ok
length(unique(database$pct_NB_IPC_LY)) # ok

# factor conversion for numerical features
database$APP_NB_TYPE_fact <- as.factor(database$APP_NB_TYPE)
database$INV_NB_TYPE_fact <- as.factor(database$INV_NB_TYPE)

# factors with missing values
names(database)[sapply(database, function(x) if(class(x)[1] == 'factor') any(is.na(x)) else FALSE)]

# for factors NA can be considered as a value 
# in itself if we observed to cases with missing values  
# let's put a threshold at 5% of the databaseaset
length(which(is.na(database$COUNTRY))) # ok
length(which(is.na(database$FISRT_APP_COUNTRY))) # nok
length(which(is.na(database$FISRT_APP_TYPE))) # ok
length(which(is.na(database$LANGUAGE_OF_FILLING))) # nok
length(which(is.na(database$FISRT_INV_COUNTRY))) # nok
length(which(is.na(database$FISRT_INV_TYPE))) # ok
length(which(is.na(database$PRIORITY_M))) # ok
length(which(is.na(database$PRIORITY_Y))) # ok
length(which(is.na(database$APP_NB_TYPE_fact))) # nok
length(which(is.na(database$INV_NB_TYPE_fact))) # nok
length(which(is.na(database$cited_nmiss))) # ok

database$COUNTRY_na_as_val[is.na(database$COUNTRY)] <- 'MISSING'; database$COUNTRY_na_as_val[!is.na(database$COUNTRY)] <- as.character(database$COUNTRY[!is.na(database$COUNTRY)]); database$COUNTRY_na_as_val <- as.factor(database$COUNTRY_na_as_val)
database$FISRT_APP_TYPE_na_as_val <- 'MISSING'; database$FISRT_APP_TYPE_na_as_val[!is.na(database$FISRT_APP_TYPE)] <- database$FISRT_APP_TYPE[!is.na(database$FISRT_APP_TYPE)]; database$FISRT_APP_TYPE_na_as_val <- as.factor(database$FISRT_APP_TYPE_na_as_val)
database$FISRT_INV_TYPE_na_as_val <- 'MISSING'; database$FISRT_INV_TYPE_na_as_val[!is.na(database$FISRT_INV_TYPE)] <- database$FISRT_INV_TYPE[!is.na(database$FISRT_INV_TYPE)]; database$FISRT_INV_TYPE_na_as_val <- as.factor(database$FISRT_INV_TYPE_na_as_val)
database$PRIORITY_M_na_as_val <- 'MISSING'; database$PRIORITY_M_na_as_val[!is.na(database$PRIORITY_M)] <- database$PRIORITY_M[!is.na(database$PRIORITY_M)]; database$PRIORITY_M_na_as_val <- as.factor(database$PRIORITY_M_na_as_val)
database$PRIORITY_Y_na_as_val <- 'MISSING'; database$PRIORITY_Y_na_as_val[!is.na(database$PRIORITY_Y)] <- database$PRIORITY_Y[!is.na(database$PRIORITY_Y)]; database$PRIORITY_Y_na_as_val <- as.factor(database$PRIORITY_Y_na_as_val)
database$cited_nmiss_fact_na_as_val <- 'MISSING'; database$cited_nmiss_fact_na_as_val[!is.na(database$cited_nmiss)] <- database$cited_nmiss[!is.na(database$cited_nmiss)]; database$cited_nmiss_fact_na_as_val <- as.factor(database$cited_nmiss_fact_na_as_val)
