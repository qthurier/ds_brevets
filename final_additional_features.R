# ADDITIONAL FEATURES #
#######################

# we try to add new features that could be relevant :
#
# - boolean comparison between factors with the same levels (typically for countries)
# - ratio that are never higher than 1 between numerical features with the same unit
# - variation coefficient (ie std/mean a relative measure of dispersion)
# - differences that are almost never negative (typically for dates)
# - sum of numerical related features with same unit

# note of additional features based on features with missing data :
#
# we use the knn imputed features instead of mode imputed feature
# this choice will lead to more variability in the aditional features

if(length(grep("knn", names(dat)))>0){
  # factor features #
  
  # countries
  dat["added_ctry_vs_first_app_ctry"] <- factor(as.character(dat$knn_imp_COUNTRY) != as.character(dat$knn_imp_FISRT_APP_COUNTRY))
  dat["added_first_inv_ctry_vs_first_app_ctry"] <- factor(as.character(dat$knn_imp_FISRT_INV_COUNTRY) != as.character(dat$knn_imp_FISRT_APP_COUNTRY))
  dat["added_first_inv_ctry_vs_ctry"] <- factor(as.character(dat$knn_imp_FISRT_INV_COUNTRY) != as.character(dat$knn_imp_COUNTRY))
  
  # types
  dat["added_first_app_type_vs_first_inv_type"] <- factor(as.character(dat$knn_imp_FISRT_APP_TYPE) != as.character(dat$knn_imp_FISRT_INV_TYPE))
  
  # sources
  # note : SOURCE_CITED_AGE always equals SOURCE_IDX_ORI so we just need to compare SOURCE_IDX_RAD vs SOURCE_IDX_ORI or SOURCE_CITED_AGE
  length(which(as.character(dat$SOURCE_CITED_AGE) != as.character(dat$SOURCE_IDX_ORI)))
  dat["added_source_idx_rad_vs_source_idx_ori"] <- factor(as.character(dat$SOURCE_IDX_RAD) != as.character(dat$SOURCE_IDX_ORI))
  
  # numerical features #
  
  # applicants
  dat["added_app_nb_pays_on_app_nb"] <- dat$knn_imp_APP_NB_PAYS/dat$knn_imp_APP_NB; max(dat["added_app_nb_pays_on_app_nb"])
  dat["added_app_nb_type_on_app_nb"] <- dat$knn_imp_APP_NB_TYPE/dat$knn_imp_APP_NB; max(dat["added_app_nb_type_on_app_nb"])
  
  # classes
  dat["added_app_nb_root_classes_on_nb_classes"] <- dat$NB_ROOT_CLASSES/dat$NB_CLASSES; max(dat["added_app_nb_root_classes_on_nb_classes"])
  
  # technologies
  dat["added_nb_sectors_on_nb_fields"] <- dat$NB_SECTORS/dat$NB_FIELDS; max(dat["added_nb_sectors_on_nb_fields"])
  
  # investment
  dat["added_inv_nb_pays_on_inv_nb"] <- dat$knn_imp_INV_NB_PAYS/dat$knn_imp_INV_NB; max(dat["added_inv_nb_pays_on_inv_nb"])
  dat["added_inv_nb_type_on_inv_nb"] <- dat$knn_imp_INV_NB_TYPE/dat$knn_imp_INV_NB; max(dat["added_inv_nb_type_on_inv_nb"])
  
  # backward
  # note : NB_BACKWARD is not the sum of all the rest 'backward' features so we create it
  mean(dat$NB_BACKWARD_NPL + dat$NB_BACKWARD_XY + dat$NB_BACKWARD_I + dat$NB_BACKWARD_AUTRE + dat$NB_BACKWARD_PL); mean(dat$NB_BACKWARD)
  dat["added_all_backward"] <- dat$NB_BACKWARD_NPL + dat$NB_BACKWARD_XY + dat$NB_BACKWARD_I + dat$NB_BACKWARD_AUTRE + dat$NB_BACKWARD_PL
  
  # cited age
  dat["added_cited_age_max_minus_min"] <- dat$cited_age_max - dat$cited_age_min; range(dat["added_cited_age_max_minus_min"])
  dat["added_cited_age_mean_on_std"] <-dat$knn_imp_cited_age_std/dat$cited_age_mean; range(dat["added_cited_age_mean_on_std"])
  
  if(any(is.nan(dat[, "added_cited_age_mean_on_std"]))) {
    dat[which(is.nan(dat[, "added_cited_age_mean_on_std"])) , "added_cited_age_mean_on_std"] <- 0
  }
  
  # little workaround : due to imputet value the ratio could be infinite, we put it at the double of the max if so
  if(range(dat["added_cited_age_mean_on_std"])[2] == Inf ) {
    inf <-  which(dat["added_cited_age_mean_on_std"]==Inf)
    max_without_inf <- range(dat[-inf, "added_cited_age_mean_on_std"])[2]
    dat[inf, "added_cited_age_mean_on_std"] <- 2*max_without_inf; range(dat["added_cited_age_mean_on_std"])
  }
  
  # dates differences
  dat["added_publication_minus_filling"] <- as.numeric(difftime(dat$PUBLICATION_MONTH, dat$FILING_MONTH, units='days')); range(dat["added_publication_minus_filling"])
  dat["added_filling_minus_begin"] <- as.numeric(difftime(dat$FILING_MONTH, dat$BEGIN_MONTH, units='days')); range(dat["added_filling_minus_begin"])
  dat["added_publication_minus_begin"] <- as.numeric(difftime(dat$PUBLICATION_MONTH, dat$BEGIN_MONTH, units='days')); range(dat["added_publication_minus_begin"])
  
  # the max differences between PRIORITY_MONTH and BEGIN_MONTH is 30 
  # so begin_month could be consider as a proxy of priority_month
  # hence we don't calculate the differences for PRIORITY_MONTH
  max(difftime(dat$PRIORITY_MONTH, dat$BEGIN_MONTH, units='days'), na.rm=TRUE)
} else {
 
  # factor features #
  
  # countries
  dat["added_ctry_vs_first_app_ctry"] <- factor(as.character(dat$simp_imp_COUNTRY) != as.character(dat$simp_imp_FISRT_APP_COUNTRY))
  dat["added_first_inv_ctry_vs_first_app_ctry"] <- factor(as.character(dat$simp_imp_FISRT_INV_COUNTRY) != as.character(dat$simp_imp_FISRT_APP_COUNTRY))
  dat["added_first_inv_ctry_vs_ctry"] <- factor(as.character(dat$simp_imp_FISRT_INV_COUNTRY) != as.character(dat$simp_imp_COUNTRY))
  
  # types
  dat["added_first_app_type_vs_first_inv_type"] <- factor(as.character(dat$simp_imp_FISRT_APP_TYPE) != as.character(dat$simp_imp_FISRT_INV_TYPE))
  
  # sources
  # note : SOURCE_CITED_AGE always equals SOURCE_IDX_ORI so we just need to compare SOURCE_IDX_RAD vs SOURCE_IDX_ORI or SOURCE_CITED_AGE
  length(which(as.character(dat$SOURCE_CITED_AGE) != as.character(dat$SOURCE_IDX_ORI)))
  dat["added_source_idx_rad_vs_source_idx_ori"] <- factor(as.character(dat$SOURCE_IDX_RAD) != as.character(dat$SOURCE_IDX_ORI))
  
  # numerical features #
  
  # applicants
  dat["added_app_nb_pays_on_app_nb"] <- dat$simp_imp_APP_NB_PAYS/dat$simp_imp_APP_NB; max(dat["added_app_nb_pays_on_app_nb"])
  dat["added_app_nb_type_on_app_nb"] <- dat$simp_imp_APP_NB_TYPE/dat$simp_imp_APP_NB; max(dat["added_app_nb_type_on_app_nb"])
  
  # classes
  dat["added_app_nb_root_classes_on_nb_classes"] <- dat$NB_ROOT_CLASSES/dat$NB_CLASSES; max(dat["added_app_nb_root_classes_on_nb_classes"])
  
  # technologies
  dat["added_nb_sectors_on_nb_fields"] <- dat$NB_SECTORS/dat$NB_FIELDS; max(dat["added_nb_sectors_on_nb_fields"])
  
  # investment
  dat["added_inv_nb_pays_on_inv_nb"] <- dat$simp_imp_INV_NB_PAYS/dat$simp_imp_INV_NB; max(dat["added_inv_nb_pays_on_inv_nb"])
  dat["added_inv_nb_type_on_inv_nb"] <- dat$simp_imp_INV_NB_TYPE/dat$simp_imp_INV_NB; max(dat["added_inv_nb_type_on_inv_nb"])
  
  # backward
  # note : NB_BACKWARD is not the sum of all the rest 'backward' features so we create it
  mean(dat$NB_BACKWARD_NPL + dat$NB_BACKWARD_XY + dat$NB_BACKWARD_I + dat$NB_BACKWARD_AUTRE + dat$NB_BACKWARD_PL); mean(dat$NB_BACKWARD)
  dat["added_all_backward"] <- dat$NB_BACKWARD_NPL + dat$NB_BACKWARD_XY + dat$NB_BACKWARD_I + dat$NB_BACKWARD_AUTRE + dat$NB_BACKWARD_PL
  
  # cited age
  dat["added_cited_age_max_minus_min"] <- dat$cited_age_max - dat$cited_age_min; range(dat["added_cited_age_max_minus_min"])
  dat["added_cited_age_mean_on_std"] <-dat$simp_imp_cited_age_std/dat$cited_age_mean; range(dat["added_cited_age_mean_on_std"])
  
  # little workaround : due to imputet value the ratio could be infinite, we put it at the double of the max if so
  if(range(dat["added_cited_age_mean_on_std"])[2] == Inf ) {
    inf <-  which(dat["added_cited_age_mean_on_std"]==Inf)
    max_without_inf <- range(dat[-inf, "added_cited_age_mean_on_std"])[2]
    dat[inf, "added_cited_age_mean_on_std"] <- 2*max_without_inf; range(dat["added_cited_age_mean_on_std"])
  }
  
  # dates differences
  dat["added_publication_minus_filling"] <- as.numeric(difftime(dat$PUBLICATION_MONTH, dat$FILING_MONTH, units='days')); range(dat["added_publication_minus_filling"])
  dat["added_filling_minus_begin"] <- as.numeric(difftime(dat$FILING_MONTH, dat$BEGIN_MONTH, units='days')); range(dat["added_filling_minus_begin"])
  dat["added_publication_minus_begin"] <- as.numeric(difftime(dat$PUBLICATION_MONTH, dat$BEGIN_MONTH, units='days')); range(dat["added_publication_minus_begin"])
  
  # the max differences between PRIORITY_MONTH and BEGIN_MONTH is 30 
  # so begin_month could be consider as a proxy of priority_month
  # hence we don't calculate the differences for PRIORITY_MONTH
  max(difftime(dat$PRIORITY_MONTH, dat$BEGIN_MONTH, units='days'), na.rm=TRUE)
  
}