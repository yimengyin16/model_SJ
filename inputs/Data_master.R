## Loading data and pre-modeling processing



#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
# rm(list = ls())
source("libraries.R")


# Loading data
source("inputs/Data_readDecrements_SJFC_AV2020.R")
source("inputs/Data_readDemographics_SJFC_20200630.R")
source("inputs/Data_readPlanInfo_SJFC_AV2020.R")


source("inputs/Data_readDecrements_SJPF_AV2020.R")
source("inputs/Data_readDemographics_SJPF_20200630.R")
source("inputs/Data_readPlanInfo_SJPF_AV2020.R")


source("inputs/Data_readDecrements_SJPF_AV2019.R")
source("inputs/Data_readDemographics_SJPF_20190630.R")
source("inputs/Data_readPlanInfo_SJPF_AV2019.R")



# Imputation
source("inputs/Data_imputation_decrements_SJFC_AV2020.R")
source("inputs/Data_imputation_demographics_SJFC_20200630.R")


source("inputs/Data_imputation_decrements_SJPF_AV2020.R")
source("inputs/Data_imputation_demographics_SJPF_20200630.R")


source("inputs/Data_imputation_decrements_SJPF_AV2019.R")
source("inputs/Data_imputation_demographics_SJPF_20190630.R")

