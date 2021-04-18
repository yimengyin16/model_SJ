## Imputation of decrement tables and salary scales for MEPERS


## Inputs
#   - inputs/data_raw/Data_MEPERS_decrements_AV2020_raw.RData"


## Outputs
 #  -  tables imputed across age/ea/yos ranges, all in "tidy" format
 

##' Notes
#'  Data processing in this script must be "model agnostics"
#'  Data processing can take into account 
#'    - the structure of demographic inputs (e.g. age, yos ranges)
#'    - relevant CalPERS rules and valuation methods 


## Data frames to be processed
#   df_qxr_regular_raw
#   df_qxr_special_raw
#   df_qxd_raw 
#   df_qxt_raw
#   df_qxm_raw
#   df_salScale_raw



## Imputation rules

#' Ranges of ea and yos should take into account the age-yos ranges of the data for active members of MEPSERS
#'  - In active memeber data: max age is 74, max yos is 44 
#'  - For the age group 70-74, the min yos is 0, implying the max ea in the data is 74. 

# Steps
#' 1. Max retirement age: based on benefit rules and retirement rate assumptions
#'    - should be 70 (retirement rate assumptions), note that there are active members with age > 70. 
#' 2. Max ea and max age for actives is (max retAge - 1)
#' 3. Max yos is (max age for actives - min ea)
#' 
#' If max age in active member data is greater than the max age determined above, 
#' then we should consider adjusting the member data(e.g. merged to lower age groups)

#'  - Range of age, general:  20-100
#'  - Range of age, actives:  20-69
#'  - Range of age, retirees: 20-100 (to take into account young beneficiaries and disability retirees)
#'  - Range of ea:  20 - 69 
#'  - Range of yos: 0-49 


#*****  For MEPERS
#*****  Note ote that there are active members with age > 69, which accounts for about 6% of the members.
#*****  For March 15 version, ignore then or merge then into the cells of age 69 with the same yos. 


#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_inputs  <- "inputs/data_proc/"
fn_inputs   <- "Data_MEPERS_decrements_AV2020_raw.RData" 
filePath_inputs <- paste0(dir_inputs, fn_inputs)

dir_outputs <- "Inputs/data_proc/"


load(filePath_inputs)



#*******************************************************************************
#                      ## Imputing service retirement rates ####
#*******************************************************************************

# Indices:
#   - regular: 
#       - age: 45-70, by 1y, NO imputation needed
#   - special: 
#       - yos: 20-40, by 1 year,
#   


# Imputation:
#   - regular: across age 45-70
#   - special: no imputation needed
 

df_qxr_regular_imputed <- 
  df_qxr_regular_raw %>% 
  splong("age", fitrange = 45:70)


df_qxr_special_imputed <- df_qxr_special_raw


#*******************************************************************************
#                      ## Imputing disability retirement rates ####
#*******************************************************************************

# Indices:
#   - age: 25-60, by 5y, imputation needed  

# Imputation:
#   - range 20-80
#   - across age 20-69 with imputation
#   - use age 69 value for age > 69

df_qxd_imputed <- 
  df_qxd_raw %>% 
  mutate(grp = "temp") %>% 
  relocate(grp) %>% 
  splong(fillvar = "age", fitrange = 20:80) %>% 
  mutate(qxd = ifelse(age >= 69, qxd[age == 69], qxd),
         grp = NULL)





#*******************************************************************************
#                      ## Imputing termination rates, refund  ####
#*******************************************************************************

## indices
#  - yos: 0~20 by y5,  imputation needed

# Imputation: 
#   - imputate yos across 0-50
#   - use yos 20 value for yos > 20


df_qxt_imputed <- 
  df_qxt_raw %>% 
  relocate(grp) %>% 
  splong(fillvar = "yos", fitrange = 0:50) %>%  
  mutate(qxt = ifelse(grp == "regular" & yos >= 20, qxt[yos == 20 & grp == "regular"], qxt),
         qxt = ifelse(grp == "special" & yos >= 10, qxt[yos == 20 & grp == "special"], qxt))





#*******************************************************************************
#                      ## Imputing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-65 by 5, imputation needed
#' Labels in col names:
#'  - female/male

#' Imputation
#'  - impute across age 20-70


df_qxm.pre_imputed <- 
  df_qxm_raw %>% 
  select(age, qxm.pre.male, qxm.pre.female) %>% 
  gather(Var, value, -age) %>% 
  relocate(Var, age) %>% 
  filter(age <=65) %>% 
  splong("age", fitrange = 20:70) %>% 
  spread(Var, value)


# df_qxm.pre_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)


#*******************************************************************************
#                      ## Imputing post-retirement mortality  ####
#*******************************************************************************

# Indices:
#   - age: 50-195, by 5y, imputation needed  


# Imputation:
#   - across yos 40-100 


df_qxm.post_imputed <- 
  df_qxm_raw %>% 
  select(age, qxm.post.male, qxm.post.female) %>% 
  gather(Var, value, -age) %>% 
  relocate(Var, age) %>% 
  filter(age >= 50) %>% 
  splong("age", fitrange = 40:100) %>% 
  spread(Var, value)


# df_qxm.post_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)



#*******************************************************************************
#                      ## Imputing disability retirement mortality  ####
#*******************************************************************************

# Indices:
#   - age: 25-70, by 5y, imputation needed  


# Imputation:
#   - fill age groups 75-100, use 2 x the healthy rates (imputation does not increase the mortality enough for larger ages)
#   - across yos 20-100 


df_qxmd_imputed <- 
  df_qxm_raw %>% 
  mutate(qxmd.male = ifelse(age >= 75, 2*qxm.post.male, qxmd.male),
         qxmd.female = ifelse(age >= 75, 2*qxm.post.female, qxmd.female)) %>% 
  select(age, qxmd.male, qxmd.female) %>% 
  gather(Var, value, -age) %>% 
  relocate(Var, age) %>% 
  filter(age >= 25) %>% 
  splong("age", fitrange = 20:100) %>% 
  spread(Var, value)


# df_qxmd_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)
# 


#*******************************************************************************
#              ## Setting morality to 1 at the max age  ####
#*******************************************************************************

#df_qxm.pre_imputed

df_qxm.post_imputed %<>% 
  mutate(qxm.post.male   = ifelse(age == max(age), 1, qxm.post.male),
         qxm.post.female = ifelse(age == max(age), 1, qxm.post.female))
  

df_qxmd_imputed %<>% 
  mutate(qxmd.male   = ifelse(age == max(age), 1, qxmd.male),
         qxmd.female = ifelse(age == max(age), 1, qxmd.female))



#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************

# Row indices
#  - yos: 0~30 by y1, just expand to 49

df_salScale_imputed <- 
  data.frame(yos = 0:50) %>% 
  left_join(df_salScale_raw, by = "yos") %>% 
  mutate(salScale_tot = ifelse(yos >30, salScale_tot[yos == 30], salScale_tot))






#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************

# df_qxr_regular_imputed
# df_qxr_special_imputed
# df_qxd_imputed
# df_qxt_imputed
# df_qxm.pre_imputed
# df_qxm.post_imputed
# df_qxmd_imputed
# df_salScale_imputed



save(
  df_qxr_regular_imputed,
  df_qxr_special_imputed,
  df_qxd_imputed,
  df_qxt_imputed,
  df_qxm.pre_imputed,
  df_qxm.post_imputed,
  df_qxmd_imputed,
  df_salScale_imputed,

	file = paste0(dir_outputs, "Data_MEPERS_decrements_AV2020_imputed.RData")
)




