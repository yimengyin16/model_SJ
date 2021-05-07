## Imputation of decrement tables and salary scales for SJ police and fire


## Inputs
#   - inputs/data_raw/Data_SJPF_decrements_AV2020_raw.RData"


## Outputs
 #  -  tables imputed across age/ea/yos ranges, all in "tidy" format
 

##' Notes
#'  Data processing in this script must be "model agnostics"
#'  Data processing can take into account 
#'    - the structure of demographic inputs (e.g. age, yos ranges)
#'    - relevant CalPERS rules and valuation methods 



## Steps
#' 1. Max retirement age: based on benefit rules and retirement rate assumptions
#'    - 62 for tier 1 and 65 for tier 2 (retirement rate assumptions)
#'    - Use 65 for all
#' 2. Max ea and max age for actives is (max retAge - 1): 64
#' 3. Max yos is (max retAge - min ea): 65 - 20 = 45 
#'    (note that yos at max retAge is needed to calculate benefits)

#'  - Range of age, general:  20-100
#'  - Range of age, actives:  20-64
#'  - Range of ea:  20 - 64 
#'  - Range of age, retirees: 20-100 (to take into account young beneficiaries and disability retirees)
#'  - Range of yos: 0-45 


#' Ranges of ea and yos should ALSO take into account the age-yos ranges of the data for active members
#'  - In active memeber data: max age is 64, max yos is 34
#'  - For the age group 64, the min yos is 15, implying the max ea in the data is 49.


#' If max age in active member data is greater than the max age determined above, 
#' then we should consider adjusting the member data(e.g. merged to lower age groups)

#*****  For MEPERS
#*****  Note ote that there are about 2-3 imputed active members with age > 62. 
#*****  For now, they are removed. 


#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_inputs  <- "inputs/data_proc/"
fn_inputs   <- "Data_SJPF_decrements_AV2020_raw.RData" 
filePath_inputs <- paste0(dir_inputs, fn_inputs)

dir_outputs <- "Inputs/data_proc/"


load(filePath_inputs)



#*******************************************************************************
#                      ## Imputing service retirement rates ####
#*******************************************************************************

# Notes:
#  - t1
#     - extending to age 50-65 by yos 0 to yos 0-35
#  - t2
#     - extending to age 50-65, yos 5-35 (0 if yos < 5)
#     
# group indices: t1.police,  t1.fire, t2.police, t2.fire

df_qxr_t1_imputed <- 
  expand_grid(grp = unique(df_qxr_t1_raw$grp), 
            age = 50:65,
            yos = 0:45) %>% 
  filter(age - yos >= 20) %>% 
  mutate(yos_range = case_when(
                         yos < 30 ~ "yos_l30",
                         yos >= 30 ~ "yos_ge30",
                         TRUE ~ NA_character_)) %>% 
  left_join(
    df_qxr_t1_raw %>% 
      gather(yos_range, qxr, -tier,- grp, -age),
    by = c("grp", "age", "yos_range")
    ) %>% 
  mutate(tier = "t1") %>% 
  unite("grp", tier, grp, sep = ".") %>% 
  select(-yos_range) %>% 
  group_by(grp, yos) %>% 
    mutate(qxr = ifelse(age> 62, qxr[age == 62], qxr)) %>% 
  ungroup



df_qxr_t2_imputed <- 
  expand_grid(grp = unique(df_qxr_t2_raw$grp), 
              age = 50:65,
              yos = 5:45) %>% 
  filter(age - yos >= 20) %>% 
  mutate(yos_range = case_when(
    yos %in% 5:19  ~ "yos_5.19",
    yos %in% 20:24 ~ "yos_20.24",
    yos %in% 25:29 ~ "yos_25.29",
    yos >= 30      ~ "yos_30",
    TRUE ~ NA_character_)) %>% 
  left_join(
    df_qxr_t2_raw %>% 
      gather(yos_range, qxr, -tier,- grp, -age),
    by = c("grp", "age", "yos_range")
  ) %>% 
  mutate(tier = "t2") %>% 
  unite("grp", tier, grp, sep = ".") %>% 
  select(-yos_range) %>% 
  group_by(grp, yos) %>% 
  ungroup


df_qxr_imputed <- 
  bind_rows(df_qxr_t1_imputed, 
            df_qxr_t2_imputed)




#*******************************************************************************
#                      ## Imputing disability retirement rates ####
#*******************************************************************************

# Imputation:
#   - range 20-65


df_qxd_imputed <- 
  df_qxd_raw %>% 
  relocate(grp) %>% 
  as.data.frame() %>% 
  splong(fillvar = "age", fitrange = 20:65, method = "hyman") %>%
  group_by(grp) %>% 
  mutate(qxd = ifelse(age < 25, qxd[age == 25], qxd)) %>% 
  ungroup

# qplot(data = df_qxd_imputed, x = age, y = qxd, color = grp, geom = "line")


#*******************************************************************************
#                      ## Imputing termination rates, refund  ####
#*******************************************************************************
# Imputation: 
#   - imputate yos across 0-45
#   - use yos 19 value for yos > 19


df_qxt_imputed <- 
  df_qxt_raw %>% 
  relocate(grp) %>% 
  as.data.frame() %>% 
  splong(fillvar = "yos", fitrange = 0:45) %>%  
  group_by(grp) %>% 
  mutate(qxt = ifelse(yos >= 20, qxt[yos == 19], qxt)) %>% 
  ungroup





#*******************************************************************************
#                      ## Imputing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-65 by 5, imputation needed
#' Labels in col names:
#'  - female/male

#' Imputation
#'  - impute across age 20-70


# df_qxm.pre_imputed <- 
#   df_qxm_raw %>% 
#   select(age, qxm.pre.male, qxm.pre.female) %>% 
#   gather(Var, value, -age) %>% 
#   relocate(Var, age) %>% 
#   filter(age <=65) %>% 
#   splong("age", fitrange = 20:70) %>% 
#   spread(Var, value)


# df_qxm.pre_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)


#*******************************************************************************
#                      ## Mortality from Pub2010  ####
#*******************************************************************************

#' Healthy non-annuitant:
#'   - PubS 2010A, healthy employees
#' Healthy retirees: 
#'   - PubS 2010A, healthy retirees
#' Disabled retirees: 
#'   - PubS 2010, diabled retirees  

# Truncated at age 100

ls_pub2010_raw <- readRDS(paste0(dir_inputs, "pub2010_raw.rds"))

df_qxm_imputed <- 
left_join(
ls_pub2010_raw$pubS2010A %>% 
  select(age,
         qxm.pre_female   = qxm.employee.female,
         qxm.pre_male     = qxm.employee.male,
         qxm.post_female  = qxm.healthyRet.female,
         qxm.post_male    = qxm.healthyRet.male),

ls_pub2010_raw$pubS2010 %>% 
  select(age,
         qxmd.post_female  = qxm.disbRet.female,
         qxmd.post_male    = qxm.disbRet.male),

by = "age"
) %>% 
filter(age <= 100)


df_qxm_imputed %<>% 
  mutate(
         qxm.pre_female   = 0.979 * qxm.pre_female,
         qxm.pre_male     = 0.979 * qxm.pre_male ,
         qxm.post_female  = 1.002 * qxm.post_female,
         qxm.post_male    = 1.002 * qxm.post_male,
         qxmd.post_female = 0.915 * qxmd.post_female,
         qxmd.post_male   = 0.915 * qxmd.post_male
         
         )
  



# ls_pub2010_raw$pubS2010 %>% names

# df_qxm.post_imputed <- 
#   df_qxm_raw %>% 
#   select(age, qxm.post.male, qxm.post.female) %>% 
#   gather(Var, value, -age) %>% 
#   relocate(Var, age) %>% 
#   filter(age >= 50) %>% 
#   splong("age", fitrange = 40:100) %>% 
#   spread(Var, value)


# df_qxm.post_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)



# #*******************************************************************************
# #                      ## Imputing disability retirement mortality  ####
# #*******************************************************************************
# 
# # Indices:
# #   - age: 25-70, by 5y, imputation needed  
# 
# 
# # Imputation:
# #   - fill age groups 75-100, use 2 x the healthy rates (imputation does not increase the mortality enough for larger ages)
# #   - across yos 20-100 
# 
# 
# df_qxmd_imputed <- 
#   df_qxm_raw %>% 
#   mutate(qxmd.male = ifelse(age >= 75, 2*qxm.post.male, qxmd.male),
#          qxmd.female = ifelse(age >= 75, 2*qxm.post.female, qxmd.female)) %>% 
#   select(age, qxmd.male, qxmd.female) %>% 
#   gather(Var, value, -age) %>% 
#   relocate(Var, age) %>% 
#   filter(age >= 25) %>% 
#   splong("age", fitrange = 20:100) %>% 
#   spread(Var, value)


# df_qxmd_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)
# 


#*******************************************************************************
#              ## Setting morality to 1 at the max age  ####
#*******************************************************************************

df_qxm_imputed %<>% 
  mutate(qxm.post_male   = ifelse(age == max(age), 1, qxm.post_male),
         qxm.post_female = ifelse(age == max(age), 1, qxm.post_female),
         qxmd.post_male   = ifelse(age == max(age), 1, qxmd.post_male),
         qxmd.post_female = ifelse(age == max(age), 1, qxmd.post_female)
         )
  



#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************

# Imputation
#  - yos: to 0-45
#  - use yos 11 value for yos > 11

df_salScale_imputed <- 
  data.frame(yos = 0:45) %>% 
  left_join(df_salScale_raw, by = "yos") %>% 
  mutate(salScale_merit = ifelse(yos >11, salScale_merit[yos == 11], salScale_merit))






#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************

df_qxr_imputed
df_qxd_imputed
df_qxt_imputed
df_qxm_imputed
df_salScale_imputed



save(
  df_qxr_imputed,
  df_qxd_imputed,
  df_qxt_imputed,
  df_qxm_imputed,
  df_salScale_imputed,
  
	file = paste0(dir_outputs, "Data_SJPF_decrements_AV2020_imputed.RData")
)




