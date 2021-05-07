## Imputation of decrement tables and salary scales for SJ Federated


## Inputs
#   - inputs/data_raw/Data_SJFC_decrements_AV2020_raw.RData"


## Outputs
 #  -  tables imputed across age/ea/yos ranges, all in "tidy" format
 

##' Notes
#'  Data processing in this script must be "model agnostics"
#'  Data processing can take into account 
#'    - the structure of demographic inputs (e.g. age, yos ranges)
#'    - relevant CalPERS rules and valuation methods 



## Steps
#' 1. Max retirement age: based on benefit rules and retirement rate assumptions
#'    - 70, max ret age in decrement table
#' 2. Max ea and max age for actives is (max retAge - 1): 69
#' 3. Max yos is (max retAge - min ea): 70 - 20 = 50 
#'    (note that yos at max retAge is needed to calculate benefits)

#'  - Range of age, general:  20-100
#'  - Range of age, actives:  20-69
#'  - Range of ea:  20 - 69 
#'  - Range of age, retirees: 20-100 (to take into account young beneficiaries and disability retirees)
#'  - Range of yos: 0-450


#' Ranges of ea and yos should ALSO take into account the age-yos ranges of the data for active members
#'  - In active memeber data: max age is 74, max yos is 44
#'  - For the age group 74, the min yos is 1, implying the max ea in the data is 73.


#' If max age in active member data is greater than the max age determined above, 
#' then we should consider adjusting the member data(e.g. merged to lower age groups)

#*****  For SJ federated
#*****   -  may lose ~ 10 members
#*****   


age_range <- 20:100
age_range_actives <- 20:69
age_range_retirees <- 20:100
ea_range <- 20:69
yos_range <- 0:50

#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_inputs  <- "inputs/data_proc/"
fn_inputs   <- "Data_SJFC_decrements_AV2020_raw.RData" 
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


df_qxr_t1_imputed <- 
  expand_grid(grp = unique(df_qxr_t1_raw$grp), 
            age = 50:70,
            yos = yos_range) %>% 
  filter(age - yos >= 20) %>% 
  mutate(yos_range = case_when(
                         yos < 15 ~ "yos_l15",
                         yos %in% 15:29 ~ "yos_15to29",
                         yos >= 30 ~ "yos_ge30",
                         TRUE ~ NA_character_)) %>% 
  left_join(
    df_qxr_t1_raw %>% 
      gather(yos_range, qxr, -tier,- grp, -age),
    by = c("grp", "age", "yos_range")
    ) %>% 
  #mutate(tier = "t1") %>% 
  #unite("grp", tier, grp, sep = ".") %>% 
  select(-yos_range) %>%
  relocate(tier, grp) %>% 
  ungroup



df_qxr_t2_imputed <- 
  expand_grid(grp = unique(df_qxr_t2_raw$grp), 
              age = 55:70,
              yos = yos_range) %>% 
  filter(age - yos >= 20,
         yos >= 5) %>% 
  mutate(yos_range = case_when(
    yos %in% 5:10  ~ "yos_5.10",
    yos %in% 11:20 ~ "yos_11.20",
    yos %in% 21:25 ~ "yos_21.25",
    yos %in% 26:34 ~ "yos_26.34",
    yos >= 35      ~ "yos_35",
    TRUE ~ NA_character_)) %>% 
  left_join(
    df_qxr_t2_raw %>% 
      gather(yos_range, qxr, -tier,- grp, -age),
    by = c("grp", "age", "yos_range")
  ) %>% 
  #mutate(tier = "t2") %>% 
  #unite("grp", tier, grp, sep = ".") %>% 
  select(-yos_range) %>% 
  group_by(grp, yos) %>% 
  relocate(tier, grp) %>% 
  ungroup


df_qxr_imputed <- 
  bind_rows(df_qxr_t1_imputed, 
            df_qxr_t2_imputed)




#*******************************************************************************
#                      ## Imputing disability retirement rates ####
#*******************************************************************************

# Imputation:
#   - range 20-70


df_qxd_imputed <- 
  df_qxd_raw %>% 
  #relocate(grp) %>% 
  as.data.frame() %>% 
  splong(fillvar = "age", fitrange = 20:70) %>%
  
  #group_by(grp) %>% 
  mutate(qxd = ifelse(age < 25, qxd[age == 25], qxd)) %>% 
  ungroup

# qplot(data = df_qxd_imputed, x = age, y = qxd, geom = "line")


#*******************************************************************************
#                      ## Imputing termination rates, refund  ####
#*******************************************************************************
# Imputation: 
#   - use yos 15 value for yos > 15


df_qxt_imputed <- 
  df_qxt_raw %>% 
  #relocate(grp) %>% 
  as.data.frame() %>% 
  splong(fillvar = "yos", fitrange = 0:50) %>%  
  #group_by(grp) %>% 
  mutate(qxt = ifelse(yos >= 15, qxt[yos == 15], qxt)) %>% 
  ungroup




#*******************************************************************************
#                      ## Mortality from Pub2010  ####
#*******************************************************************************

#' Healthy non-annuitant:
#'   - PubG 2010, healthy employees
#' Healthy retirees: 
#'   - PubG 2010, healthy retirees
#' Disabled retirees: 
#'   - CalPERS 2009 ordinary disability mortality
#'   - use pubG 2010, disabled retiree as placeholder 

# Truncated at age 100

ls_pub2010_raw <- readRDS(paste0(dir_inputs, "pub2010_raw.rds"))

#ls_pub2010_raw$pubG2010 %>% names

df_qxm_imputed <- 
ls_pub2010_raw$pubG2010 %>% 
  select(age,
         qxm.pre_female   = qxm.employee.female,
         qxm.pre_male     = qxm.employee.male, 
         qxm.post_female  = qxm.healthyRet.female,
         qxm.post_male    = qxm.healthyRet.male,
         qxmd.post_female  = qxm.disbRet.female,
         qxmd.post_male    = qxm.disbRet.male) %>% 
filter(age <= 100) 


df_qxm_imputed %<>% 
  mutate(
         qxm.pre_female   = 0.960 * qxm.pre_female,
         qxm.pre_male     = 0.995 * qxm.pre_male ,
         qxm.post_female  = 1.084 * qxm.post_female,
         qxm.post_male    = 0.992 * qxm.post_male,
         qxmd.post_female = 0.991 * qxmd.post_female,
         qxmd.post_male   = 1.051 * qxmd.post_male
         
         )
  


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
#  - use yos 15 value for yos > 15

df_salScale_imputed <- 
  data.frame(yos = 0:50) %>% 
  left_join(df_salScale_raw, by = "yos") %>% 
  mutate(salScale_merit = ifelse(yos >15, salScale_merit[yos == 15], salScale_merit))






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
  
	file = paste0(dir_outputs, "Data_SJFC_decrements_AV2020_imputed.RData")
)




