# Constructing tier SJFC tier 1



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_SFFC_decrements_AV2020_imputed.RData
#'   - inputs/data_proc/Data_SFFC_demographics_2020630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "SC"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   
#'     - tier specific parameters


#*******************************************************************************
#                               Tier specification  ####
#*******************************************************************************
# Source: AV2019, ep59


##' Members included 
#'  - SJFC tier 1 members
#'     - Police: hired before Sep 30, 2012
#'     
#'  How to allocate total active and retiree numbers to tier 1 and tier 2


###' Service retirement 
#'  
#'  - Benefit rules
#'      - Use benefit rules of tier 1
#'      
#'    
#   - Eligibility for unreduced benefits
#'       - age 55 & yos 5
#'       - yos 30 
#'
#'  - Eligibility for reduced benefits
#'       - NA
#'      
#'  - Vesting: 
#'       - 5
#'  
#'  
#'  - Final compensation (FAS) 
#'      - The plan policy
#'        - 12 highest consecutive months, with anti-spike measures. 
#'      - Model:
#'        - 1 year
#'  
#'  
#'  - Benefit formula
#'       - 2.5%
#'       - Max of 75% of FAS 




###' Deferred retirement  
#' - Plan  policy:
#'   - YOS < 5: Lump sum EEC with interests (int rate? 2% as in P&F?)
#'   - YOS >= 5, service retirement benefit, payable at age 55
#'   
#' 
#  - Model: 
#'   - Simplification: reduced defrRet benefit for YOS < 5
#'   - for now reduce benefit by 50% if yos < 5



## Disability: 45% assumed to be duty related, 55% non-duty related
##  - weighted average of the two types

###' Disability retirement, service connected
#' 
#'  - Plan policy:
#'     - no age/yos requirement
#'     - 2.5% per yos. min 40%, max 75% FAS
#  
#   - Model:
#'     - 


###' Disability retirement, non-service connected
#'   - YOS>=5
#'   - hired before 1998: Service-connected benefit reduced by 0.5% for each year that the disability age preceded 55.
#'   - hired after 1998:  20% of FAS, plus 2% for yos between 6 and 16; 2.5% for yos greater than 16; max 75% FAS
#'   
#'   - Model: 


# Death benefit: service connected
#'   - YOS < 5: Lump-sum: EECs and interest, plus 1 month of salary for each yos, up to 6 years 
#'   - yos >=5: 2.5% per yos, min 40% and max 75% of FAS
#'   
#'   - model: to simplify as lump-sum


# COLA:
#   - Policy: 3% 



###' Member contribution
#      - 3/11th of NC





## Assumptions, needs to be revisited 


# gender ratio:
# - No gender ratio provided in AV and CAFR, 
#   - Assumption: 55% female and 45% male
share_male <- 0.45
share_female <- 1 - share_male


## Assumptions on demographics 

#'  - SJFC tier 1 members
#'     - Police: hired before Sep 30, 2012
#'     
#'  How to allocate total active and retiree numbers to tier 1 and tier 2


# t1 and t2 mebmers:
#  Active members:
#   - According to AV2020 ep 46, there are 2215 tier 2 members
#   - For now, use yos <= 7 as tier 2 members (in theory, only a portion of memebers with yos == 7)
#   - in the imputed data, there are 2169 members (2.1% lower)
#   
#
#
#  Serivice retirees for regular members
#    - number should be very small for tier 2 (2 retired in 2020)
#
#
#  Initial terminated members
#   - For now, we assume that for each tier the liability of initial terminated members(in or not in pay status) 
#     is a fixed percentage of the AL of retirees. 
#   - there are 230 vested terms in tier 2, should think about how to model them 





#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data    <- "inputs/data_proc/"
dir_outputs <- "model/tiers/tierData/"



# Model settings
range_age <- 20:100
range_ea  <- 20:64  # max retirement age is assumed to be 65 (qxr = 1 at age 65 in AV tables) 



# Tier specific parameters
tier_name <- "fc.t1"
age_vben  <- 55 # 50 if yos >= 25, 55 if yos < 25. assume 55 for all in the model
v.year    <- 0
fasyears  <- 1  
# bfactor   <- 0.02
cola_assumed <- 0.03 # assumed cola rates for valuation  
# EEC_rate <- 0.0735 # use EEC and ERC caps 


#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_SJFC_decrements_AV2020_imputed.RData"))
load(paste0(dir_data, "Data_SJFC_demographics_20200630_fillin.RData"))
df_mp2020_raw <- readRDS(paste0(dir_data, "MP2020_raw.rds"))

# Data loaded:

## Decrements:
# df_qxr_regular_imputed
# df_qxr_special_imputed
# df_qxd_imputed
# df_qxt_imputed
# df_qxm.pre_imputed
# df_qxm.post_imputed
# df_qxmd_imputed
# df_salScale_imputed

## Member data
# df_nactives_fillin
# df_n_servRet_fillin
# df_n_disbRet_occ_fillin
# df_n_disbRet_nonocc_fillin
# df_n_beneficiaries_fillin

# df_nactives_fillin %>% filter(yos<= 7) %>% pull(nactives) %>% sum

#*******************************************************************************
#                      ## Decrements 1: combining groups ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "t1")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "t1","wgt"] <-  1


## calculate weighted average
df_qxr_tier <- 
  df_qxr_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age, yos) %>% 
  summarise(qxr = weighted.mean(qxr, wgt), .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Disability retirement rates

# groups included
grp_include <- "t1" # df_qxd_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "t1")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "t1","wgt"] <-  1



## calculate weighted average
# Need to combine two types of disability rates: adding the two rates
df_qxd_tier <- 
  df_qxd_imputed %>% 
  mutate(grp = "t1") %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxd = weighted.mean(qxd, wgt), .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Termination with refund

# groups included
grp_include <- "t1" #df_qxt_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "t1")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "t1","wgt"] <-  1



## calculate weighted average
df_qxt_tier <- 
  df_qxt_imputed %>% 
  mutate(grp = "t1") %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos) %>% 
  summarise(qxt = weighted.mean(qxt, wgt),
            .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(yos) %>% 
  ungroup()


##  Mortality

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

df_qxm_tier <- 
  ls_pub2010_raw$pubG2010 %>% 
  select(age,
         qxm.pre_female   = qxm.employee.female,
         qxm.pre_male     = qxm.employee.male, 
         qxm.post_female  = qxm.healthyRet.female,
         qxm.post_male    = qxm.healthyRet.male,
         qxmd.post_female  = qxm.disbRet.female,
         qxmd.post_male    = qxm.disbRet.male) %>% 
  filter(age <= 100) 


df_qxm_tier %<>% 
  mutate(
    qxm.pre_female   = 0.960 * qxm.pre_female,
    qxm.pre_male     = 0.995 * qxm.pre_male ,
    qxm.post_female  = 1.084 * qxm.post_female,
    qxm.post_male    = 0.992 * qxm.post_male,
    qxmd.post_female = 0.991 * qxmd.post_female,
    qxmd.post_male   = 1.051 * qxmd.post_male
    
  )


df_qxm_tier <- 
  df_qxm_imputed %>% 
  mutate(qxm.pre   = share_female * qxm.pre_female   + share_male * qxm.pre_male,
         qxm.post  = share_female * qxm.post_female  + share_male * qxm.post_male,
         qxmd.post = share_female * qxmd.post_female + share_male * qxmd.post_male,
         grp = tier_name
         ) %>% 
  select(grp, age, 
         qxm.pre, qxm.pre_female, qxm.pre_male,
         qxm.post, qxm.post_female, qxm.post_male,
         qxmd.post, qxmd.post_female, qxmd.post_male)
  
# df_qxr_tier
# df_qxd_tier
# df_qxt_tier
# df_qxm_tier



#*******************************************************************************
#        ## Decrements 2: Single decrement table ####
#*******************************************************************************
#*
decrements_tier <- 
  expand.grid(age = range_age, 
              ea  = range_ea) %>% 
  mutate(yos = age - ea,
         grp = tier_name) %>% 
  filter(age >= ea) %>% 
  left_join(df_qxm_tier,        by = c("grp", "age")) %>%         # mortality 
  left_join(df_qxt_tier,        by = c("grp", "yos")) %>%         # termination 
  left_join(df_qxr_tier,        by = c("grp", "age", "yos")) %>%  # service retirement
  left_join(df_qxd_tier,        by = c("grp", "age")) %>%         # disability

  select(grp, ea, age, yos, 
         qxm.pre,   
         qxm.pre_female,   qxm.pre_male,
         qxm.post,  qxm.post_female,  qxm.post_male,
         qxmd.post, qxmd.post_female, qxmd.post_male,
         qxt, 
         qxr, 
         qxd, 
         everything()
         #-qxm.pre
         )%>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.)






#*******************************************************************************
#        ## Decrements 3: adding eligibility information ####
#*******************************************************************************

# Create 2 columns for each tier
 # elig_servRet_full:  number of year of being eligible for full or greater retirement benefits
 # elig_servRet_early: number of year of being eligible for early retirement benefits; 
 #             0 after being eligible for full retirement benefits

#   - Eligibility for unreduced benefits
#'       - age 55 & yos 5
#'       - yos 30 


decrements_tier  %<>% 
  group_by(ea) %>% 
  mutate(
    # Eligibility for full (or greater) retirement benefit
    elig_servRet_full = ifelse(  (age >= 55 & yos >= 5)|
                                 (yos >= 30),  
                                 1, 0) %>% cumsum,
    
    # Eligibility for early retirement benefit
    elig_servRet_early = elig_servRet_full,
    elig_servRet_early = ifelse( elig_servRet_full, 0, elig_servRet_early),
    
    # number of years before full retirement
    year_b4full = order_by(-age, cumsum(!as.logical(elig_servRet_full))),
    year_b4full = ifelse(as.logical(elig_servRet_early), year_b4full, 0)
    ) %>% 

  ## Adjustments to decrement rates based on eligibility
  #   1. Only keep retirement rates when a member is eligible
  #   2. Coerce termination rates to 0 when eligible for early retirement or full retirement, or age >= age_vben 
  
  mutate(
    qxr        = ifelse(elig_servRet_early | elig_servRet_full, qxr, 0),
    qxt        = ifelse((elig_servRet_early == 0 & elig_servRet_full == 0) & age < age_vben, qxt, 0)
  ) %>% 
  ungroup

# decrements_tier %>%
#   filter(ea == 30)


#*******************************************************************************
#   ## Decrements 3.1: explicitly define mortality for terminated members   ####
#*******************************************************************************

decrements_tier %<>% 
  mutate(
         qxm.defrRet        = ifelse(age >= age_vben, qxm.post, qxm.pre),
         qxm.defrRet_male   = ifelse(age >= age_vben, qxm.post_male,   qxm.pre_male),
         qxm.defrRet_female = ifelse(age >= age_vben, qxm.post_female, qxm.pre_female))





#*******************************************************************************
#                      ## Decrements 4: Improvement table  ####
#*******************************************************************************

# Target format:
#  data frame indexed by year and age. 
#  each row is the improvement factor to be applied to the value in that year-age cell  





# extending to 1900 to 2220
#  -  assume 0 for year < 1951
#  -  assume 2036 value for year > 2036

df_mp2020 <- 
  bind_rows(

  df_mp2020_raw$male %>% 
  gather(year, fct, -age, -gender) %>% 
  mutate(year = as.numeric(year),
         fct = as.numeric(fct)),

  df_mp2020_raw$female %>% 
    gather(year, fct, -age, -gender) %>% 
    mutate(year = as.numeric(year),
           fct = as.numeric(fct))
)


decrements_improvement <- 
  expand_grid(gender = c("male", "female"), 
              age    = range_age,
              year   = 1900:2220) %>% 
  left_join(df_mp2020,
            by = c("gender", "age", "year"))  
  

decrements_improvement %<>% 
  group_by(gender, age) %>% 
  mutate(fct = ifelse(year < 1951, 0, fct),
         fct = ifelse(year > 2036, fct[year ==  2036], fct),
         ) %>% 
  mutate(impr = ifelse(year > 2010, lag(cumprod(1 - ifelse(year>=2010,fct,0))), 1),
         impr = ifelse(year < 2010, lead(order_by(-year,  cumprod(1/(1 - ifelse(year<=2010,fct,0))))), impr)
  )


decrements_improvement %<>% 
  select(-fct) %>% 
  spread(gender, impr) %>% 
  rename(impr_male = male,
         impr_female = female)




# decrements_improvement <- 
#   expand_grid(year =  2017:(2017+14),
#               age  =  range_age) %>% 
#   left_join(
#     bind_rows(
#       # df_qxm.post_imputed %>% mutate(year = 2017),
#       # df_qxm.post_proj_imputed %>% 
#       #   rename_with( ~str_remove(.x, "_proj" )) %>% 
#       #   mutate(year = 2017+14)
#       
#       df_qxm.post_tier %>% mutate(year = 2017),
#       df_qxm.post_proj_tier %>%
#         rename_with( ~str_remove(.x, "_proj" )) %>%
#         mutate(year = 2017+14)
#       ),
#     by = c("year", "age")
#     )
# 
# decrements_improvement %<>% 
#   group_by(age) %>% 
#   arrange(age, year) %>% 
#   # filter(age == 90) %>% 
#   mutate(across(!c(year, grp), ~ seq(first(.x), last(.x), length.out = n()))) %>% 
#   mutate(across(!c(year, grp), ~ .x / .x[year == min(year)])) %>% 
#   rename_with(~ paste0("impr_", .x), !c(year, age, grp)) %>% 
#   mutate(grp = tier_name )

#*******************************************************************************
#                      ## Salary Scale  ####
#*******************************************************************************

# df_salScale_imputed
# wage inflation: 3%

df_salScale_tier <- 
  df_salScale_imputed %>% 
  mutate(grp = tier_name,
         salScale = salScale_merit + 0.03) %>%
  select(grp, yos, salScale) %>%
  arrange(yos)
  

# # groups included
# grp_include <- df_salScale_imputed$grp %>% unique
# grp_include <- grp_include[str_detect(grp_include, "misc|inds")]
# 
# # weight for each group
# wgts <- tibble(grp = grp_include, wgt = 0)
# 
# wgts[wgts$grp == "misc", "wgt"] <-  0.679
# wgts[wgts$grp == "inds", "wgt"] <-  0.046
# wgts
# 
# ## calculate weighted average
# df_salScale_tier <- 
#   df_salScale.merit_imputed %>% 
#   filter(grp %in% grp_include) %>% 
#   left_join(wgts, by = "grp") %>% 
#   group_by(yos, ea) %>% 
#   summarise(salScale.merit = weighted.mean(salScale.merit, wgt),
#             .groups = "rowwise") %>% 
#   mutate(grp = tier_name,
#          salScale.infl = 0.0275,
#          salScale = salScale.merit + salScale.infl) %>% 
#   relocate(grp) %>% 
#   arrange(ea, yos) %>% 
#   ungroup()




#*******************************************************************************
#                      ## Initial demographics  ####
#*******************************************************************************

##  View the inputs
# df_nactives_fillin
# df_n_servRet_fillin
# df_n_beneficiaries_fillin


## groups included 
grp_include <- df_nactives_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "all")]



## Active members

# all active members 
df_n_actives_tier <- 
  df_nactives_fillin %>% 
  filter(grp %in% grp_include) %>% 
  group_by(yos, ea) %>% 
  summarise(salary   = weighted.mean(salary, nactives, na.rm = TRUE) %>% na2zero(),
            nactives = sum(nactives, na.rm= TRUE) %>% na2zero,
            .groups = "drop") %>% 
  mutate(grp = tier_name,
         age = ea + yos) %>% 
  relocate(grp) %>% 
  arrange(ea, age) %>% 
  ungroup() %>% 
  
  # tier 1
  filter(ea >=20,
         age <= 69,
         yos <= 49) # 21 members are removed


# df_n_actives_tier %>% pull(nactives) %>% sum



# Keep tier 1 members only
#  assume 
#    - members with yos > 7 are tier 1 members
 

df_n_actives_tier %<>%
  mutate(nactives = case_when(
    yos <= 7 ~ 0,
    TRUE ~ nactives
  ))

# df_n_actives_tier$nactives %>% sum



## Retirees (all types included)

# all service retirees are assumed to be tier 1 members

df_n_servRet_tier <- 
  df_n_servRet_fillin %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_servRet = weighted.mean(benefit_servRet, n_servRet, na.rm= TRUE),
            n_servRet       = sum(n_servRet, na.rm = TRUE),
            
            .groups = "drop") %>% 
  colwise(na2zero)(.) %>% 
  mutate(grp = tier_name) %>% 
  select(grp, age, n_servRet, benefit_servRet) %>% 
  arrange(age) %>% 
  ungroup()

# (df_n_servRet_tier$n_servRet*df_n_servRet_tier$benefit_servRet) %>% sum
# model/target:169508194/169508194  = 100%




## View the results
# df_n_actives_tier
# df_n_servRet_tier


 
#*******************************************************************************
#                    ## Saving tier information in a list  ####
#*******************************************************************************

# collect tier-specific parameters in a list
tier_params <- 
  list(
    tier_name = tier_name,
    age_vben  = age_vben,
    v.year    = v.year,
    fasyears  = fasyears,  
    cola_assumed = cola_assumed,
    share_male   = share_male,
    share_female = share_female
    #bfactor = bfactor
    #EEC_rate = EEC_rate
  )


# Store all tier data in a list
assign(paste0("tierData_", tier_name), 
       
         list(
           tier_name = tier_name,
           
           decrements = decrements_tier,
           decrements_improvement = decrements_improvement,
           
           df_n_actives = df_n_actives_tier,
           df_n_servRet = df_n_servRet_tier,
           
           df_salScale  = df_salScale_tier,
           
           tier_params = tier_params
         )
       )

# Save the list of tier data in a .rds (single object) file
saveRDS(get(paste0("tierData_", tier_name)), 
        file = paste0(dir_outputs, "tierData_", tier_name, ".rds"))


# tierData <- readRDS(paste0(dir_outputs, "tierData_", tier_name, ".rds"))


