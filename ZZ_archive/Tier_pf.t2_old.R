# Constructing tier SJPF tier 2



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_SFPF_decrements_AV2019_imputed.RData
#'   - inputs/data_proc/Data_SFPF_demographics_2019630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "SC"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   
#'     - tier specific parameters


#*******************************************************************************
#*******************************************************************************
#                               Tier specification  ####
# Source: AV2019, ep63


##' Members included 
#'  - SJPF tier 1 members
#'     - Police: hired on or after Aug 4, 2013
#'     - Fire  : hired on or after Jan 2, 2015
#'     


###' Service retirement 
#'  
#'  - Benefit rules
#'      - Use benefit rules of tier 2
#'      
#'    
#   - Eligibility for unreduced benefits
#'       - age 57 & yos 5
#'
#'  - Eligibility for reduced benefits
#'      - age 50 & yos 5
#'      - reduction: 7% per year before age 57
#'      
#'  - Vesting: 
#'       - ?
#'  
#'  
#'  - Final compensation (FAS) 
#'      - The plan policy
#'        - 36 highest consecutive months, with anti-spike measures. 
#'      - Model:
#'        - 3 year
#'  
#'  
#'  - Benefit formula
#'      - Police and fire: 
#'          - yos in 1-20: 2.4% per year
#'          - yos in 21-25: 3% per year
#'          - yos of 26 and up: 3.4% per year 
#'          - Max of 80% of FAS 
#'      - survivor: 50% joint and survivor annuity    




###' Deferred retirement  
#' - Plan  policy:
#'   - YOS <  5: accumulated EEC with interest 
#'   - YOS >= 5: servRet benefit, actuarially reduced for early retirment, 
#'               payable when eligibility is reached 
#' 
#  - Model: 
#'   - Simplification: 



###' Disability retirement, service connected
#' 
#'  - Plan policy:
#'     - no age/yos requirement
#'     - Greater of:
#'        -  50% of FAS
#'        -  service retirement, if eligible for service retirement
#'        -  actuarial reduced retirement benefit from age 50, if not eligible for servRet
#'        
#   - Model:
#'     - combine Policy and fire


###' Disability retirement, non-service connected
#'   - not modeled b/c all disabilities are assumed to be duty related in the AV 



# Death benefit: 
#'   - YOS>= 2 and before servRet eligibility
#'        - 24% of FAS + 0.75% for each yos in excess of 2, up to 37.5% of FAS
#'   - after servRet eligibility
#'        - servRet
#'   - death in the line of duty
#'        - greater of 
#'          - 37.5% of FAS
#'          - 50% of servRet



# COLA:
#   - Policy: CPI-U for SJ, subject to a cap of 2%.  
#   - model: 2%


###' Member contribution
#    - 50% of total Tier 2 contributions (NC + SC + admin)
#    - Increases in UAAL contribution are limited to 1/3 % of compensation each year
#    - contribution >= 50% of NC



#' Shares of police and fire members
#' Source: AV2019 ep43
#'  - police: 541 / 1215
#'  - fire: 674/1215

share_fire   <- 541/1215 # 44.5%
share_police <- 1 - share_fire  

# gender ratio:
# - No gender ratio provided in AV and CAFR, 
#   - Assumption: 10% female and 90% male
share_male <- 0.9
share_female <- 1-share_male


## Assumptions on demographics 

#'  - SJPF tier 1 members
#'     - Police: hired before Aug 4, 2013
#'     - Fire  : hired before Jan 2, 2015
#'     
#'  How to allocate total active and retiree numbers to tier 1 and tier 2


# t1 and t2 mebmers:
#  Active members:
#   - According to AV2019 ep 43, there are 486 tier 2 members
#   - In AV2019 demographic data, the number of actives with yos <= 4 is 478
#   - For now, use yos <= 4 as tier 2 members
#   - In theory, some tier 2 police members should have yos = 5 (5 11/12). Should 
#     keep this in mind. 
#
#
#  Serivice retirees for regular members
#    - According to AV2019 ep 52, there are no any type of retirees in tier 2
#
#
#  Initial terminated members
#   - For now, we assume that for each tier the liability of initial terminated members(in or not in pay status) 
#     is a fixed percentage of the AL of retirees. 
#   - As we assume the tier 2 has no retirees in the model, there are no AL for initial terminated members 
#     under the current simplification method. The should not be an issue because the actual AL for termianted should be 
#     very small as tier 2 is still new. 





#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data    <- "inputs/data_proc/"
dir_outputs <- "model/tiers/tierData/"



# Model settings
range_age <- 20:100
range_ea  <- 20:64  # max retirement age is assumed to be 65 (qxr = 1 at age 65 in AV tables) 



# Tier specific parameters
tier_name <- "pf.t1"
age_vben  <- 55 # 50 if yos >= 25, 55 if yos < 25. assume 55 for all in the model
v.year    <- 0
fasyears  <- 1  
# bfactor   <- 0.02
cola_assumed <- 0.03 # assumed cola rates for valuation  
# EEC_rate <- 0.0735 # use EEC and ERC caps 


#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_SJPF_decrements_AV2019_imputed.RData"))
load(paste0(dir_data, "Data_SJPF_demographics_20190630_fillin.RData"))

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


#*******************************************************************************
#                      ## Decrements 1: combining groups ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "t1.police|t1.fire")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "t1.police","wgt"] <-  share_police
wgts[wgts$grp == "t1.fire","wgt"]   <-  share_fire


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
grp_include <- df_qxd_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "police|fire")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "police","wgt"] <-  share_police
wgts[wgts$grp == "fire",  "wgt"] <-  share_fire



## calculate weighted average
# Need to combine two types of disability rates: adding the two rates
df_qxd_tier <- 
  df_qxd_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxd = weighted.mean(qxd, wgt), .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Termination with refund

# groups included
grp_include <- df_qxt_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "police|fire")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "police","wgt"] <-  share_police
wgts[wgts$grp == "fire",  "wgt"] <-  share_fire



## calculate weighted average
df_qxt_tier <- 
  df_qxt_imputed %>% 
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
df_qxm_tier <- 
  df_qxm_imputed %>% 
  mutate(qxm.pre   = share_female * qxm.pre_female   + share_male * qxm.pre_male,
         qxm.post  = share_female * qxm.post_female  + share_male * qxm.post_male,
         qxmd.post = share_female * qxmd.post_female + share_male * qxmd.post_male,
         grp = tier_name
         ) %>% 
  select(grp, age, qxm.pre, qxm.post, qxmd.post)
  
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
         qxm.post, 
         qxmd.post, 
         qxt, 
         qxr, 
         qxd, 
         everything())%>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.)

# decrements_tier




#*******************************************************************************
#        ## Decrements 3: adding eligibility information ####
#*******************************************************************************

# Create 2 columns for each tier
 # elig_servRet_full:  number of year of being eligible for full or greater retirement benefits
 # elig_servRet_early: number of year of being eligible for early retirement benefits; 
 #             0 after being eligible for full retirement benefits

#   - Eligibility for unreduced benefits
#'       - age 55 & yos 20
#'       - age 50 & yos 25
#'       - age 70
#'       - yos 30 
#'
#'  - Eligibility for reduced benefits
#'      - age 50 & yos 20
#'      - Reduction factors
#'      - applied before the max benefit?


decrements_tier  %<>% 
  group_by(ea) %>% 
  mutate(
    # Eligibility for full (or greater) retirement benefit
    elig_servRet_full = ifelse(  (age >= 55 & yos >= 20)|
                                 (age >= 50 & yos >= 25)|
                                 (age >= 70)|
                                 (yos >= 30),  
                                 1, 0) %>% cumsum,
    
    # Eligibility for early retirement benefit
    elig_servRet_early = ifelse( (age >= 50 & yos >= 20), 1, 0) %>% cumsum,
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
#                      ## Decrements 4: Improvement table  ####
#*******************************************************************************



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
# wage inflation: 3.25% AV2019, ep 53

df_salScale_tier <- 
  df_salScale_imputed %>% 
  mutate(grp = tier_name,
         salScale = salScale_merit + 0.0325 ) %>%
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
         age <= 64,
         yos <= 44) # no members are removed


# df_n_actives_tier %>% pull(nactives) %>% sum

# CalPERS: Check total salary againt the AV value: payroll
# sum(df_n_actives_tier$nactives * df_n_actives_tier$salary)
# model/target: 12951558687/12950836352 = 100.0056%


# Keep classic members only
#  assume 
#    - members with yos > 4 are tier 1 members
 

df_n_actives_tier %<>%
  mutate(nactives = case_when(
    yos <= 4 ~ 0,
    TRUE ~ nactives
  ))

# df_n_actives_tier$nactives %>% sum



## Retirees (all types included)

# all service retirees are tier 1 members

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
    fasyears  = fasyears,  # based on policy before PEPRA
    cola_assumed = cola_assumed
    
    #bfactor = bfactor
    #EEC_rate = EEC_rate
  )


# Store all tier data in a list
assign(paste0("tierData_", tier_name), 
       
         list(
           tier_name = tier_name,
           
           decrements = decrements_tier,
           #decrements_improvement = decrements_improvement,
           
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


