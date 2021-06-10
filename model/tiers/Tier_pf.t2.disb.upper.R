# Constructing tier SJPF tier 2



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_SFPF_decrements_AV2020_imputed.RData
#'   - inputs/data_proc/Data_SFPF_demographics_2020630_fillin.RData


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
#'  - SJPF tier 2 members
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
#'       - 5 years?
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
#'     - only the first 2: max of 50% of fas and servRet benefit 


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



## Assumptions, needs to be revisited 


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
share_female <- 1 - share_male


## Assumptions on demographics 

#'  - SJPF tier 1 members
#'     - Police: hired before Aug 4, 2013
#'     - Fire  : hired before Jan 2, 2015
#'     
#'  How to allocate total active and retiree numbers to tier 1 and tier 2


# t1 and t2 mebmers:
#  Active members:
#   - According to AV2020 ep 46, there are 565 tier 2 members, and 1144 tier 1 members
#   - In AV2020 imputed demographic data, the number of actives with yos <= 5 is 551
#   - For now, use yos <= 5 as tier 2 members
#   - In theory, some tier 2 police members should have yos = 6 (6 11/12)?. Should 
#     keep this in mind. 
#
#
#  Serivice retirees for regular members
#    - According to AV2020 ep 30, there are no any type of retirees in tier 2
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
tier_name <- "pf.t2.disb.upper"
age_vben  <- 60 # AV2019 ep60, at age 60 for Tier 2 vested members
v.year    <- 5
fasyears  <- 3  
# bfactor   <- 0.02
cola_assumed <- 0.02 # assumed cola rates for valuation  
# EEC_rate <- 0.0735 # use EEC and ERC caps 


#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_SJPF_decrements_AV2020_imputed.RData"))
load(paste0(dir_data, "Data_SJPF_demographics_20200630_fillin.RData"))
df_mp2019_raw <- readRDS(paste0(dir_data, "MP2019_raw.rds"))





#*******************************************************************************
#                      ## Decrements 1: combining groups ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "t2.police|t2.fire")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "t2.police","wgt"] <-  share_police
wgts[wgts$grp == "t2.fire","wgt"]   <-  share_fire


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



## Disability retirement rates, San Jose

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



## Disability retirement rates, PERF

# Disability rates are contructed based on CalPERS rates as follows:
#   - Fire: simple average of POFF rates and public agency rates for fire
#   - Policy: simple average of POFF rates and public agency rates for police

df_qxd_perf <- 
  bind_rows(
    df_qxd_PERF_imputed %>% 
      filter(grp %in% c("perf_pa.fire", "perf_poff")) %>% 
      group_by(age) %>% 
      summarise(qxd = mean(qxd), .groups = "drop") %>% 
      mutate(grp = "perf_fire"),
    
    df_qxd_PERF_imputed %>% 
      filter(grp %in% c("perf_pa.police", "perf_poff")) %>% 
      group_by(age) %>% 
      summarise(qxd = mean(qxd), .groups = "drop") %>% 
      mutate(grp = "perf_police")
  )

# groups included
grp_include <- df_qxd_perf$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "perf_police|perf_fire")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "perf_police","wgt"] <-  share_police
wgts[wgts$grp == "perf_fire",  "wgt"] <-  share_fire

## calculate weighted average
# Need to combine two types of disability rates: adding the two rates
df_qxd_perf_tier <- 
  df_qxd_perf %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxd = weighted.mean(qxd, wgt), .groups = "drop") %>% 
  mutate(grp = tier_name,) %>% 
  rename(qxd_perf = qxd) %>% 
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
#   Based on SOA pub 2010 decrement table




ls_pub2010_raw <- readRDS(paste0(dir_data, "pub2010_raw.rds"))

df_qxm_tier <- 
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


df_qxm_tier %<>% 
  mutate(
    qxm.pre_female   = 0.979 * qxm.pre_female,
    qxm.pre_male     = 0.979 * qxm.pre_male ,
    qxm.post_female  = 1.002 * qxm.post_female,
    qxm.post_male    = 1.002 * qxm.post_male,
    qxmd.post_female = 0.915 * qxmd.post_female,
    qxmd.post_male   = 0.915 * qxmd.post_male
  )


df_qxm_tier <-
  df_qxm_imputed %>%
  mutate(qxm.pre   = share_female * qxm.pre_female   + share_male * qxm.pre_male,
         qxm.post  = share_female * qxm.post_female  + share_male * qxm.post_male,
         qxmd.post = share_female * qxmd.post_female + share_male * qxmd.post_male,
         grp = tier_name
         ) %>%
  select(grp, age,
         qxm.pre,   qxm.pre_female,  qxm.pre_male,
         qxm.post,  qxm.post_female, qxm.post_male,
         qxmd.post, qxmd.post_female,qxmd.post_male)



# df_qxr_tier
# df_qxd_tier
# df_qxt_tier
# df_qxm_tier




#*******************************************************************************
#        ## Decrements 2: Single decrement table ####
#*******************************************************************************

decrements_tier <- 
  expand.grid(age = range_age, 
              ea  = range_ea) %>% 
  mutate(yos = age - ea,
         grp = tier_name) %>% 
  filter(age >= ea) %>% 
  left_join(df_qxm_tier,        by = c("grp", "age")) %>%         # mortality 
  left_join(df_qxt_tier,        by = c("grp", "yos")) %>%         # termination 
  left_join(df_qxr_tier,        by = c("grp", "age", "yos")) %>%  # service retirement
  left_join(df_qxd_tier,        by = c("grp", "age")) %>%         # disability, San Jose
  left_join(df_qxd_perf_tier,   by = c("grp", "age")) %>%         # disability, perf

  select(grp, ea, age, yos, 
         qxm.pre,   
         qxm.pre_female, qxm.pre_male,
         qxm.post,  qxm.post_female,  qxm.post_male,
         qxmd.post, qxmd.post_female, qxmd.post_male,
         qxt, 
         qxr, 
         qxd, 
         qxd_perf,
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
 #                     0 after being eligible for full retirement benefits
 # year_b4full: number of years below the full/unreduced retirement age

#   - Eligibility for unreduced benefits
#'       - age 57 & yos 5
#'
#'  - Eligibility for reduced benefits
#'      - age 50 & yos 5
#'      - reduction: 7% per year before age 57



decrements_tier  %<>% 
  group_by(ea) %>% 
  mutate(
    # Eligibility for full (or greater) retirement benefit
    elig_servRet_full = ifelse( (age >= 57 & yos >= 5),
                                 1, 0) %>% cumsum,
    
    # Eligibility for early retirement benefit
    elig_servRet_early = ifelse( (age >= 50 & yos >= 5), 1, 0) %>% cumsum,
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
#   ## Decrements 3.2: Disability rates based on perf   ####
#*******************************************************************************

# Adjustments to service retirement rates
#  - If eligible for service retirement, the difference between the SJ disability rate and the CalPERS based disability rate will be added to service retirement rate.
#  - If not eligible for service retirement, no adjustment will be made to other types of decrements.

decrements_tier %<>% 
  mutate(qxr_adj = ifelse(elig_servRet_full|elig_servRet_early, qxr + (qxd - qxd_perf), qxr),
         qxr_adj = pmin(1, qxr_adj) 
  ) 
  # %>% 
  # select(grp, ea, age, qxr_adj, qxr) %>% 
  # filter(ea == 45)


## replace the SJ rates with perf-based rates
decrements_tier %<>% 
  mutate(qxd = qxd_perf,
         qxr = qxr_adj)



## settting healthy mortality equal to disability mortality
decrements_tier %<>% 
  mutate(qxm.post = qxmd.post,
         qxm.post_male = qxmd.post_male,
         qxm.post_female = qxmd.post_female)



#*******************************************************************************
#                      ## Decrements 4: Improvement table  ####
#*******************************************************************************

# Target format:
#  data frame indexed by year and age. 
#  each row is the improvement factor to be applied to the value in that year-age cell  


# extending to 1900 to 2220
#  -  assume 0 for year < 1951
#  -  assume 2035 value for year > 2035

df_mp2019 <- 
  bind_rows(

  df_mp2019_raw$male %>% 
  gather(year, fct, -age, -gender) %>% 
  mutate(year = as.numeric(year),
         fct = as.numeric(fct)),

  df_mp2019_raw$female %>% 
    gather(year, fct, -age, -gender) %>% 
    mutate(year = as.numeric(year),
           fct = as.numeric(fct))
)


decrements_improvement <- 
  expand_grid(gender = c("male", "female"), 
              age    = range_age,
              year   = 1900:2220) %>% 
  left_join(df_mp2019,
            by = c("gender", "age", "year"))  
  

decrements_improvement %<>% 
  group_by(gender, age) %>% 
  mutate(fct = ifelse(year < 1951, 0, fct),
         fct = ifelse(year > 2035, fct[year ==  2035], fct),
         ) %>% 
  mutate(impr = ifelse(year > 2010, lag(cumprod(1 - ifelse(year>=2010,fct,0))), 1),
         impr = ifelse(year < 2010, lead(order_by(-year,  cumprod(1/(1 - ifelse(year<=2010,fct,0))))), impr)
  )


decrements_improvement %<>% 
  select(-fct) %>% 
  spread(gender, impr) %>% 
  rename(impr_male   = male,
         impr_female = female)




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

  filter(ea >=20,
         age <= 64,
         yos <= 44) # no members are removed


# df_n_actives_tier %>% pull(nactives) %>% sum




# Keep tier 2 members only
#  assume 
#    - members with yos <= 4 are tier 2 members
 

df_n_actives_tier %<>%
  mutate(nactives = case_when(
    yos > 5 ~ 0,
    TRUE ~ nactives
  ))

df_n_actives_tier$nactives %>% sum # 551, compared to 565 in AV2020
weighted.mean(df_n_actives_tier$salary, df_n_actives_tier$nactives) #11.16k vs 11.28k from AV2019
sum(df_n_actives_tier$salary*df_n_actives_tier$nactives) # total: $63.87m vs 60.92 from AV2020, ep29


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

# no retirees in tier 2
df_n_servRet_tier %<>% 
  mutate(n_servRet = 0)
  


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


