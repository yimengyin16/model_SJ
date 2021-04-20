# This script prepares data for valuation




# 1. function for salary scale
#  - add_salary_full
  

# 2. function for distribution of new entrants
  # add_entrantsDist


# 3. function adjusting retirement rates for modeling





#*******************************************************************************
#                     Adjustments to tier parameters                        ####
#*******************************************************************************
adj_tierParams <- function(tierData,
                           val_paramlist_    =  val_paramlist,
                           Global_paramlist_ =  Global_paramlist){
  # 
  # tierData          <-  tierData_miscAll
  # val_paramlist_    <-  val_paramlist
  # Global_paramlist_ <-  Global_paramlist
  
  # Override assumed cola:
  
  assign_parmsList(val_paramlist_,    envir = environment())
  
  if(!is.na(val_paramlist_$cola_assumed_override)){
    tierData$tier_params$cola_assumed <- val_paramlist_$cola_assumed_override
  }
  
  return(tierData)
}





#*******************************************************************************
#                   Constructing full salary schedule                      #####                  
#*******************************************************************************

add_salary_full <- function(tierData, 
                            Global_paramlist_ = Global_paramlist,
                            val_paramlist_    = val_paramlist
                            ){

# dev -- 
# tierData          <- ls_tierData[[tierName]]  
# Global_paramlist_ <- Global_paramlist
# val_paramlist_    <- val_paramlist
# dev -- 



assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(val_paramlist_,    envir = environment())  

salScale_tier     <- tierData$df_salScale
df_n_actives_tier <- tierData$df_n_actives


# Step 0: Check compatibility
# max yos
max_yos_model <- max_retAge - min_age
max_yos_tier  <- max(salScale_tier$yos)
if(!max_yos_model <= max_yos_tier) stop("Incomplete yos range")

# ea range, N/A for MEPERS
# ea_range_tier  <- range(salScale_tier$ea)
# ea_range_model <- range(range_ea)
# if(!ea_range_tier[1]<=ea_range_model[1] | 
#    !ea_range_tier[2]>=ea_range_model[2]) stop("Incomplete ea range")


## Step 1. Create complete salary scale  

# This step generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
# to model. 

# - Salaries in year 1 are set to 1. 
# - For future workers (entry year greater than 1) whose spans of career years do not include year 1, 
#   assumption about their starting salary levels is applied.

# salScale_tier

range_start_year  <- (1 - (max_age - min_age)):nyear # smallest relevant start year is the entry year who is at the max_age in year 1
range_age_actives <- min_age:(max_retAge - 1)

salScale_full <- 
  expand_grid(start_year = range_start_year,
              ea         = range_ea, 
              age        = range_age_actives) %>% 
  filter(age >= ea, 
         start_year + (max_retAge - 1 - ea) >= 1  # workers must stay in workforce at least up to year 1. 
         ) %>%
  mutate(yos = age - ea) %>% 
  #left_join(select(salScale_tier, yos, salScale)) %>%  #, by = c("yos")) %>%
  left_join(salScale_tier) %>% 
  group_by(start_year, ea) %>% 
  mutate(year = start_year + (age - ea),
         growth_start = (1 + startingSalgrowth)^(start_year - 1),   # assume starting salary grows at the assumed value for all entry ages for all years
         scale_cum = cumprod(ifelse(age == ea, 1, lag(1 + salScale))), 
         scale_cum = ifelse(start_year <= 1, scale_cum/scale_cum[year == 1], # Salary levels before starting year are scaled based on salary in the initial year.
                            scale_cum * growth_start)) %>% 
  ungroup %>% 
  mutate(year       = init_year + year - 1,
         start_year = init_year + start_year - 1 # convert to valuation year 
  ) %>% 
  select(start_year, ea, age, year, scale_cum) %>% 
  arrange(start_year, ea, age)

# salScale_full %>% filter(start_year ==2021, ea == 30)


## Step 2. Supplement the inital salary table with all starting salary

# This function generates a table of initial salary (year 1) which include all starting salary levels (age = ea)

# If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
# extraploate the missing values. 

salary_tier <- 
  df_n_actives_tier %>% 
  mutate(age = ea + yos) %>% 
  select(ea, age, salary) 


salary_start <- 
  salary_tier %>% 
  as.data.frame() %>% # splong does not work well with tibbles 
  splong("ea", range_ea, method = "natural") %>% 
  filter(age == ea) %>% 
  select(-age) %>% 
  splong("ea", range_ea) %>% 
  mutate(age = ea,
         salary = ifelse(salary < 0, 0, salary))

salary_tier <- rbind(salary_tier, salary_start) 

salary_tier <- salary_tier[!duplicated(salary_tier[c("age","ea")]),]


# Step 3. Create complete salary history 

salary_full <- 
  salScale_full %>% 
  left_join(salary_tier, by = c("ea", "age")) %>% 
  group_by(start_year, ea) %>% 
  mutate(salary = na2zero(salary),
  	     sx = ifelse(start_year <= init_year, 
  	                 salary[year == init_year] * scale_cum, 
                     salary[age == ea]* scale_cum)) %>% 
  select(start_year, ea, age, year, sx)
  
# salary_full %>% filter(start_year == 2015 )

tierData$salary_full <- salary_full

return(tierData)

}


#*******************************************************************************
#        Infering ditribution of entrants from low yos actives             #####                  
#*******************************************************************************

add_entrantsDist <- function(tierData,          
                             yos_max = 3,
                             val_paramlist_    =  val_paramlist,
                             Global_paramlist_ =  Global_paramlist,
                             simple = FALSE){
  
  # Simple imputation rule is applied under the following circumstances:
  # 1. parameter "simple" is set to TRUE
  # 2. negative weights are generated by the regular rule. 

  ## dev -- 
  
  # tierData          <- ls_tierData[[tierName]]
  # yos_max           <- 3
  # val_paramlist_    <- val_paramlist
  # Global_paramlist_ <- Global_paramlist
  
  ## dev --
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(val_paramlist_,    envir = environment())   
  
  
  nactives_tier <- 
    tierData$df_n_actives %>% 
    mutate(age = ea + yos) %>% 
    select(age, ea, nactives)
  # nactives_tier
  
  
  # For safty, do interpolation for potential missing cells.   
  nactives_tier <- splong(nactives_tier, "ea", range_ea) %>% splong("age", range_ea) %>% filter(age >= ea)
  nactives_tier %>% spread(age, nactives)
  
  
  # For each ea,  Calculate average number of members with yos <= yos_max
  entrants <- 
    nactives_tier %>% 
    filter(age - ea <= yos_max) %>% 
    group_by(ea) %>% 
    summarise(avg_ent = mean(nactives), .groups = "drop")
  
  # Check negative numbers
  neg_ea <- entrants[which(entrants$avg_ent < 0), "ea"]
  
  if(any(entrants$avg_ent < 0)){warning("\n",  tierData$tier_params$tier_name, ":", "\n", "Negative inferred value(s) in the following entry age(s): " , as.character(neg_ea), "\n", "Values will be coerced to 0." )
    #"  Simple imputation rule is applied")
    #ent <- nact1                          
  }
  
  entrants %<>% 
    mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))
  
  
  # ## Distributon by simple rule
  # nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = range_ea))
  # N <- 1
  # while(any(is.na(nact1$avg_ent))) {
  #   if(N <= length(nact1)) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent)) else
  #     nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lead(avg_ent) , avg_ent))
  #   N <- N + 1
  #   }
  
  # ent %<>% mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))
  
  # if(simple) ent <- nact1
  
  dist <- lowess(entrants$avg_ent, f= 0.1)$y
  dist <- dist/sum(dist)
  names(dist) <- range_ea
  
  # plot(dist)
  
  # add back to tier data
  tierData$entrants_dist <- dist
  
  return(tierData)
  
}




#*******************************************************************************
#            Modifying retirement rates for the purpose of modeling         ####
#*******************************************************************************

# Adjustment to the decrement table:

# Why  
#   In Winklevoss, retirement is treated as an immediate event, that is, retirees would start receiving benefit payments
# the same year when they retire. This is different from how diability and death benefits are treated, for which beneficiaries
# start receiving benefits the next year disability/death occurs, and can cause difficulty in modeling retirement with multiple 
# possible retirement ages (need to check). 
#
#   To facilitate modeling and maintain consistent treatment aross all types of benefits, 
# we assume that retirement occurs at the end of year t-1 for those who start receiving benefits at the begining of year t. Since 
# theoretically year end of t-1 and year begining of t is the same point of time, we maintained the same theoretical treatment of 
# retirement in Winklevoss (retirement is a immediate event); while assigning the retirement event and benefit payment event into two model
# periods allow retirement to be modeled the same manner as other benefit types. 

#   Note that since retirement is assumed to occur at the year end of the previous year, the retirement rate of year t is applied to 
# those who survived all other types of separation events (death, termination, disability).

# How
# 	Move qxr backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is qxr(t)*(1 - qxt(t-1) - qxm(t-1) - qxd(t-1))
# For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
# which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
# whether they will die at r.max)      

# share of contigent annuity and life annuity. 
# TEMP: For now, assume all members opt for life annuity


adj_retRates <- function(tierData,
                         val_paramlist_    =  val_paramlist,
                         Global_paramlist_ =  Global_paramlist){

# dev --   

# tierData          <- ls_tierData[[tierName]]
# val_paramlist_    <-  val_paramlist
# Global_paramlist_ <-  Global_paramlist

# dev --   
  
assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(val_paramlist_,    envir = environment())   

decrements_tier <- tierData$decrements
#decrements_tier %>% head


## For now, assume all retirees choose life annuity
#  - la: life annuity
#  - ca: Contingent annuity

pct_ca <- 0          # percentage choosing contingent annuity 
pct_la <- 1 - pct_ca # percentage choosing life annuity


decrements_tier %<>% 
  group_by(ea) %>%  
  mutate(qxr = ifelse(age == max_retAge - 1,
                      1 - qxt - qxm.pre - qxd, 
                      lead(qxr) * (1 - qxt - qxm.pre - qxd)), # Total probability of retirement
         
         qxr.la = ifelse(age == max_retAge, 0 , qxr * pct_la),  # Prob of opting for life annuity
         qxr.ca = ifelse(age == max_retAge, 0 , qxr * pct_ca),  # Prob of opting for contingent annuity
  )   

tierData$decrements <- decrements_tier

return(tierData)


######!!!! need to construct retirement age dependent mortality for life annuitants.
# For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
#mutate(qxm.la.r   = qxm.r) 
}



#*******************************************************************************
#           Creating a generational decrement table for the model           ####
#*******************************************************************************

expand_decrements <- function(tierData,
                              val_paramlist_    =  val_paramlist,
                              Global_paramlist_ =  Global_paramlist){
  
  # # dev --   
  # 
  # # tierData          <- ls_tierData[[tierName]]
  # # val_paramlist_    <-  val_paramlist
  # # Global_paramlist_ <-  Global_paramlist
  # 
  # # dev -- 
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(val_paramlist_,    envir = environment())   
  
  decrements_tier <- tierData$decrements
  decrements_tier
  
  # dims of decrement_ter: ea x age
  # dims of expanded decrement table: year x ea x age
  
  # range_start_year <- 1915:(init_year + nyear - 1) 
  # starting from 1915 is more than enough, just be safe
  range_year_decrements <- 1915:(init_year + nyear + max_age) 
  
  decrements_tier_expanded <- 
    expand_grid(year = range_year_decrements,
                age  = range_age, 
                ea   = range_ea) %>% 
    mutate(yos = age - ea) %>% 
    filter(age >= ea
           # start_year + (max_retAge - 1 - ea) >= 1
    ) %>% 
    left_join(decrements_tier, by = c("ea", "age", "yos")) %>%  
    colwise(na2zero)(.) %>% 
    relocate(year, ea, age, yos)%>%          
    arrange(year, ea, age)
  
  tierData$decrements_expanded <- decrements_tier_expanded
  
  return(tierData)
  
}



#*******************************************************************************
#                        Apply improvement            ####
#*******************************************************************************
# plan specific



apply_decImprovements <- function(tierData,
                                  val_paramlist_    =  val_paramlist,
                                  Global_paramlist_ =  Global_paramlist){
# 
  # dev --   
  
  # tierData          <- ls_tierData[[tierName]]
  # val_paramlist_    <-  val_paramlist
  # Global_paramlist_ <-  Global_paramlist
  
  # dev -- 
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(val_paramlist_,    envir = environment())   
  
  decrements_expanded    <- tierData$decrements_expanded
  decrements_improvement <- tierData$decrements_improvement
  
  
  ## range of age the original improvement table covers
  range_year_imprTab <- range(decrements_improvement$year) 
  
  ## expand the improvement table to cover all ages in range_age 
   # For each age, filling the missing years with the end values in the original tabel
  decrements_improvement <- 
    expand_grid(year = decrements_expanded$year %>% unique(),
                age  = range_age) %>% 
    left_join(decrements_improvement, by = c("year", "age")) %>% 
    group_by(age) %>% 
    mutate(across(
      !c(year), # should not include the grouping variable
      ~ifelse(year < range_year_imprTab[1], .x[year == range_year_imprTab[1]], .x )
    )) %>% 
    mutate(across(
      !c(year),
      ~ifelse(year > range_year_imprTab[2], .x[year == range_year_imprTab[2]], .x )
    )) %>% 
    ungroup
  
  
  ## Merging the improvement table to the expanded decrement table and adjust
  #  the according decrement rates
  
  # decrements_expanded %<>%
  #   left_join(decrements_improvement, by = c("year", "age")) %>% 
  #   
  #   # applying improvements
  #   mutate(qxm.post_female = qxm.post_female * impr_qxm.post_female,
  #          qxm.post_male   = qxm.post_male   * impr_qxm.post_male,
  #          
  #          qxmd.post.nonocc_female = qxmd.post.nonocc_female * impr_qxmd.post.nonocc_female,
  #          qxmd.post.nonocc_male   = qxmd.post.nonocc_male   * impr_qxmd.post.nonocc_male,
  #          
  #          qxmd.post.occ_female = qxmd.post.occ_female * impr_qxmd.post.occ_female,
  #          qxmd.post.occ_male   = qxmd.post.occ_male   * impr_qxmd.post.occ_male
  #          ) 
  
  decrements_expanded %<>%
    left_join(decrements_improvement, by = c("year", "age", "grp")) %>% 
    
    # applying improvements
    mutate(qxm.post  = qxm.post  * impr_qxm.post,
           qxmd.post = qxmd.post * impr_qxmd.post) %>% 
    colwise(na2zero)(.) %>% 
    ungroup
  
  ## update the combined rates.
  ## Should use the same set of weights when constructing tier 
  
  ## TODO: This does not belong to this function, need figure out how to move it to other functions
   
  # if(tierData$tier_name == "miscAll"){
  #   decrements_expanded %<>% 
  #   mutate(
  #     qxm.post         = 0.6 * qxm.post_female         + 0.4 * qxm.post_male,
  #     qxmd.post.nonocc = 0.6 * qxmd.post.nonocc_female + 0.4 * qxmd.post.nonocc_male,
  #     qxmd.post.occ    = 0.1 * qxmd.post.occ_female    + 0.9 * qxmd.post.occ_male,
  #     qxmd.post        = 0.8 * qxmd.post.nonocc        + 0.2 * qxmd.post.occ
  #   ) %>% 
  #   colwise(na2zero)(.) %>% 
  #   ungroup
  # }
  # 
  # if(tierData$tier_name == "sftyAll"){
  #   decrements_expanded %<>% 
  #     mutate(
  #       qxm.post         = 0.1 * qxm.post_female         + 0.9 * qxm.post_male,
  #       qxmd.post.nonocc = 0.1 * qxmd.post.nonocc_female + 0.9 * qxmd.post.nonocc_male,
  #       qxmd.post.occ    = 0.1 * qxmd.post.occ_female    + 0.9 * qxmd.post.occ_male,
  #       qxmd.post        = 0.5 * qxmd.post.nonocc        + 0.5 * qxmd.post.occ
  #     ) %>% 
  #     colwise(na2zero)(.) %>% 
  #     ungroup
  # }
  # 
    
  ## Calibration
  decrements_expanded %<>%  
    mutate(qxm.post = (1 + calib_qxm.post) * qxm.post)
  
  tierData$decrements_expanded <- decrements_expanded
  
  return(tierData)
  
}




#*******************************************************************************
#                     Adjustments to initial members                        ####
#*******************************************************************************


adj_initMembers <- function(tierData,
                            val_paramlist_    =  val_paramlist,
                            Global_paramlist_ =  Global_paramlist){

  # dev --   
  
  # tierData          <- ls_tierData[[tierName]]
  # val_paramlist_    <-  val_paramlist
  # Global_paramlist_ <-  Global_paramlist
  
  # dev -- 
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(val_paramlist_,    envir = environment())   

  # For modeling purposes, age of entry and age of retirement must be added to
  # data for all types of retirees. The following assumptions are made:
  #  - Entry age: The minimum age allowed in the model. 
  #    TODO / WARNING: may cause issue if the data has non-zero members at the min age. 
  #  - age of retirement: the current age,  which implies that the retirement year 
  #    is the first simulation year 
  # 
  
  tierData$df_n_servRet %<>% 
    mutate(year       = init_year,
           ea         = min_age,
           age_servRet= age,
           start_year = year - (age - ea),
           benefit_servRet = (1 + B_adjust) * benefit_servRet
           ) %>% 
    relocate(grp, start_year, year, ea, age, age_servRet)
  
  
  # N/A for MEPERS
  # tierData$df_n_disbRet %<>% 
  #   mutate(year       = init_year,
  #          ea         = min_age,
  #          age_disbRet= age,
  #          start_year = year - (age - ea),
  #          benefit_disbRet = (1 + B_adjust) * benefit_disbRet) %>% 
  #   relocate(grp, start_year, year, ea, age, age_disbRet)
  
  
  return(tierData)
  
}  












