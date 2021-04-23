# Actuarial valuation for SJ plans
#
#
# Next step:
#   Workforce growth issue when modeling two active tiers joinly. 


# val_name_run <- val_runList$val_name[1]
# create_val_2tiers(val_name_run)

create_val <- function(val_name_run){

  
## dev --
  
# val_name_run <- val_runList$val_name[1]
  
## dev --  
  
cat("Creating valuation", val_name_run, "\n")
  
#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************


## Import global parameters
Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
  filter(!is.na(init_year)) %>% 
  as.list
 
## Import valuation parameters
val_paramlist <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
  filter(!is.na(val_name), val_name == val_name_run) %>% 
  as.list



## Additinal global variables 

# age and entry age ranges
Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)


# turn tier names into a character vector
val_paramlist$tier_include <-  rlang::parse_expr(paste0("c(", val_paramlist$tier_include, ")" )) %>% eval




#*******************************************************************************
#                         Load data                                         ####
#*******************************************************************************

# Load tier data, each tier is stored as an element in the list ls_tierData
dir_tierData <- "model/tiers/tierData/"
ls_tierData  <- list()

for (tierName in val_paramlist$tier_include){
  ls_tierData[[tierName]] <- 
         readRDS(paste0(dir_tierData, "tierData_", tierName,  ".rds" ))
  }

# ls_tierData$


#*******************************************************************************
#                          Data preparation                                 ####
#*******************************************************************************
source("model/valuation/model_val_prepDataFuns.R", local = TRUE)

## Modify tier parameters as specified in the parameter list   
# !! This should be run before any further operations!!


for (tierName in names(ls_tierData)){
  
  # dev --
  # tierName <- "pf.t1"
  # dev --
  
  ls_tierData[[tierName]] <- adj_tierParams(ls_tierData[[tierName]])
  
  
  ## 1. Full salary schedule
  ls_tierData[[tierName]] <- add_salary_full(ls_tierData[[tierName]])
  
  
  # 2. Distribution of new entrants
  ls_tierData[[tierName]] <- add_entrantsDist(ls_tierData[[tierName]])
  # ls_tierData$pf.t1$entrants_dist %>% plot # note that SJPF t1 is closed
  

  
  # 3. Create a generational decrement table
  ls_tierData[[tierName]] <- expand_decrements(ls_tierData[[tierName]])
  
  
  # 4. apply improvements
  # [Done] TODO: to be updated 
  # This step includes calibration of post-retirement mortality
  
  ls_tierData[[tierName]] <- apply_decImprovements(ls_tierData[[tierName]])
  
  # 5. Adjustments to retirement rates
  ls_tierData[[tierName]] <- adj_retRates(ls_tierData[[tierName]])
  
  
  # 6. Adjustments to initial members
  # This stip includes calibration of benefit payments in year 1
  ls_tierData[[tierName]] <- adj_initMembers(ls_tierData[[tierName]])
}


# ls_tierData$pf.t1$decrements_expanded %>% 
#   mutate(start_year = year - (age - ea)) %>% 
#   arrange(start_year,ea)
# 
# ls_tierData$pf.t1$decrements <- NULL
# ls_tierData$pf.t2$entrants_dist %>% plot


#*******************************************************************************
#                            calibration of salary                          ####
#*******************************************************************************
# tierName <- "pf.t1"
# ls_tierData[[tierName]]$salary_full %<>% 
#   mutate(sx = ifelse( year < 2019, sx * 1.2, sx))
  



#*******************************************************************************
#                            Demographics                                   ####
#*******************************************************************************
invisible(gc())
# source("model/valuation/model_val_demographics_singleTier.R", local = TRUE)
source("model/valuation/model_val_demographics_multiTier.R", local = TRUE)

pop <- get_demographics(ls_tierData)
# Note that the function returns a list



#*******************************************************************************
#      Individual actuarial liabilities, normal costs and benenfits    ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_indivLiab_flexbf(4).R", local = TRUE)

indivLiab <- list()

for (tierName in names(ls_tierData)){
  indivLiab[[tierName]] <- get_indivLiab(ls_tierData[[tierName]])
}


#*******************************************************************************
#     Aggregate actuarial liabilities, normal costs and benefits        ####
#*******************************************************************************
invisible(gc())
source("model/valuation/model_val_aggLiab.R", local = TRUE)

aggLiab <- list()

for (tierName in names(ls_tierData)){
  aggLiab[[tierName]] <- get_aggLiab(tierName, pop, indivLiab)
}

# aggLiab$miscAll


#*******************************************************************************
#   Simplification: Initial vested and inactives who are not in pay status  
#*******************************************************************************

# For initial PVB of terminated vested members 
#  - no corresponding demographic data 
#  - PVB = AL

# - Assume the PVFB for initial vested members are paid up through out the next 50 years. 
# - ALs and Bs of initial terminated vested and inactive members will be added to ALx.v and B.v. 
# - Based on method used in PSERS model. 

# TODO The current method may have issue there are active and non-active tiers 


for (tierName in names(ls_tierData)){

if (val_paramlist$estInitTerm){
  AL.init.defrRet <-  val_paramlist$AL_defrRet_pctALservRet * aggLiab[[tierName]]$servRet.la[1, "ALx.servRet.la"]
 
  
  df_init.defrRet <- data.frame(
    year = 1:51 + (Global_paramlist$init_year - 1),
    #B.init.v.yearsum = c(0, amort_cd(AL.init.v, paramlist$i, 50, TRUE))) %>% 
    B.init.defrRet = c(0, amort_cp(AL.init.defrRet, val_paramlist$i, 50, val_paramlist$startingSalgrowth, TRUE))) %>% 
    mutate(ALx.init.defrRet = ifelse(year == Global_paramlist$init_year, AL.init.defrRet, 0))
  
  for(i_v in 2:nrow(df_init.defrRet)){
    df_init.defrRet$ALx.init.defrRet[i_v] <- 
      with(df_init.defrRet, (ALx.init.defrRet[i_v - 1] - B.init.defrRet[i_v - 1]) * (1 + val_paramlist$i))
  }
  
  # df_init.vested
  
  aggLiab[[tierName]]$defrRet %<>% 
    as.data.frame() %>%
    left_join(df_init.defrRet, by = "year") %>%
    mutate_all(list(na2zero)) %>%
    mutate(ALx.defrRet = ALx.defrRet + ALx.init.defrRet,
           B.defrRet   = B.defrRet   + B.init.defrRet) %>%
    as.matrix
}

}

# aggLiab[[val_paramlist$tier_include[1]]]$defrRet




#*******************************************************************************
#            Aggregate valuation results of all tiers    ####
#*******************************************************************************
 
aggLiab$sumTiers <-
	get_AggLiab_sumTiers(aggLiab)

# aggLiab$sumTiers


#*******************************************************************************
#    plan information associated with this valuation        ####
#*******************************************************************************

load(filePath_planInfo)   %>% print


init_amort_raw_val <- list()

for (tierName in names(ls_tierData)){
  # tierName <- names(ls_tierData)[1]

  init_amort_include <- 
    case_when(
      tierName %in% "pf.t1" ~  c("pf.t1.fire", "pf.t1.police"),
      tierName %in% "pf.t2" ~  c("pf.t2.fire", "pf.t2.police")
      )
  
  init_amort_raw_val[[tierName]] <- 
    init_amort_raw %>% 
    filter(grp %in% init_amort_include)
  }


init_amort_raw_val$sumTiers <- 
  bind_rows(init_amort_raw_val)
  
  
init_unrecReturns.unadj_val <- init_unrecReturns.unadj # 


#*******************************************************************************
#  Save decrements          ####
#*******************************************************************************

decrements <- list()

for (tierName in names(ls_tierData)){
  decrements[[tierName]] <- ls_tierData[[tierName]]$decrements
}



#*******************************************************************************
#  Save outputs          ####
#*******************************************************************************

cat("Saving results...\n")
saveRDS(
    list(
      aggLiab   = aggLiab,
      indivLiab = indivLiab,
      pop = pop,
      init_amort_raw = init_amort_raw_val,
      decrements = decrements,
      init_unrecReturns.unadj = init_unrecReturns.unadj_val
    ),
  file = paste0(dir_outputs_val, "val_", val_name_run, ".rds")
)
}



# aggLiab

# ls_miscAll <- readRDS("model/valuation/outputs_val/val_miscAll_bf100_cola2.rds")
# ls_sftyAll <- readRDS("model/valuation/outputs_val/val_sftyAll_bf100_cola2.rds")
# 
# 
# ls_mix <- readRDS("model/valuation2/outputs_val/val_mix_bf100_cola2.rds")
# 
# 
# 
# x <- names(ls_miscAll$aggLiab$miscAll)[2]
# ls_miscAll$aggLiab$miscAll[[x]] - ls_mix$aggLiab$miscAll[[x]]
# 
# 
# x <- names(ls_sftyAll$aggLiab$sftyAll)[5]
# 100 * (ls_sftyAll$aggLiab$sftyAll[[x]] - ls_mix$aggLiab$sftyAll[[x]])/ls_sftyAll$aggLiab$sftyAll[[x]]

# 
# ls2 <- readRDS("model/valuation2/outputs_val/val_sfty2t_bf100_cola2.rds")
# 
# ls2$aggLiab$sfty_classic$servRet.la
# ls2$aggLiab$sfty_classic$active
# 
# ls2$aggLiab$sfty_classic$servRet.la
# ls2$aggLiab$sfty_pepra$servRet.la
# 
# 
# ls2$aggLiab$sumTiers$active
# ls2$aggLiab$sumTiers$servRet.la
# ls2$aggLiab$sumTiers$disbRet
# 
# 44511484637
# 
# 
# 
# ls <- readRDS("model/valuation2/outputs_val/val_misc2t_bf100_cola2.rds")
# 
# ls$aggLiab$misc_classic$servRet.la
# ls$aggLiab$misc_classic$active
# 
# ls$aggLiab$misc_classic$servRet.la
# ls$aggLiab$misc_pepra$servRet.la
# 
# 
# ls$aggLiab$sumTiers$active
# ls$aggLiab$sumTiers$servRet.la
# 
# 44511484637


# df <- readRDS(paste0(here::here(),"/model/valuation2/outputs_val/val_poff2t_bf100_cola2.rds"))
# 
# 
# df$aggLiab$sumTiers$active
# 
# df$aggLiab$sumTiers$disbRet
# 
# 22396502168 + 
# 15615394389 + 
# 11110941320 + 
#   649772643 + 
#  2463615238
# 
# 



