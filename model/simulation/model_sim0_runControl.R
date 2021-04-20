# Actuarial valuation for SJ plans


# Valuation name
rm(list = ls())

source("libraries.R")

#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************
## File path of the run control file

dir_runControl <- "model/"
fn_runControl  <- "RunControl.xlsx"
filePath_runControl <- paste0(dir_runControl, fn_runControl)

dir_outputs <- "model/simulation/outputs_sim/"



## Import global parameters
Global_paramlist <- read_excel(filePath_runControl, sheet="GlobalParams") %>% 
  filter(!is.na(init_year)) %>% 
  as.list

## Additinal global variables 

# age and entry age ranges
Global_paramlist$range_age <- with(Global_paramlist, min_age:max_age)
Global_paramlist$range_ea  <- with(Global_paramlist, min_ea:max_ea)


## Import valuation parameters
sim_runList <- read_excel(filePath_runControl, sheet="params_sim", skip  = 3) %>% 
  filter(!is.na(sim_name), include == TRUE) 

## Import investment return scenarios
returnScenarios <- read_excel(filePath_runControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))



#*******************************************************************************
#                           ### Run simulations ####                      
#*******************************************************************************


for(sim_name_run in sim_runList$sim_name){
  
  # sim_name_run <- "Dev_pf.t1"
  sim_paramlist <- filter(sim_runList, sim_name == sim_name_run) %>% as.list
 
  source("model/simulation/model_sim0_master.R")
  
}









