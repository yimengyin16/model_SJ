## Loading demographic data of MEPERS as of 06/30/2020 in AV2020


## Inputs
#   - inputs/data_raw/Data_MEPERS_Demographics_20200630.xlsx"


## Outputs
#  - All tables in "tidy" format  


# Tables that are not loaded but are useful for modeling and/or calibration




#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
source("libraries.R")




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************
dir_dataRaw  <- "inputs/data_raw/"
fn_dataRaw   <- "Data_MEPERS_Demographics_20200630.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"

sheetNames <- excel_sheets(filePath_dataRaw)







#*******************************************************************************
#                     Importing data of active members  ####
#*******************************************************************************

# Cols in the target format:
#'  - AV_date  
#'  - grp     
#'  - age.cell, agegrp  
#'  - yos.cell, yosgrp 
#'  - nactives, salary



# Local helper function
tidy_memberData_matrix <- function(data_list){
  
  # data_list <- read_excel_range(filePath_dataRaw, "actives_misc_t1", "a2:b4")
  
  df       <- data_list$df
  grp_name <- data_list$tblInfo$grp_name
  AV_date  <- data_list$tblInfo$AV_date
  
  df %<>% 
    filter(!is.na(type)) %>% 
    #mutate(keyVar = paste0(type, age.cell)) %>% 
    gather(yos.cell, value, -type, -age.cell, -agegrp)
  
  df_yosgrp <- df %>% filter(type == "yosgrp") %>% select(yos.cell, yosgrp = value)
  
  df %>% 
    filter(type != "yosgrp") %>% 
    left_join(df_yosgrp, by = "yos.cell") %>% 
    mutate_at(vars(age.cell, yos.cell, value), list(~as.numeric(.))) %>% 
    mutate(AV_date = AV_date,
           grp = grp_name) %>% 
    spread(type, value) %>% 
    relocate(AV_date, grp, age.cell, agegrp, yos.cell, yosgrp) %>% 
    arrange(age.cell, yos.cell)
  
}

get_agecuts <- function(df){
  agecuts <- df %>% select(age.cell, agegrp) 
  agecuts <- agecuts[!duplicated(agecuts), ]
  agecuts %<>% 
    separate(agegrp, into = c("agelb", "ageub")) %>% 
    mutate(across(everything(), as.numeric ))
}

get_yoscuts <- function(df){
  yoscuts <- df %>% select(yos.cell, yosgrp)
  yoscuts <- yoscuts[!duplicated(yoscuts), ]
  yoscuts %<>% 
    separate(yosgrp, into = c("yoslb", "yosub")) %>% 
    mutate(across(everything(), as.numeric ))
}



# sheet names for actives
sheetNames_actives <- sheetNames[str_detect(sheetNames, "actives") & !str_detect(sheetNames, "_raw")]

# construct tidy data for actives
df_nactives_raw <-
  map(
    sheetNames_actives,
    ~ read_excel_range(filePath_dataRaw, ., "a2:b4") %>% tidy_memberData_matrix
  ) %>%
  bind_rows

# extract age and yos cuts
agecuts_actives <- get_agecuts(df_nactives_raw)
yoscuts_actives <- get_yoscuts(df_nactives_raw)




#*******************************************************************************
#                     Importing data of retirees  ####
#*******************************************************************************

# Cols in the target format:
#'  - AV_date  
#'  - grp     
#'  - age.cell, agegrp  
#'  - n_retirees_xx, n_benefit_xx (or variants)



# Local helper function
# tidy_retirees_calpers <- function(data_list){
#   
#   # data_list <- read_excel_range(filePath_dataRaw, "retirees_misc_t1", "a2:b4")
#   
#   df       <- data_list$df
#   grp_name <- data_list$tblInfo$grp_name
#   AV_date  <- data_list$tblInfo$AV_date
#   
#   df %>% 
#     filter(!is.na(type)) %>% 
#     gather(grp, value, -type, -age.cell, -agegrp) %>% 
#     unite("type", type, grp, sep = "_") %>% 
#     spread(type, value) %>% 
#     mutate(AV_date = AV_date,
#            grp = grp_name) %>% 
#     relocate(AV_date, grp, age.cell, agegrp) %>% 
#     arrange(age.cell)
# }

tidy_retirees_wide1 <- function(data_list){
  
  # cols in the input data frame:
  #  - age.cell, agegrp
  #  - var for number of retirees (eg. "servRet", "retirees_all")
  #  - var for benefit (eg. "benefit_tot", "benefit_avg" )
  
  # dev --
  #  data_list <- read_excel_range(filePath_dataRaw, "retirees_regular", "a2:b4")
  # dev --
  
  df       <- data_list$df
  grp_name <- data_list$tblInfo$grp_name
  AV_date  <- data_list$tblInfo$AV_date
  
  df %>% 
    filter(!is.na(age.cell)) %>% 
    #gather(grp, value, -type, -age.cell, -agegrp) %>% 
    #unite("type", type, grp, sep = "_") %>% 
    #spread(type, value) %>% 
    mutate(AV_date = AV_date,
           grp = grp_name) %>% 
    relocate(AV_date, grp, age.cell, agegrp) %>% 
    arrange(age.cell)
}



# sheet names for retirees
sheetNames_retirees <- sheetNames[str_detect(sheetNames, "retirees") & !str_detect(sheetNames, "_raw")]


# construct tidy data for retirees (all types)
df_nretirees_raw <-
  map(
    sheetNames_retirees,
    ~ read_excel_range(filePath_dataRaw, ., "a2:b4") %>% tidy_retirees_wide1
  ) %>%
  bind_rows

# extract age and yos cuts
agecuts_retirees <- get_agecuts(df_nretirees_raw)


# Calculate average benefit payments

df_nretirees_raw %<>% 
  mutate(benefit        = benefit_tot / n_retirees_all,
        ) %>% 
  mutate(across(!c(AV_date, grp, age.cell, agegrp), na2zero))



#*******************************************************************************
#                     ## Review and save results ####
#*******************************************************************************

df_nactives_raw
df_nretirees_raw

agecuts_actives
yoscuts_actives

agecuts_retirees


save(
  df_nactives_raw,
  df_nretirees_raw,
  
  agecuts_actives,
  yoscuts_actives, 
  
  agecuts_retirees,
		 
	file = paste0(dir_dataOut, "Data_MEPERS_demographics_20200630_raw.RData")
)







  
  



