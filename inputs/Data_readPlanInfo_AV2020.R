# Load the following plan information of PERF A:
 # Schedule of amortization payments for existing UAALs 



## Inputs
#   - inputs/data_raw/Data_MEPERS_planInfo_AV2020.xlsx"


## Outputs
#  - All tables in "tidy" format  





#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
# source("libraries.R")




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_dataRaw  <- "inputs/data_raw/"
fn_dataRaw   <- "Data_MEPERS_planInfo_AV2020.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


names_sheet <- excel_sheets(filePath_dataRaw)



#*******************************************************************************
#                      ## Initial amortization payments ####
#*******************************************************************************
init_amort_raw <- read_excel_range(filePath_dataRaw, "Init_amort")
init_amort_raw <- 
  init_amort_raw$df %>% 
  mutate(AV_date = init_amort_raw$tblInfo$AV_date) %>% 
  relocate(AV_date, grp)





#*******************************************************************************
#                      ## Unrecognized investment gains/losses  ####
#*******************************************************************************
init_unrecReturns.unadj <- read_excel_range(filePath_dataRaw, "Init_unrecReturn")
init_unrecReturns.unadj <-
  init_unrecReturns.unadj$df %>%
  mutate(AV_date = init_unrecReturns.unadj$tblInfo$AV_date)



#*******************************************************************************
#                      ## Save Data ####
#*******************************************************************************

save(init_amort_raw,
		 init_unrecReturns.unadj,
		 file = paste0(dir_dataOut, "Data_MEPERS_planInfo_AV2020.RData"))
