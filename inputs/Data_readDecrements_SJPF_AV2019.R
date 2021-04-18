## Loading decrement tables and salary scales from AV2020 of MEPERS


## Inputs
#   - inputs/data_raw/Data_MEPERS_decrements_AV2020.xlsx"


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
fn_dataRaw   <- "Data_MEPERS_Decrements_AV2020.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


names_sheet <- excel_sheets(filePath_dataRaw)




#*******************************************************************************
#                      ## Importing service retirement rates ####
#*******************************************************************************

## Target format, cols:
#' - age/yos
#' - grp
#' - qxr

# Note for MEPERS:
#  - by age for regular members and by YOS for special members
#  - do not use standard functions


# Local helper function (not used for MEPERS)
# get_servRetRates <- function(sheetName, fileName = filePath_dataRaw){
#   
#   ls <- read_excel_range(fileName, sheetName) 
#   
#   var_name <- ls$tblInfo$var_name # ls$tblInfo[ls$tblInfo$var == "var_name", "value"][[1]] 
#   grp_name <- ls$tblInfo$grp_name # ls$tblInfo[ls$tblInfo$var == "grp_name", "value"][[1]] 
#   
#   
#   ls$df %>% 
#     gather(yos, value, -age) %>% 
#     rename(!!var_name := value) %>% 
#     mutate(grp = grp_name) %>% 
#     select(grp, everything())
# }
# 
# sheetNames_serRet <- names_sheet[str_detect(names_sheet, "servRet") & !str_detect(names_sheet, "_raw")]
# 
# df_qxr_raw <- map(sheetNames_serRet, get_servRetRates) %>% 
#   bind_rows() %>% 
#   mutate(yos = as.numeric(yos))


# For MEPERS:
df_qxr_regular_raw <- 
  read_excel_range(filePath_dataRaw, "servRet_regular")$df %>% 
  gather(grp, qxr, -age) %>% 
  relocate(grp)

df_qxr_special_raw <- 
  read_excel_range(filePath_dataRaw, "servRet_special")$df %>% 
  gather(grp, qxr, -yos) %>% 
  relocate(grp)



#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************
## Target format, cols:
#' - grp
#' - age/yos
#' - qxr

df_qxd_raw <- 
  read_excel_range(filePath_dataRaw, "disbRet")$df %>% 
  mutate(age = as.numeric(age)) 




#*******************************************************************************
#                      ## Importing termination rates ####
#*******************************************************************************
## Target format, cols:
#' - grp
#' - age/yos
#' - qxr


df_qxt_raw <-
    read_excel_range(filePath_dataRaw, "defrRet")$df %>%
    gather(grp, qxt, -yos) %>%
    relocate(grp)







#*******************************************************************************
#                      ## Importing mortality  ####
#*******************************************************************************


#' Labels in col names:
#'  - pre: pre-retirement
#'  - post： post-retirement
#'  - female/male

df_qxm_raw <-
    read_excel_range(filePath_dataRaw, "mortality")$df %>% 
    mutate(across(everything(), na2zero ))



#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************


df_salScale_raw <-
    read_excel_range(filePath_dataRaw, "salScale")$df



#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************


df_qxr_regular_raw
df_qxr_special_raw
df_qxd_raw 
df_qxt_raw
df_qxm_raw
df_salScale_raw



save(
  df_qxr_regular_raw,
  df_qxr_special_raw,
  df_qxd_raw,
  df_qxt_raw,
  df_qxm_raw,
  df_salScale_raw,
  
	file = paste0(dir_dataOut, "Data_MEPERS_decrements_AV2020_raw.RData")
)




