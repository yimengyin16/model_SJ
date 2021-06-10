## Loading decrement tables and salary scales from AV2019 of SJ polic and fire


## Inputs
#   - inputs/data_raw/Data_SJPF_decrements_AV2019.xlsx"


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
fn_dataRaw   <- "Data_SJPF_Decrements_AV2020.xlsx" 
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


df_qxr_t1_raw <- 
  read_excel_range(filePath_dataRaw, "servRet_t1")$df  %>% 
  # gather(yos_range, qxr, - grp,-age)
  mutate(tier = "t1") %>% 
  relocate(tier, grp)

df_qxr_t2_raw <- 
  read_excel_range(filePath_dataRaw, "servRet_t2")$df %>% 
  mutate(tier = "t2") %>% 
  relocate(grp) %>% 
  relocate(tier, grp)



#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************
## Target format, cols:
#' - grp
#' - age/yos
#' - qxd

df_qxd_raw <- 
  read_excel_range(filePath_dataRaw, "disbRet")$df %>% 
  mutate(age = as.numeric(age)) 





#*******************************************************************************
#                      ## Importing disability retirement rates, PERF ####
#*******************************************************************************
## For constructing alternative scenarios with lower disability rates

## Target format, cols:
#' - grp
#' - age/yos
#' - qxd

df_qxd_PERF_raw <- 
  read_excel_range(filePath_dataRaw, "disbRet_PERF")$df %>% 
  mutate(age = as.numeric(age)) 






#*******************************************************************************
#                      ## Importing termination rates ####
#*******************************************************************************
## Target format, cols:
#' - grp
#' - age/yos
#' - qxt


df_qxt_raw <-
    read_excel_range(filePath_dataRaw, "defrRet")$df %>%
    #gather(grp, qxt, -yos) %>%
    relocate(grp)







#*******************************************************************************
#                      ## Importing mortality  ####
#*******************************************************************************

# SJPF use Pub10


#' Labels in col names:
#'  - pre: pre-retirement
#'  - post： post-retirement
#'  - female/male

# df_qxm_raw <-
#     read_excel_range(filePath_dataRaw, "mortality")$df %>% 
#     mutate(across(everything(), na2zero ))






#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************


df_salScale_raw <-
    read_excel_range(filePath_dataRaw, "salScale")$df



#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************


df_qxr_t1_raw
df_qxr_t2_raw
df_qxd_raw
df_qxd_PERF_raw
df_qxt_raw
#df_qxm_raw
df_salScale_raw



save(
  df_qxr_t1_raw,
  df_qxr_t2_raw,
  df_qxd_raw,
  df_qxd_PERF_raw,
  df_qxt_raw,
  # df_qxm_raw,
  df_salScale_raw,
  
	file = paste0(dir_dataOut, "Data_SJPF_decrements_AV2020_raw.RData")
)




