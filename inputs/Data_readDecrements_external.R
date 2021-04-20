## Loading SOA Pub10 mortality table and MP2019 improvement table


 
#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
# source("libraries.R")




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_dataRaw  <- "inputs/data_raw/"
#filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


#*******************************************************************************
#                      ## Importing pre-retirement mortality  ####
#*******************************************************************************

# Sheets to read
#' PubS 2010 (A)
#' PubS 2010
#' PubG 2010




# Import mortality data 
df_pubS2010   <- read_excel(paste0(dir_dataRaw, "/Pub2010/pub-2010-amount-mort-rates.xlsx"), sheet = "PubS-2010", skip = 4)[, c(-2,-7, -12, -13)]  # exclude an empty column
df_pubG2010   <- read_excel(paste0(dir_dataRaw, "/Pub2010/pub-2010-amount-mort-rates.xlsx"), sheet = "PubG-2010", skip = 4)[, c(-2,-7, -12, -13)]
df_pubS2010A  <- read_excel(paste0(dir_dataRaw, "/Pub2010/pub-2010-amount-mort-rates.xlsx"), sheet = "PubS-2010(A)", skip = 4)[, c(-2,-6, -10, -11)]


names(df_pubS2010) <- c("age", "qxm.employee.female", "qxm.healthyRet.female", "qxm.disbRet.female","qxm.survivor.female",  
                               "qxm.employee.male",   "qxm.healthyRet.male",   "qxm.disbRet.male",  "qxm.survivor.female")
names(df_pubG2010) <- c("age", "qxm.employee.female", "qxm.healthyRet.female", "qxm.disbRet.female","qxm.survivor.female",  
                               "qxm.employee.male",   "qxm.healthyRet.male",   "qxm.disbRet.male",  "qxm.survivor.female")


names(df_pubS2010A)  <- c("age", "qxm.employee.female", "qxm.healthyRet.female", "qxm.survivor.female",  
                                 "qxm.employee.male",   "qxm.healthyRet.male",   "qxm.survivor.female")

Pub2010_raw <- list(pubS2010 = df_pubS2010,
                    pubG2010 = df_pubG2010, 
                    pubS2010A = df_pubG2010)


# Import projection scale (scale BB-2D)


MP2019_male_raw <- 
  read_excel(paste0(dir_dataRaw, "MP2019/scale-mp-2019-rates.xlsx"), sheet = "Male", skip = 1) %>% 
  rename(age = 1) %>% 
  filter(!is.na(age)) %>% 
  mutate(age = 20:120, 
         gender = "male")
names(MP2019_male_raw) <- c("age",1951:2035,"gender")

MP2019_female_raw <- 
  read_excel(paste0(dir_dataRaw, "MP2019/scale-mp-2019-rates.xlsx"), sheet = "Female", skip = 1) %>% 
  rename(age = 1) %>% 
  filter(!is.na(age)) %>% 
  mutate(age = 20:120, 
         gender = "female")
names(MP2019_female_raw) <- c("age",1951:2035,"gender")


MP2019_raw <- list(
  male   = MP2019_male_raw,
  female = MP2019_female_raw
)


# Expand the scales to 1915-2164
# 1915: the year when a 120-year old retiree in 2015 was at age 20. 
# 2235: the year when a 20-year old new entrant in 2115 will be at age 120.
# The scale BB-2D covers year 1951-2030. Years before 1951 use the scale for 1951, and years after 2030 use the scale for 2030. 





#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************

# Check output data frames:
Pub2010_raw
MP2019_raw


# save RP2014 and MR2018 in separate files

saveRDS(
  Pub2010_raw,
  file = paste0(dir_dataOut, "pub2010_raw.rds")
)

saveRDS(
  MP2019_raw,
  file = paste0(dir_dataOut, "MP2019_raw.rds")
)

