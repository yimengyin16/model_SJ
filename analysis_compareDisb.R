
source("libraries.R")


fn <- "inputs/data_raw/disb/PR_EachDataSet_FY2016-17toFY2018-19_20201209_V1.xlsx"

df_include <- 
	tribble(
	~ planName, ~planid, 
	#"City of Alameda Police and Fire Pension 1079 and 1082",	7218,
	# "City of Albany Police and Fire Relief Pension Fund",	7219,
	# "City of Bakersfield Firemen's Disability and Retirement System",	7234,
  # "City of Eureka Fire and Police Retirement System",	7233,
	"City of Fresno Fire and Police Retirement System",	7231,
	"City of Los Angeles Fire and Police Pension System",	7236,
	"City of Oakland Fire and Police Retirement System",	7222,
	"City of Pasadena Fire and Police Retirement System",	7239,
	"City of Piedmont Police and Fire Pension Plan",	7224,
	#"City of Richmond Police and Firemen's Pension Plan",	7228,
	"City of San Jose Police and Fire Retirement Plan",	7250,
	#"City of Santa Barbara Police and Fire Pension Fund",	7248,
	# "Public Employees' Retirement Fund (PERF) A", 12892
	"San Diego City Employees' Retirement System", 7246

	)



df_benDisb <- 
  read_excel(fn, sheet = "PR_DEDUCTIONS") %>% 
	select(planName = "Entity Name",
		     planid = "Entity ID", 
				 year   = "Fiscal Year",
				 Ben_disbRet_safety = "Safety_Disability Retirement_Benefit Payments",
				 Ben_servRet_safety = "Safety_Service Retirement_Benefit Payments",
				 Ben_other_safety   = "Safety_Other Benefit Payments_Benefit Payments"
				 ) %>% 
	mutate(across(c(-planName, planid, year), ~ na2zero(as.numeric(.x))  )) %>% 
	mutate(Ben_tot_safety = Ben_disbRet_safety +  Ben_servRet_safety + Ben_other_safety,
		     disbRet_share = Ben_disbRet_safety / Ben_tot_safety) %>% 
	filter(planid %in% df_include$planid)


df_disbRates <- 
read_excel(fn, sheet = "PR_DEMOASSUMP_RATES") %>% 
	select(planName = "Entity Name",
				 planid = "Entity ID", 
				 year   = "Fiscal Year",
				 gender = "Gender_Demographic Assumption Rates - Age",
				 type   = "Member Type_Demographic Assumption Rates - Age",
				 age    = "Age_Demographic Assumption Rates - Age",
				 rate_servRet         = "Service Retirement Rate_Demographic Assumption Rates - Age",
				 rate_disbRet_duty    = "Service_Disability Retirement Rate_Demographic Assumption Rates - Age",
				 rate_disbRet_nonduty = "Ordinary_Disability Retirement Rate_Demographic Assumption Rates - Age"
				 ) %>% 
	mutate(across(c(rate_servRet, rate_disbRet_duty, rate_disbRet_nonduty), ~ na2zero(as.numeric(.x)))) %>% 
	filter(planid %in% df_include$planid,
				 year == 2019,
				 type == "Safety",
				 gender == "Male") 
				 
				 
df_disbRates %>% 
	select(planName, planid, year, age, rate_disbRet_duty) %>% 
	ggplot(aes(x = age, y = rate_disbRet_duty, color = as.character(planName) )) + 
	geom_line()




df_disbRates %>% 
	filter(age %in% c(40, 50, 60))












