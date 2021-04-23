

## Tools #####


#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

df_simNames <- 
	tribble(
		~simName,              ~label_benPolicy,
		"pf.t1_baseline",        "",
		"pf.t2_baseline",        "",
		
		"pf.t1_benCut1_lowERC",  "",
		"pf.t2_benCut1_lowERC",  "",
		
		"pf.t1_colaCut1_lowERC", "",
		"pf.t2_colaCut1_lowERC",  ""
		)


results_all <- 
	map(df_simNames$simName, ~readRDS(paste0(dir_modelResults, "sim_", .x, ".rds"))$results) %>% 
	bind_rows()

results_all$sim_name %>% unique


# sim_slc <- 
# 	map(df_simNames$simName[1], ~readRDS(paste0(dir_modelResults, "sim_", .x, ".rds"))$sim_slc) %>% 
# 	bind_rows() %>% 
# 	mutate(sim_original = sim,
# 				 sim = sim_mod,
# 				 sim_mod = NULL)



#*******************************************************************************
##                     Adjusting variable names for TCRS                    ####
#*******************************************************************************

results_all %>% names()

results_all %<>% 
	mutate(
		FR_MA = 100* MA/AL,
		FR_AA = 100* AA/AL,
		C_PR  = 100* C/PR,
		NC_PR = 100* C/PR,
		ERC_PR = 100 * ERC/PR,
    EEC_PR = 100 * EEC/PR,
		ADC_PR        = ADC/PR) %>% 
	relocate(sim, year)

results_all %<>% 
	mutate(NC.servRet_PR = 100 * NC.servRet/PR,
				 NC.disbRet_PR = 100 * NC.disbRet/PR,
				 NC.defrRet_PR = 100 * NC.defrRet/PR,
				 NC.death_PR   = 100 * NC.death/PR,
				 
				 ALa.servRet_PR = AL.active.servRet/PR,
				 ALa.disbRet_PR = AL.active.disbRet/PR,
				 ALa.defrRet_PR = AL.active.defrRet/PR,
				 ALa.death_PR   = AL.active.death/PR
				 
	)




# Display1 Basic examination
var_display1 <- c("sim_name", "val_name", "sim", "year", 
									"AL", "FR_MA",  "UAAL", "ERC", "ERC_PR","EEC_PR", "NC",
									"MA",
									"AL", 
									"AL.active", 
									"AL.active.servRet",
									"AL.active.disbRet",
									"AL.nonactive",
									"AL.defrRet",
									"AL.servRet",
									
									"NC.servRet_PR",
									"NC.disbRet_PR",
									"NC.defrRet_PR",
									"NC.death_PR",
									
									"ALa.servRet_PR",
									"ALa.disbRet_PR",
									"ALa.defrRet_PR",
									"ALa.death_PR", 
									
									
									"PVFB",
									"PVFB.active.disbRet",
									"PVFB.active.servRet",
									"cola_actual",
									"B",
									"NC_PR",
									"ERC_PR",
									"EEC_PR",
									# "ADC", 
									"NC", "ERC", "EEC", "SC", "LG", "i.r", "PR",
									"n_actives"
)


var_display2 <- c("sim_name", "val_name", "sim", "year", 
									"FR_MA",  # "UAAL", "ERC", "ERC_PR","EEC_PR", "NC",
									#"MA",
									
									"NC.servRet_PR",
									"NC.disbRet_PR",
									"NC.defrRet_PR",
									"NC.death_PR",
									
									"ALa.servRet_PR",
									"ALa.disbRet_PR",
									"ALa.defrRet_PR",
									"ALa.death_PR", 
									
									"PVFB.active",
									"PVFB.nonactive",
									#"PVFB.active.servRet",
									"n_actives",
									"cola_actual"
)


#results_all %>% filter(sim == 0)  %>% select(one_of(var_display2))  %>% print



results_all %>% filter(sim == 0, year == 2019, str_detect(sim_name, "\\.t1"))  %>% select(one_of(var_display2))  %>% print

results_all %>% filter(sim == 0, year == 2019, str_detect(sim_name, "\\.t2"))  %>% select(one_of(var_display2))  %>% print














