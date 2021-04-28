
#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

df_simNames <- 
  read_excel(paste0(here::here(), "/model/RunControl.xlsx"), sheet = "SimsAnalysis") %>% 
	filter(!is.na(sim_name), include)



df_results <- 
	map(df_simNames$sim_name, ~readRDS(paste0(here::here(), "/", dir_simResults , "sim_", .x, ".rds"))$results) %>% 
	bind_rows() %>% 
	left_join(df_simNames, by = "sim_name")

# results_all$sim_name %>% unique





#*******************************************************************************
##              SJ plans: combining t1 and t2 for each policy               ####
#*******************************************************************************

## data frame for aggregate results

vars_agg <- c("AL", "UAAL", "MA", "AA", "NC", "SC", "ERC", "EEC", "PR")

df_results_agg <-
  df_results %>%
  mutate(sim_name = str_replace(sim_name, ".t1|.t2", ".agg")) %>%
  group_by(sim_name, sim, year) %>%
  summarise(
  	sim_name = sim_name[1],
    AL = sum(AL),
    MA = sum(MA),
    AA = sum(AA),
    UAAL = sum(UAAL),
    NC = sum(NC),
    SC = sum(SC),
    ERC = sum(ERC),
    EEC = sum(EEC),
    PR  = sum(PR),
    B   = sum(B),
    POB_payment = sum(POB_payment),
    i.r = i.r[1],
  	plan = plan[1],
  	policy = policy[1],
  	ERCType = ERCType[1],
    .groups = "drop"
  ) %>%
  mutate(

        
         tier = "Plan"
         )


df_results <-
  bind_rows(df_results,
            df_results_agg) %>%
	mutate( 
		ERC_pension = ERC,
		ERC  = ERC_pension + POB_payment,
		
		FR_MA  = 100 * MA/AL,
		ERC_PR = 100 * ERC/PR,
		EEC_PR = 100 * EEC/PR,
		NC_PR  = 100 * NC/PR,
		SC_PR  = 100 * SC/PR,
		
		NC.ER  =  NC - EEC,
	  NC.ER_PR = 100 * (NC.ER)/PR) %>% 
  # mutate(
  #        #sim_name = str_replace(sim_name, "&", "_"),
  #        sim_name_fct = factor(sim_name, levels = df_simNames$simName )
  #        ) %>%
  arrange(plan, tier, ERCType, policy,  sim, year)


# df_results %>% filter(sim == 1, year == 2019)

# df_results_agg %>% select(sim, year,  sim_name, policy_name) %>% 
#   filter(sim == 1, year ==2018) %>% 
#   group_by(policy_name) %>% 
#   arrange(policy_name)
# df_results$sim_name

# df_results %>%
# 	filter(sim_name %in%  c("all2t_baseline", 
# 													"all2t_benCut1_lowERC", 
# 													"all2t_colaCut1_lowERC",
# 													"all2t_benCut1_colaCut1_lowERC"), 
# 				 year %in% c(2018), sim == 0) %>%
# 	select(sim_name, year, FR_MA,NC, NC.ER, SC, ERC, EEC_PR, UAAL, AL)












