# This script calculates individual liabilities and normal costs for SJ plans


## Notes for calibration
#   Mark for calibration: #calibration


## Road map

#  0. Preparation

# Service retirement
#  1.1 AL and NC of life annuity and contingent annuity for actives
#  1.2 AL and benefit for retirees with life annuity

# Deferred retirement
#  2.1 AL and NC of deferred benefits for actives
#  2.2 AL and benefits for vested terminated members

# Disability benefit
#  3.1 AL and NC of disability benefit
#  3.2 AL and benefits for disability benefit

# Death benefit
#  4.1 AL and NC of benefit for death before retirement
#  4.2 AL and benefits for vested terminated members

#  5. Selecting variables for the chosen actuarial method

{
##' What's new in ver(3) 
#'   - Setting all Bx.xxx in one section
#'   

	
##' What's new in ver(4) 
#'   - allow for cutting benefit factor for future services

	
##' What's new in ver(5) 
#'   - better weights for P&F tier 1 benefit factors
	
##' What's new in ver(6) 
#'   - benefit factor for P&F pre1999 policy scenario
#'   

##' What's new in ver(7) 
#'   - benefit rule for federated plan tier 1
	
	
}

get_indivLiab <- function(tierData,
                         val_paramlist_    =  val_paramlist,
                         Global_paramlist_ =  Global_paramlist){

# Inputs for development

# dev -- 
	
# tierData <- ls_tierData[[1]]
# val_paramlist_    =  val_paramlist
# Global_paramlist_ =  Global_paramlist

# dev --  
	
assign_parmsList(Global_paramlist_, envir = environment())
assign_parmsList(val_paramlist_,    envir = environment()) 

assign_parmsList(tierData$tier_params,    envir = environment()) 

# TEMP
# bfactor  <- 0.02
# EEC_rate <- 0.07
# i        <- 0.07 
v <- 1/(1+i)

share_fire   <- 541/1215 # 44.5%
share_police <- 1 - share_fire  


#*******************************************************************************
#                               0. Preparation                             #####                  
#*******************************************************************************
cat("Tier:", tierData$tier_name, "\n")
cat("Preparation")



### Set up a data frame for active members, with all combos of start_year-ea-age needed.

## Specify the earliest year needed for actives
#  Ealiest year required for actives：the year a (max_retAge - 1) year old active in year 1 entered the workforce at age min_ea 

min_year_actives <- init_year - ((max_retAge - 1) -  min_ea)
min_year         <- min_year_actives


## Start_year for actives specified above needs to be supplemented by combinations 
#  needed by all types of beneficiaries. 
#   - Service retirees: combinations can be obtained from    tierData$df_n_servRet
#   - Disability retirees: combinations can be obtained from tierData$df_n_disbRet

start_year_supl <- 
	(tierData$df_n_servRet %>% filter(benefit_servRet != 0, start_year < min_year))$start_year
	# union(
	# 	(tierData$df_n_servRet %>% filter(benefit_servRet != 0, start_year < min_year))$start_year,
	#   (tierData$df_n_disbRet %>% filter(benefit_disbRet != 0, start_year < min_year))$start_year
	# 	)

## Set up the data frame
liab_active <- 
  bind_rows(
  # for actives
  expand_grid(start_year = min_year:(init_year + nyear - 1) , 
              ea         = range_ea, 
              age        = range_age), 
  # for service retirees
  expand_grid(start_year = start_year_supl,
  						ea  = unique(tierData$df_n_servRet$ea), # Should be just one value (assumed ea for all types of initial retirees)
  						age = range_age 
  						)
  )



### Clean up the data frame and incorporate data needed
liab_active %<>%
  filter(start_year + max_age - ea >= init_year, # drop redundant combinations of start_year and ea. (delete those who never reach year 1.) 
                                                 # max_age can replaced by max_retAge - 1 if only consider actives
         age >= ea) %>%   
  mutate(year = start_year + age - ea,
         ) %>%            
  arrange(start_year, ea, age) %>% 
  left_join(tierData$salary_full, by = c("start_year", "ea", "age", "year")) %>%         # adding sx (annual salaries)
  left_join(select(tierData$decrements_expanded, -grp), by = c("year", "ea", "age")) %>% # adding decrements
	mutate(grp = tierData$tier_name,
	       yos = age - ea) %>% 
  colwise(na2zero)(.) %>% 
  relocate(grp, year, start_year, ea, age, yos)
 

## Create variables needed to calculate liabilities 
liab_active %<>% 
	group_by(start_year, ea) %>% 
   # Calculate salary and benefits
  mutate(
    
  	# #calibration: Calibration for salary history of initial active members
  	# sx = ifelse(start_year <= init_year & year < init_year, 1.1*sx, sx), # sx * (1 + 0.01)^(init_year - year)
  	# sx = ifelse(start_year <= init_year & year < init_year, sx * (1 + 0.012)^(init_year - year), sx), 
  
    # ## Calculate Final Average Salary
    # Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
    # n  = pmin(yos, fasyears),                                      # years used to compute fas
    # fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    # fas= ifelse(age == min(age), 0, fas),
    # 
    ## COLA (for compound cola, will not be used for benefits with flexible COLA)
    COLA.scale_compound = (1 + cola_assumed)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    
    ## Accrued service retirement benefits with constant benefit factor 
    # Bx = na2zero(bfactor * yos * fas), # simple formula for accrued benefits,
    #                                    # note that only Bx for ages above min retirement age are necessary under EAN.

    
   
    ## Bx with benefit Not constant over time
    # Base benefit factor, may be subject to further adjustments, such as early retirement 
    
    ## Annual benefit accrual rate
    # bfactor_vec = case_when(
    # 	                yos <  20 ~ 0.025,
    # 	                yos == 20 ~ 0.075,
    # 	                yos >  20 ~ 0.035,
    # 	                TRUE ~ 0),
    # 
    # bfactor_vec = ifelse(yos  == 0, 0, bfactor_vec),
    # 
    # ## CA rule reduction
    # # bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
    # # or
    # # bfactor_vec = ifelse(year >= year_reduction, bfactor * (1 - bfactor_reduction), bfactor),
    # 
    # ## calibration
    # # bfactor_vec = (1 + cali_bfactor) * bfactor_vec,
    # 
    # ## Accrued benefit
    # Bx = na2zero(cumsum(bfactor_vec) * fas),
    
    
    # Calibration of Bx
  	# Bx = ifelse(start_year <= init_year, Bx * 1.03, Bx), 
  	
    # bx = lead(Bx) - Bx,                              # benefit accrual at age x
    
    ## Vesting
    # v.year   = v.year,    # yos needed for vesting
    # age_vben = age_vben,  # assumed age for vested members to start receiving benefits
    
    
    ## calculate survival rates needed in valuations
    
    qxm_active  = qxm.pre,   # TODO: change variable name for qxmd.post; MEPERS: qxm.pre = 0 for age >= 70
    qxm_disbRet = qxmd.post, # TODO: change variable name for qxmd.post; MEPERS: 20-100
    qxm_servRet = qxm.post,  # TODO: change variable name for qxm.post;  MEPERS: 40-100
    # qxm_defrRet = qxm.post,  # TODO: use qxm for actives for smaller ages; 
    qxm_defrRet = qxm.defrRet, 
    
    
    pxm_active  = 1 - qxm_active,   # TODO: change variable name for qxmd.post; MEPERS: qxm.pre = 0 for age >= 70
    pxm_disbRet = 1 - qxm_disbRet,  # TODO: change variable name for qxmd.post
    pxm_servRet = 1 - qxm_servRet,  # TODO: change variable name for qxm.post
    pxm_defrRet = 1 - qxm_defrRet,  # TODO: use qxm for actives for smaller ages. 
    
    pxT         = 1 - qxr - qxt - qxd - qxm_active, # probability of survival for active members
    
    
    ## Probabilities (at current age x) of surviving up to age_vben (the age vested terminated members can start claiming benefits)
    # px_r.vben_m = order_by(-age, cumprod(ifelse(age >= age_vben, 1, pxm_defrRet)))  
    pxm_age_vben = order_by(-age, cumprod(ifelse(age >= age_vben, 1, pxm_defrRet))),  
    
    ## ax.XXX: actuarial present value of future benefit, for $1's benefit in the initial year. 
    #  using compound COLA and assumed COLA rate. 
    #  Since retirees die at max_age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max_age. 
    
    # These values will be used to calculate PVFB
    
    # Note: these ax.X variables won't be needed after flexible benefits are modeled.
    #       but these values can be useful for modeling risk-sharing
    #       TODO: will need to think carefully about how to streamline this.
    
    
    ax.servRet  = get_tla(pxm_servRet, i, COLA.scale_compound),  # service retirement benefit (will be replaced when contingent retirement beneift is added) 
    ax.defrRet  = get_tla(pxm_defrRet, i, COLA.scale_compound),  # deferred retirement benefit (actually we only need the value at age_vben?)
    ax.disbRet  = get_tla(pxm_disbRet, i, COLA.scale_compound),  # disability retirement benefit
    ax.deathBen = get_tla(pxm_servRet, i, COLA.scale_compound),  # beneificiaries of death benefits
    
    
    # ax.servRet  = get_tla(pxm_servRet, i, COLA.scale),   
    # ax.r.W.ret is already in mortality.post.model_
    
    
    ## Temporary annuity values from age x to retirment age (fixed end)
    # These values will used to calculate PVFNCxs
    # TODO: figure better ways to do the valuation
    # Note: It is assumed that
    #   - the NCs for service and disability retirement benefits are spread through max_retAge - 1
    #   - the NCs for deferred retirement benefits are spread through age_vben - 1
    #   - MEPERS: If using single retirement age, then spread cost up to the single retirement age - 1 
    
     # for service and disability retirement
    axR = c(get_tla(pxT[age < max_retAge], i), rep(0, max_age - max_retAge + 1)),                          # aT..{x:max_retAge-x-|} discount value of max_retAge at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < max_retAge], i,  sx[age < max_retAge]), rep(0, max_age - max_retAge + 1)),   # ^s_aT..{x:max_retAge-x-|}
    
     
     # for deferred retirement 
    axr = ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i), rep(0, max_age - unique(age_vben) + 1))),                       # Similar to axR,but based  on age_vben.       
    axrs= ifelse(ea >= age_vben, 0, c(get_tla(pxT[age < age_vben], i,  sx[age < age_vben]), rep(0, max_age - unique(age_vben) + 1))),  # Similar to axRs, but based on age_vben.
    
    
    ## Temporary annuity values from a fixed entry age y to x (fixed start) 
    #  PV of future salary up to age (x-1), valuated at entry age without and with salary growth
    #  These values will be sued to calculate NCs
    #  Note: 
    #   - Only two values are used: at max_retAge for service/disability retirement, and at age_vben for deferred retirement
    #   - TODO: figure out how to optimize.  
    ayx = c(get_tla2(pxT[age <= max_retAge], i), rep(0, max_age - max_retAge)),                          
    ayxs= c(get_tla2(pxT[age <= max_retAge], i,  sx[age <= max_retAge]), rep(0, max_age - max_retAge)),  
    
    
    ## Employee contribution. EEC rate is variable under MEPERS
    # EEC = sx * EEC_rate 
  )
cat("......DONE\n")





#*******************************************************************************
#         0.1 Accured benefit         #####                  
#*******************************************************************************


## For SJ police and fire tier 1

## Calculating benefit payment in the first retirement year

if(tier_name %in% "pf.t1"){

	calib_Bx.servRet <- 1.05
	calib_Bx.disbRet <- 1.4
	
	## Service retirement benefit
	liab_active %<>%   
		mutate(
			
			sx = ifelse(start_year <= init_year & year < init_year, 1.1 * sx, sx), # sx * (1 + 0.01)^(init_year - year)
			
			## Calculate Final Average Salary
			Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
			n  = pmin(yos, fasyears),                                      # years used to compute fas
			fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
			fas= ifelse(age == min(age), 0, fas),
		
			# Unreduced initial benefit
			# TODO: use better weights for police and fire bfactors
			bfactor_vec = case_when(
				                yos <  20 ~ 0.025,
				                yos == 20 ~ 0.125 * share_fire + 0.025 * share_police,
				                yos >  20 ~ 0.03  * share_fire + 0.04  * share_police,
				                TRUE ~ 0),
			
			bfactor_vec = ifelse(yos  == 0, 0, bfactor_vec),
		)
	
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost") ){	
	liab_active %<>%   
		mutate(
			## CA rule reduction
			# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
			# or
			bfactor_vec = ifelse(year >= year_reduction, bfactor_vec * (1 - bfactor_reduction_t1), bfactor_vec),
			
			Bx = pmin(0.9 * fas, na2zero(cumsum(bfactor_vec) * fas)),
			
			# Retirement eligibility and % benefit can be claimed at retirement 
			benReduction = year_b4full * 0.07, 
			
			gx.servRet.laca = case_when(
				as.logical(elig_servRet_full)  ~ 1, 
				as.logical(elig_servRet_early) ~ 1 - benReduction,
				TRUE ~ 0),
			
			Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
			
			#calibration
			Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
		)
	}
	
	
	if(policyChg_type %in% c("pre99") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_policyChg, bfactor_newServ, bfactor_vec),
				
				bfactor_vec_cum = cumsum(na2zero(bfactor_vec)),
				
				bfactor_vec_cumMax = ifelse(start_year >= year_policyChg, 
																		ben_maxFAS, 
																		max(ben_maxFAS, min(0.9, bfactor_vec_cum[year == year_policyChg]))
																		),
				
				# Bx = pmin(0.9 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(bfactor_vec_cum, bfactor_vec_cumMax) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.07, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	

	# liab_active %>%
	# 	filter(start_year == 2021, ea == 20) %>%
	# 	select(start_year, ea, age, year, bfactor_vec, bfactor_vec_cum, bfactor_vec_cumMax, Bx, fas)
	# 

	
	
	# Disability retirement
	liab_active %<>%
		mutate(
			
			bfactor_disbRet_vec = case_when(
				yos == 1      ~ 0.50,
				yos %in% 2:19 ~ 0,
				yos == 20     ~ 0.1  * share_fire + 0    * share_fire,
				yos >  20     ~ 0.03 * share_fire + 0.04 * share_police,
				TRUE ~ 0),
		)
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost")){
		
		liab_active %<>%
		mutate(
			# For now benefit change does not affect the 50% FAS benefit minimum for disability 
			bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, bfactor_disbRet_vec * (1 - bfactor_reduction_t1), bfactor_disbRet_vec),
		
			
		  Bx.disbRet = pmin(na2zero(cumsum(bfactor_disbRet_vec) * fas), 0.9 * fas),

			#calibration 
			Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
		)
	}
	
	if(policyChg_type %in% c("pre99") ){
	
		liab_active %<>%
		mutate(
			# For now benefit change does not affect the 50% FAS benefit minimum for disability 
			# bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, bfactor_disbRet_vec * (1 - bfactor_reduction_t1), bfactor_disbRet_vec),
			
			bfactor_disbRet_vec = ifelse(year >= year_policyChg & yos >= 20, bfactor_newServ, bfactor_disbRet_vec),
			
			bfactor_disbRet_vec_cum = na2zero(cumsum(bfactor_disbRet_vec)),
			
			bfactor_disbRet_vec_cumMax = ifelse(start_year >= year_policyChg, 
																	        ben_maxFAS, 
																	        max(ben_maxFAS, min(0.9, bfactor_disbRet_vec_cum[year == year_policyChg]))
			),
			
			Bx.disbRet = pmin(bfactor_disbRet_vec_cum, bfactor_disbRet_vec_cumMax) * fas,
			#Bx.disbRet = pmin(na2zero(cumsum(bfactor_disbRet_vec) * fas), 0.9 * fas),
			
			
			#calibration 
			Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
		)
		}
	
	
	
	# Deferred retirement benefit (terminated members)
	liab_active %<>%
		mutate(
			# gx.defrRet = yos >= v.year,   # actives become vested after reaching v.yos years of yos
			
			# Bx is already reduced by policy change
			# TODO: calibration needed for deferred benefit
			Bx.defrRet = ifelse(yos < v.year, 0.5 * Bx, Bx),
			
			Bx.defrRet = ifelse(ea < age_vben, 
													Bx.defrRet,   
													0)             
		)
}


if(tier_name %in% "pf.t2"){
	
	calib_Bx.servRet <- 1
	calib_Bx.disbRet <- 1
	
	# Service retirement benefit
	liab_active %<>%   
		mutate(
			
			#calibration of salary
			sx = ifelse(start_year <= init_year & year < init_year, 1 * sx, sx), # sx * (1 + 0.01)^(init_year - year)
			
			## Calculate Final Average Salary
			Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
			n  = pmin(yos, fasyears),                                      # years used to compute fas
			fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
			fas= ifelse(age == min(age), 0, fas),
			
			
			# Unreduced initial benefit
			bfactor_vec = case_when(
				yos <=  20     ~ 0.024,
				yos %in% 21:25 ~ 0.03,
				yos > 26       ~ 0.034,
				TRUE ~ 0),
			
			bfactor_vec = ifelse(yos  == 0, 0, bfactor_vec),
		)
		
	if(policyChg_type %in% c("general", "measureB", "sharedCost") ){	
		liab_active %<>%   
			mutate(	
			
			## CA rule reduction
			# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
			# or
			bfactor_vec = ifelse(year >= year_reduction, bfactor_vec * (1 - bfactor_reduction_t2), bfactor_vec),
			
			## calibration
			# bfactor_vec = (1 + cali_bfactor) * bfactor_vec,
			
			Bx = pmin(0.8 * fas, na2zero(cumsum(bfactor_vec) * fas)),
			
			# Retirement eligibility and % benefit can be claimed at retirement 
			benReduction = year_b4full * 0.07, 
			
			gx.servRet.laca = case_when(
				as.logical(elig_servRet_full)  ~ 1, 
				as.logical(elig_servRet_early) ~ 1 - benReduction,
				TRUE ~ 0),
			
			Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
			
			#calibration
			Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
		)
	}

	
	if(policyChg_type %in% c("pre99") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_policyChg, bfactor_newServ, bfactor_vec),
				
				bfactor_vec_cum = na2zero(cumsum(bfactor_vec)),
				
				bfactor_vec_cumMax = ifelse(start_year >= year_policyChg, 
																		ben_maxFAS, 
																		max(ben_maxFAS, min(0.9, bfactor_vec_cum[year == year_policyChg]))
																	 ),
				
				# Bx = pmin(0.9 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(bfactor_vec_cum, bfactor_vec_cumMax) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.07, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	
	
	# Disability retirement benefit
	liab_active %<>%
		mutate(
			
			# Note that
			# 1. Bx.servRet has been reduced by policy change 
			# 2. For now, policy change does not affect the 50% FAS min for disability benefit
			Bx.disbRet  = pmax(0.5 * fas, Bx.servRet.laca),
			
			#calibration 
			Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
		)
	
	
	# Deferred retirement benefit (terminated members)
	liab_active %<>%
		mutate(
			# gx.defrRet = yos >= v.year,   # actives become vested after reaching v.yos years of yos
			
			# Bx has been reduced by policy change 
			Bx.defrRet = ifelse(yos < v.year, 0.5 * Bx, Bx),
			
			Bx.defrRet = ifelse(ea < age_vben, 
													Bx.defrRet,   
													0)
				)
}



if(tier_name %in% "fc.t1"){
	
	calib_Bx.servRet <- 1
	calib_Bx.disbRet <- 1
	calib_sal <- 1
	
	## Service retirement benefit
	liab_active %<>%   
		mutate(
			
			sx = ifelse(start_year <= init_year & year < init_year, calib_sal * sx, sx), # sx * (1 + 0.01)^(init_year - year)
			
			## Calculate Final Average Salary
			Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
			n  = pmin(yos, fasyears),                                      # years used to compute fas
			fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
			fas= ifelse(age == min(age), 0, fas),
			
			# Unreduced initial benefit
			# bfactor_vec = case_when(
			# 	yos <  20 ~ 0.025,
			# 	yos == 20 ~ 0.125 * share_fire + 0.025 * share_police,
			# 	yos >  20 ~ 0.03  * share_fire + 0.04  * share_police,
			# 	TRUE ~ 0),
			
			# bfactor_vec = 0.025,
			
			bfactor_vec = ifelse(yos  == 0, 0, 0.025),
		)
	
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_reduction, bfactor_vec * (1 - bfactor_reduction_t1), bfactor_vec),
				
				bfactor_vec_cum = cumsum(na2zero(bfactor_vec)),
				
				#Bx = pmin(0.75 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(0.75, bfactor_vec_cum) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.05, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	
	
	if(policyChg_type %in% c("pre99") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_policyChg, bfactor_newServ, bfactor_vec),
				
				bfactor_vec_cum = cumsum(na2zero(bfactor_vec)),
				
				bfactor_vec_cumMax = ifelse(start_year >= year_policyChg, 
																		ben_maxFAS, 
																		max(ben_maxFAS, min(0.75, bfactor_vec_cum[year == year_policyChg]))
				),
				
				# Bx = pmin(0.9 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(bfactor_vec_cum, bfactor_vec_cumMax) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.07, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	
	
	# liab_active %>%
	# 	filter(start_year == 2021, ea == 20) %>%
	# 	select(start_year, ea, age, year, bfactor_vec, bfactor_vec_cum, bfactor_vec_cumMax, Bx, fas)
	# 
	
	
	
	## Disability retirement
	liab_active %<>%
		mutate(
			bfactor_disbRet_vec = 0.025
			# bfactor_disbRet_vec = case_when(
			# 	yos == 1      ~ 0.50,
			# 	yos %in% 2:19 ~ 0,
			# 	yos == 20     ~ 0.1  * share_fire + 0    * share_fire,
			# 	yos >  20     ~ 0.03 * share_fire + 0.04 * share_police,
			# 	TRUE ~ 0),
		)
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost")){
		
		liab_active %<>%
			mutate(
				# For now benefit change does not affect the 50% FAS benefit minimum for disability 
				# bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, 
				# 														 bfactor_disbRet_vec * (1 - bfactor_reduction_t1), 
				# 														 bfactor_disbRet_vec),
				
				# TODO: to modify to include non-service connected disability
				
				bfactor_disbRet_vec = ifelse(year >= year_reduction, 
																		 bfactor_disbRet_vec * (1 - bfactor_reduction_t1), 
																		 bfactor_disbRet_vec),
				
				bfactor_disbRet_vec_cum = cumsum(na2zero(bfactor_disbRet_vec)),
				

			  # Max 75%, min 40%
				Bx.disbRet = pmin(bfactor_disbRet_vec_cum, 0.75) * fas,
				Bx.disbRet = pmax(Bx.disbRet, 0.4 * fas),
				
				#calibration 
				Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
			)
	}
	
	if(policyChg_type %in% c("pre99") ){
		
		liab_active %<>%
			mutate(
				# For now benefit change does not affect the 50% FAS benefit minimum for disability 
				# bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, bfactor_disbRet_vec * (1 - bfactor_reduction_t1), bfactor_disbRet_vec),
				
				bfactor_disbRet_vec = ifelse(year >= year_policyChg & yos >= 20, bfactor_newServ, bfactor_disbRet_vec),
				
				bfactor_disbRet_vec_cum = na2zero(cumsum(bfactor_disbRet_vec)),
				
				bfactor_disbRet_vec_cumMax = ifelse(start_year >= year_policyChg, 
																						ben_maxFAS, 
																						max(ben_maxFAS, min(0.9, bfactor_disbRet_vec_cum[year == year_policyChg]))
				),
				
				Bx.disbRet = pmin(bfactor_disbRet_vec_cum, bfactor_disbRet_vec_cumMax) * fas,
				#Bx.disbRet = pmin(na2zero(cumsum(bfactor_disbRet_vec) * fas), 0.9 * fas),
				
				
				#calibration 
				Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
			)
	}
	
	
	
	## Deferred retirement benefit (terminated members)
	liab_active %<>%
		mutate(
			# gx.defrRet = yos >= v.year,   # actives become vested after reaching v.yos years of yos
			
			# Bx is already reduced by policy change
			# TODO: calibration needed for deferred benefit
			Bx.defrRet = ifelse(yos < v.year, 0.5 * Bx, Bx),
			
			Bx.defrRet = ifelse(ea < age_vben, 
													Bx.defrRet,   
													0)             
		)
}


if(tier_name %in% "fc.t2"){
	
	calib_Bx.servRet <- 1
	calib_Bx.disbRet <- 1
	calib_sal <- 1
	
	## Service retirement benefit
	liab_active %<>%   
		mutate(
			
			sx = ifelse(start_year <= init_year & year < init_year, calib_sal * sx, sx), # sx * (1 + 0.01)^(init_year - year)
			
			## Calculate Final Average Salary
			Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),              # Cumulative salary
			n  = pmin(yos, fasyears),                                      # years used to compute fas
			fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
			fas= ifelse(age == min(age), 0, fas),
			
			# Unreduced initial benefit
			# bfactor_vec = case_when(
			# 	yos <  20 ~ 0.025,
			# 	yos == 20 ~ 0.125 * share_fire + 0.025 * share_police,
			# 	yos >  20 ~ 0.03  * share_fire + 0.04  * share_police,
			# 	TRUE ~ 0),
			
			# bfactor_vec = 0.025,
			
			bfactor_vec = ifelse(yos  == 0, 0, 0.02),
		)
	
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_reduction, bfactor_vec * (1 - bfactor_reduction_t1), bfactor_vec),
				
				bfactor_vec_cum = cumsum(na2zero(bfactor_vec)),
				
				#Bx = pmin(0.75 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(0.7, bfactor_vec_cum) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.05, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	
	
	if(policyChg_type %in% c("pre99") ){	
		liab_active %<>%   
			mutate(
				## CA rule reduction
				# bfactor_vec = ifelse(year >= year_reduction, bfactor - bfactor_reduction, bfactor),
				# or
				bfactor_vec = ifelse(year >= year_policyChg, bfactor_newServ, bfactor_vec),
				
				bfactor_vec_cum = cumsum(na2zero(bfactor_vec)),
				
				bfactor_vec_cumMax = ifelse(start_year >= year_policyChg, 
																		ben_maxFAS, 
																		max(ben_maxFAS, min(0.7, bfactor_vec_cum[year == year_policyChg]))
				),
				
				# Bx = pmin(0.9 * fas, na2zero(cumsum(bfactor_vec) * fas)),
				Bx = pmin(bfactor_vec_cum, bfactor_vec_cumMax) * fas,
				
				# Retirement eligibility and % benefit can be claimed at retirement 
				benReduction = year_b4full * 0.05, 
				
				gx.servRet.laca = case_when(
					as.logical(elig_servRet_full)  ~ 1, 
					as.logical(elig_servRet_early) ~ 1 - benReduction,
					TRUE ~ 0),
				
				Bx.servRet.laca  = gx.servRet.laca * 	Bx, # benefit in the first retirement year if retirement age = x
				
				#calibration
				Bx.servRet.laca =  calib_Bx.servRet * Bx.servRet.laca
			)
	}
	
	
	# liab_active %>%
	# 	filter(start_year == 2021, ea == 20) %>%
	# 	select(start_year, ea, age, year, bfactor_vec, bfactor_vec_cum, bfactor_vec_cumMax, Bx, fas)
	# 
	
	
	
	## Disability retirement
	liab_active %<>%
		mutate(
			bfactor_disbRet_vec = 0.02
			# bfactor_disbRet_vec = case_when(
			# 	yos == 1      ~ 0.50,
			# 	yos %in% 2:19 ~ 0,
			# 	yos == 20     ~ 0.1  * share_fire + 0    * share_fire,
			# 	yos >  20     ~ 0.03 * share_fire + 0.04 * share_police,
			# 	TRUE ~ 0),
		)
	
	if(policyChg_type %in% c("general", "measureB", "sharedCost")){
		
		liab_active %<>%
			mutate(
				# For now benefit change does not affect the 50% FAS benefit minimum for disability 
				# bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, 
				# 														 bfactor_disbRet_vec * (1 - bfactor_reduction_t1), 
				# 														 bfactor_disbRet_vec),
				
				# TODO: to modify to include non-service connected disability
				
				bfactor_disbRet_vec = ifelse(year >= year_reduction, 
																		 bfactor_disbRet_vec * (1 - bfactor_reduction_t1), 
																		 bfactor_disbRet_vec),
				
				bfactor_disbRet_vec_cum = cumsum(na2zero(bfactor_disbRet_vec)),
				
				
				# Max 75%, min 40%
				Bx.disbRet = pmin(bfactor_disbRet_vec_cum, 0.7) * fas,
				Bx.disbRet = pmax(Bx.disbRet, 0.4 * fas),
				
				#calibration 
				Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
			)
	}
	
	if(policyChg_type %in% c("pre99") ){
		
		liab_active %<>%
			mutate(
				# For now benefit change does not affect the 50% FAS benefit minimum for disability 
				# bfactor_disbRet_vec = ifelse(year >= year_reduction & yos != 1, bfactor_disbRet_vec * (1 - bfactor_reduction_t1), bfactor_disbRet_vec),
				
				bfactor_disbRet_vec = ifelse(year >= year_policyChg & yos >= 20, bfactor_newServ, bfactor_disbRet_vec),
				
				bfactor_disbRet_vec_cum = na2zero(cumsum(bfactor_disbRet_vec)),
				
				bfactor_disbRet_vec_cumMax = ifelse(start_year >= year_policyChg, 
																						ben_maxFAS, 
																						max(ben_maxFAS, min(0.9, bfactor_disbRet_vec_cum[year == year_policyChg]))
				),
				
				Bx.disbRet = pmin(bfactor_disbRet_vec_cum, bfactor_disbRet_vec_cumMax) * fas,
				#Bx.disbRet = pmin(na2zero(cumsum(bfactor_disbRet_vec) * fas), 0.9 * fas),
				
				
				#calibration 
				Bx.disbRet = calib_Bx.disbRet * Bx.disbRet
			)
	}
	
	
	
	## Deferred retirement benefit (terminated members)
	liab_active %<>%
		mutate(
			# gx.defrRet = yos >= v.year,   # actives become vested after reaching v.yos years of yos
			
			# Bx is already reduced by policy change
			# TODO: calibration needed for deferred benefit
			Bx.defrRet = ifelse(yos < v.year, 0.5 * Bx, Bx),
			
			Bx.defrRet = ifelse(ea < age_vben, 
													Bx.defrRet,   
													0),
			
			# Applie early retirement reduction for age 55
			Bx.defrRet = Bx.defrRet * (1- 0.05*(62-55))
			
		)
}






#*******************************************************************************
#          1.1 Service Retirement: AL and benefits for retirees            #####                  
#*******************************************************************************
cat("Service Retirement - retirees")



# select(liab_active, grp, year,  start_year, ea, age, Bx, sx, yos, fas, benReduction, gx.servRet.laca,  Bx.servRet.laca, pxm_servRet) %>%
#    filter(start_year == 1995, ea == 30)




if(nrow(filter(tierData$df_n_servRet, benefit_servRet != 0)) == 0) {
	liab_la_init <- NULL
	} else {

liab_servRet.la_init <- 
	expand.grid(
		# Set up grids for retirees
		# 4 dimensions:
		#  - age_servRet
		#  - age
		#  - ea,
		#  - start_year
		age_servRet= filter(tierData$df_n_servRet, benefit_servRet != 0)$age, # This ensures that year of retirement is year 1.
		age        = range_age[range_age >= min(filter(tierData$df_n_servRet, benefit_servRet != 0)$age)]) %>%
	
	mutate(ea         = min_age,  # by global model assumption
				 start_year = init_year - (age_servRet - ea)) %>% 
	filter(# age >= ea, 
		     age >= age_servRet) %>% 
	
	left_join(tierData$df_n_servRet %>% 
	            select(start_year, ea, age, age_servRet, benefit_servRet), by = c("ea", "age", "start_year", "age_servRet")) %>% 
	left_join(liab_active %>% 
	            select(start_year, ea, age, pxm_servRet, ax.servRet), by = c("age", "ea", "start_year")) %>% 
	
	group_by(start_year, age_servRet, ea) %>% 
	mutate(year_servRet = start_year + age_servRet - ea, 
				 year = start_year + age - ea,
				 COLA.scale_compound = (1 + cola_assumed)^(age - min(age)), # may not be used under flexible COLA
				 
				 # Annual benefit payments
				 # more complex benefit streams can be specified
				 # IMPROVEMENT: can be combined with future members
				 B.servRet.la    = benefit_servRet[age == age_servRet] * COLA.scale_compound / COLA.scale_compound[age == age_servRet],

				 #calibration
				 # B.la   = B.la  * (1 + calib_g)^(year - init_year),
				 
				 # liability
				 ALx.servRet.la  = get_tla_cashflow(pxm_servRet, i, B.servRet.la)
				 )
	}

# liab_servRet.la_init %>% filter(start_year == 1988)


## Benefit payments and ALs for current and future active members
liab_servRet.la <- 
	# grids for who retire after year 1.
	expand_grid(ea           = range_ea[range_ea < max_retAge], # Just be safe. Note that this would drop all cells out of this ea range. 
							age_servRet  = min_retAge:max_retAge,
							start_year = (init_year + 1 - (max_retAge - min_ea)):(init_year + nyear - 1), # should be the same as min_year, min start_year: entry year for those who have entry age of min_ea and retire in year 2 at max_retAge. 
							age        = range_age[range_age >=min_retAge]) %>%
		filter(age         >= ea,
					 age_servRet >= ea,
					 age         >= age_servRet) %>% 
					 #start_year + (age_servRet - ea) >= init_year,     # retire after year 2, LHS is the year of retirement
					 #start_year + age - ea           >= init_year) %>% # not really necessary since we already have age >= age.r
	data.table(key = "start_year,ea,age_servRet,age")
#liab_la <- liab_la[!duplicated(liab_la %>% select(start_year, ea, age, age_servRet ))]  # should have no duplication, just for safety


# Merging data from liab_active
liab_servRet.la <- 
  merge(liab_servRet.la,
				select(liab_active, start_year, ea, age, Bx.servRet.laca, pxm_servRet, ax.servRet) %>% data.table(key = "ea,age,start_year"),
				all.x = TRUE, 
				by = c("ea", "age","start_year")) %>%
	# arrange(start_year, ea, age_servRet) %>% 
	mutate(year   = start_year + age - ea) %>% 
	as.data.frame



# Calculate benefit and AL
liab_servRet.la %<>% 
	group_by(start_year, age_servRet, ea) %>% 
	mutate(year_servRet = start_year + age_servRet - ea, 
				 COLA.scale_compound = (1 + cola_assumed)^(age - min(age)), # may not be used under flexible COLA
		     
				 # Annual benefit payments
				 B.servRet.la   = Bx.servRet.laca,
				 B.servRet.la   = B.servRet.la[age == age_servRet] * COLA.scale_compound / COLA.scale_compound[age == age_servRet],
		

				 ALx.servRet.la  = get_tla_cashflow(pxm_servRet, i, B.servRet.la)
				 )



# Save liability (PVFB) upon retirement for later calculations (actives)
df_AL.servRet.la_init <-
liab_servRet.la %>% 
	ungroup() %>% 
	mutate(year = start_year + age - ea) %>% 
	filter(age == age_servRet) %>% 
	select(start_year, ea, age, ALx.servRet.la) %>% 
	arrange(ea, age)





# Combining results
liab_servRet.la <- 
   bind_rows(
      liab_servRet.la_init,
      liab_servRet.la %>% 
        filter(start_year + (age_servRet - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
   						 start_year + age - ea           >= init_year + 1) # years after year 2
   ) %>% 
  filter(year %in% seq(init_year, len = nyear))

cat("......DONE\n")




#*******************************************************************************
#      1.2  Service Retirement: ALs and NCs for actives #####                  
#*******************************************************************************
cat("Service Retirement - actives")

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab_active %<>%   
	left_join(df_AL.servRet.la_init, by = c("start_year", "ea", "age")) %>% 
  mutate(
  	TCx.servRet.laca   = qxr.la * lead(ALx.servRet.la) * v, # term cost for current age x (age_sevRet  = x + 1 )
 
    # PV of future benefits, PV of future salary, and PV of employee contributions, all up ot max_retAge - 1
    PVFBx.servRet.laca  = c(get_PVFB(pxT[age <= max_retAge], v, TCx.servRet.laca[age <= max_retAge]), rep(0, max_age - max_retAge)),
    PVFSx               = c(get_PVFB(pxT[age <= max_retAge], v, sx[age <= max_retAge]),       rep(0, max_age - max_retAge)),
  	# PVFEEC              = c(get_PVFB(pxT[age <= max_retAge], v, EEC[age <= max_retAge]),      rep(0, max_age - max_retAge)),
 
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

  # NC and AL of EAN.CD
  NCx.EAN.CD.servRet.laca = ifelse(age < max_retAge, PVFBx.servRet.laca[age == min(age)]/ayx[age == max_retAge], 0),
  PVFNCx.EAN.CD.servRet.laca = NCx.EAN.CD.servRet.laca * axR,
  ALx.EAN.CD.servRet.laca = PVFBx.servRet.laca - PVFNCx.EAN.CD.servRet.laca,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.servRet.laca   = ifelse(age < max_retAge, sx * PVFBx.servRet.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
  PVFNCx.EAN.CP.servRet.laca = NCx.EAN.CP.servRet.laca * axRs,
  ALx.EAN.CP.servRet.laca   = PVFBx.servRet.laca - PVFNCx.EAN.CP.servRet.laca
  ) 

cat("......DONE\n")

# liab_active$ALx.EAN.CP.servRet.laca #%>% sum(na.rm = T)



#*******************************************************************************
#    2.1 Deferred Retirement: AL and NC for actives   #####
#*******************************************************************************
cat("Deferred Retirement - actives")

# Calculate normal costs and liabilities of deferred retirement benefits
  # Vested terms begin to receive deferred retirement benefit at age_vben, 
  # After they start receiving benefits, our understanding is that they are considered as retirees in the AV.
  # In the model, however, they always stay in the category of vested terms. 

# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age age_vben - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNCx(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: When PVFB is only amortized up to age min_retAge, there will be a problem if actives entering after min_retAge can get vested, 

# CalPERS
  # - Note only only qxt.vest should be used in the calculation of term costs
  # - It is assumed that NCs for deferred retirement benefits are spread up to age_vben, 
  #   then only salary up to age_vben are used in the calculation of NC rate. 

liab_active %<>%
  mutate(
  	
  # 	     gx.defrRet = yos >= v.year,   # actives become vested after reaching v.yos years of yos
  #        
  # 			 Bx.defrRet = ifelse(ea < age_vben, 
  # 			                     gx.defrRet * Bx,   
  #  			 							       0),              # initial annuity amount when the vested term retires at age age_vben, when a employee is vested at a certain age. 
  # 			                                      # May be unnecessary since we have set qxt = 0 for age>= age_vben. Left for safety. 
  # 			 
  # 			 Bx.defrRet = ifelse(yos < 10, 
  # 			 										 0.5 * Bx.defrRet,   
  # 			 										 Bx.defrRet), 
  # 			 
  # 			 
  			 
  			 # TODO: should think about what benefit factor to use. 
  			 #  - currently use the standard benefit factor, which is incorporated in Bx
  			 #  - May need to use the factor at age_vben if we use age-specific factors
  			 
  			 # Bx.defrRet = Bx.defrRet * 0.644,  #calibration
  			 
  			 # Term cost of vested termination benefits. We assume term rates are 0 after age_vben.
  			 TCx.defrRet   = ifelse(ea < age_vben, qxt * lead(pxm_age_vben) * v^(age_vben - age) * (lead(Bx.defrRet) * ax.defrRet[age == age_vben]), 0), 
         
  			 # PVFB of vested deferred retirement benefits
         PVFBx.defrRet = ifelse(ea < age_vben, c(get_PVFB(pxT[age < age_vben], v, TCx.defrRet[age < age_vben]), rep(0, max_age - unique(age_vben) + 1)), 0),  # To be compatible with the cases where workers enter after age age_vben, max_retAge is used instead of min_retAge, which is used in textbook formula(winklevoss p115).
         
         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         
         # NC and AL of EAN.CD
         NCx.EAN.CD.defrRet   = ifelse(age < age_vben, PVFBx.defrRet[age == min(age)]/ayx[age == age_vben], 0),
  			 PVFNCx.EAN.CD.defrRet = NCx.EAN.CD.defrRet * axr,
         ALx.EAN.CD.defrRet   = PVFBx.defrRet - PVFNCx.EAN.CD.defrRet,
         
         # NC and AL of EAN.CP
         NCx.EAN.CP.defrRet   = ifelse(age < age_vben, PVFBx.defrRet[age == min(age)]/(sx[age == min(age)] * ayxs[age == age_vben]) * sx, 0), 
         PVFNCx.EAN.CP.defrRet = NCx.EAN.CP.defrRet * axrs,
         ALx.EAN.CP.defrRet   = PVFBx.defrRet - PVFNCx.EAN.CP.defrRet
  ) 
  
# x <- liab_active %>% filter(start_year == 2017, ea == 20) %>% select(start_year, ea, age, gx.v, Bx.v, TCx.v, PVFBx.v,  NCx.EAN.CP.v, ALx.EAN.CD.v, px_r.vben_m, ax.terms, qxd)
# x$qxd

cat("......DONE\n")



#*******************************************************************************
#        2.2 Deferred Retirement:  AL for vested terminated members      #####
#*******************************************************************************
cat("Deferred Retirement - retirees")

## Calculate AL and benefit payment for initial vested terms.
#  This part will not be needed if an simplified approach is taken for initial vested members.


# init_terminated_ %<>%  
#   mutate(year = init.year,
#          age.term = age - 1)         # assume all terms are terminated in init.year - 1.
#          #yos = age - ea,
#          #start.year = year - (age - ea))
# 
# # init_terminated_
# 
# 
# # liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
# #                               age.term   = unique(init_terminated_$age.term),
# #                               start.year = unique(init_terminated_$start.year),
# #                               age = range_age) %>%
# 
# liab.term.init <- expand.grid(age.term = unique(init_terminated_$age.term),
#                               age = range_age) %>% 
#   mutate(ea   = min(init_terminated_$ea),
#          year = init.year + (age - age.term - 1),
#          start.year = year - (age - ea)
#          ) %>% 
#   filter(start.year + age - ea >= 1,
#          age >= ea,
#          age.term >= ea) %>%
#   left_join(init_terminated_ %>% select(age.term, age, benefit.term = benefit)) %>%
#   left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m, ax.vben)) %>%
#   left_join(decrement.model_ %>% select(start.year, ea, age, pxm.term)) %>% 
#   group_by(start.year, ea, age.term) %>%
# 
#   mutate(
#     year = start.year + age - ea,
#     age.ben =  ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 
#     year.term = year[age == age.term],
# 
#     COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
#     ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     
#     
#     Bx.v  = benefit.term,
#     
#     B.v   = ifelse(age.ben > r.vben, 0,    ifelse(age >= r.vben, Bx.v[age == unique(age.term) + 1]  * COLA.scale/COLA.scale[age == r.vben],  0)), # Benefit payment after r.vben, for age.ben == r.vben
#     B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term) + 1] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
#     ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term) + 1] * ax.vben[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
#                    B.v * ax.vben)
#     ) %>%                                                                                     # liab after receiving benefits      
#   ungroup %>%
#   select(ea, age, start.year, year, year.term, B.v, ALx.v, ax.vben, pxm.term) %>%
#   filter(year %in% seq(init.year, len = nyear),
#          year.term == init.year - 1)


##  Calculate AL and benefit payment for vested terms that terminates after year 1.
#   Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab_defrRet <- expand.grid(
                         start_year   = min_year:(init_year + nyear - 1),   # (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
                         ea  = range_ea[range_ea < age_vben],
                         age = range_age,
                         age_defrRet = range_age[range_age <= age_vben]) %>%
  filter(start_year + max_age - ea >= init_year,
         age >= ea, 
  			 age_defrRet >= ea,
         age >= age_defrRet) %>% # drop redundant combinations of start_year and ea.
  data.table(key = "ea,age,start_year,age_defrRet")


liab_defrRet <- 
  merge(liab_defrRet,
			  select(liab_active, start_year, year, ea, age, Bx.defrRet, pxm_age_vben,  ax.defrRet, pxm_defrRet) %>% data.table(key = "ea,age,start_year"),
        all.x = TRUE, by = c("ea", "age","start_year")) %>% 
  as.data.frame


liab_defrRet %<>%
  group_by(start_year, ea, age_defrRet) %>%
  mutate(year_defrRet = year[age == age_defrRet],

         COLA.scale_compound = (1 + cola_assumed)^(age - min(age)),   # COLA.scale in liab.active does not trace back long enough
         ax.defrRet = get_tla(pxm_defrRet, i, COLA.scale_compound),   # COLA.scale in liab.active does not trace back long enough

         B.defrRet = ifelse(age >= age_vben, 
                            Bx.defrRet[age == unique(age_defrRet)] * COLA.scale_compound / COLA.scale_compound[age == age_vben], 
                            0),  # Benefit payment after age_vben
         
         ALx.defrRet = ifelse(age <  age_vben, 
                              Bx.defrRet[age == unique(age_defrRet)] * ax.defrRet[age == age_vben] * pxm_age_vben * v^(age_vben - age),
                              B.defrRet * ax.defrRet)

  ) %>%
  ungroup  %>%
  select(ea, age, age_defrRet, start_year, year, year_defrRet, B.defrRet, ALx.defrRet, Bx.defrRet, ax.defrRet, pxm_defrRet) %>%
  filter(year %in% seq(init_year, len = nyear))

cat("......DONE\n")





#*******************************************************************************
#         3.1  Disability Retirement: ALs and NCs for actives            #####
#*******************************************************************************
cat("Disability Retirement - actives")


maxAgeNC_disbRet <- max_retAge


# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages
liab_active %<>% 
  				mutate( 
  			
  # 				Bx.disbRet  = case_when(
  #             yos < 20 ~ 0.5 * fas,
  #             yos == 20 ~ 0.55 * fas,
  #             yos > 20 ~ (0.55 + 0.035 * (yos - 20)) * fas,
  # 				    TRUE ~ 0
  # 				  ),
  # 				
  # 				
  # 				Bx.disbRet = pmin(Bx.disbRet, 0.9 * fas),
  # 				
  # 				Bx.disbRet = calib_Bx.disbRet * Bx.disbRet, 
  				 
  				# Bx.disbRet = Bx.disbRet * adj_fct.act.disbRet, #calibration
  				 
          # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x.
  				TCx.disbRet = qxd * v * lead(Bx.disbRet) *  lead(ax.disbRet), 
  				
  				
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.disbRet  = c(get_PVFB(pxT[age <= max_retAge ], v, TCx.disbRet[age <= max_retAge]), rep(0, max_age - max_retAge)),

          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),

          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

          # NC and AL of EAN.CD
          NCx.EAN.CD.disbRet = ifelse(age < max_retAge, PVFBx.disbRet[age == min(age)]/ayx[age == max_retAge], 0),
  				PVFNCx.EAN.CD.disbRet =  NCx.EAN.CD.disbRet * axR,
  				ALx.EAN.CD.disbRet = PVFBx.disbRet - PVFNCx.EAN.CD.disbRet,

          # NC and AL of EAN.CP
          NCx.EAN.CP.disbRet   = ifelse(age < maxAgeNC_disbRet, sx * PVFBx.disbRet[age == min(age)]/(sx[age == min(age)] * ayxs[age == maxAgeNC_disbRet]), 0),
  				PVFNCx.EAN.CP.disbRet = NCx.EAN.CP.disbRet * axRs,
          ALx.EAN.CP.disbRet   = PVFBx.disbRet - PVFNCx.EAN.CP.disbRet
  )

# liab_active %>% filter(start_year == 2016, ea == 20) %>% 
# 	select(start_year, ea, age, gx.disbRet, fas, Bx.disbRet)

cat("......DONE\n")

#*******************************************************************************
#     3.2 Disability Benefit: ALs and benefits for disabled retirees       #####
#*******************************************************************************

cat("Disability Retirement - retirees")
 

#if(nrow(filter(tierData$df_n_disbRet, benefit_disbRet != 0)) == 0) {
	liab_disbRet_init <- NULL
# } else {
# 
# liab_disbRet_init <- 
# 	
# 	# grids for initial disability retirees in year 1
# 	# To simplified the calculation, it is assumed that all initial disabled entered the workforce at age min.age and
# 	# become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligibility rules,
# 	# but this is not an issue since the main goal is to generate the correct cash flow and liablity for the initial disabled.
# 	
# 	expand_grid(age_disbRet = filter(tierData$df_n_disbRet, benefit_disbRet != 0)$age, # This ensures that year of retirement is year 1.
# 							age         = range_age[range_age >= min(filter(tierData$df_n_disbRet, benefit_disbRet != 0)$age)]) %>%
# 	mutate(ea            = unique(tierData$df_n_disbRet$ea),
# 				 start_year    = init_year - (age_disbRet - ea)) %>%
# 	filter(age >= age_disbRet,
# 	       age >= ea)
# }	

liab_disbRet <-
  # grids for who die after year 1.
  expand_grid(ea           = range_ea[range_ea < max_retAge],
              age_disbRet  = min_age:max_retAge,
              start_year   = (init_year + 1 - (max_retAge - min(range_ea))):(init_year + nyear - 1),
              age          = range_age) %>%
    filter(age         >= ea,
           age_disbRet >= ea,
           age         >= age_disbRet,
           start_year + (age_disbRet - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
           start_year + age - ea >= init_year + 1)           # not really necessary since we already have age >= age.r


liab_disbRet <- 
	bind_rows(
		liab_disbRet_init,
		liab_disbRet
	) %>%
  data.table(key = "start_year,ea,age_disbRet,age")
# liab_disb <- liab_disb[!duplicated(liab_disb %>% select(start_year, ea, age, age_disbRet ))]


liab_disbRet <- 
  merge(liab_disbRet,
        select(liab_active, start_year, ea, age, Bx.disbRet, ax.disbRet) %>% data.table(key = "ea,age,start_year"), # cola.scale_compound
        all.x = TRUE,
        by = c("ea", "age","start_year")) %>%
  # arrange(start_year, ea, age_disbRet) %>%
  as.data.frame %>%
  # left_join(select(tierData$df_n_disbRet, ea, age, start_year, age_disbRet, benefit_disbRet), 
  #           by = c("ea", "age", "start_year", "age_disbRet")) %>%
	mutate(benefit_disbRet = 0) %>% 
	
  mutate(year = start_year + age - ea) %>% 
  left_join(tierData$decrements_expanded %>% 
              mutate(pxm_disbRet = 1 - qxmd.post) %>% 
              select(year, ea, age, pxm_disbRet), 
            by = c("year", "ea", "age"))  # need to add start_year if we use generational table in the future


# TODO: This is slow, need to optimize functions containing loops

liab_disbRet %<>% 
  as.data.frame  %>%
  group_by(start_year, ea, age_disbRet) %>%
  mutate(
    # year = start_year + age - ea,

    COLA.scale_compound    = (1 + cola_assumed)^(age - min(age)),  # COLA.scale in liab.active does not trace back long enough, may not be necessary now
    ax.disbRet    = get_tla(pxm_disbRet, i, COLA.scale_compound),  # COLA.scale in liab.active does not trace back long enough, may not be necessary now
    year_disbRet  = start_year + age_disbRet - ea,                 # year of disability of the active

    Bx.disbRet    = ifelse(is.na(Bx.disbRet), 0, Bx.disbRet),  # just for safety
    B.disbRet     = ifelse(year_disbRet <= init_year,
                           benefit_disbRet[year == init_year] * COLA.scale_compound / COLA.scale_compound[year == init_year],  # Benefits for initial disability retirees
                           Bx.disbRet[age == age_disbRet] * COLA.scale_compound / COLA.scale_compound[age == age_disbRet]),    # Benefits for disability retirees after year 1
    ALx.disbRet   = B.disbRet * ax.disbRet                                                                   # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA

  ) %>% ungroup %>%
  filter(year %in% seq(init_year, len = nyear) ) %>%
  select(year, ea, age, year_disbRet, age_disbRet, start_year, B.disbRet, ALx.disbRet)


cat("......DONE\n")


#*******************************************************************************
#   4.1  Death benefit (before retirement): ALs, NCs for actives           #####
#*******************************************************************************

# PERF A pre-retirement death benefit:
# - Basic Death Benefit
#   - Only for active members
#   - Benefit: the sum of
#      - Member's accumulated contributions, with interest max(6%, prevailing discount rate)
# 		 - 6 months' salary if eligible for service retirement or Alternate Death Benefit

# Simplified version for now: 2-years of final salary


cat("Death Benefits - actives")
# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages
liab_active %<>%
	mutate( gx.death  = 1,
					
					# Simplifed PERF A basic death benefit policy
					Bx.death = 4 * sx * gx.death,   # pmax(0, gx.death * sx * 3 - 50000),
					
					# Bx.death = Bx.death * adj_fct.act.death, #calibration
					
					# This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x.
					# For TRS: 1. Lump sum death benefit equal to PV of future benefit (Bx.death * ax.deathBen);
					#            2. Death benefit are assumed to be claimed 1 year after death
					TCx.death = qxm_active * v * lead(Bx.death) , # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
					
					# TCx.r = Bx.r * qxr.a * ax,
					PVFBx.death  = c(get_PVFB(pxT[age <= max_retAge], v, TCx.death[age <= max_retAge]), rep(0, max_age - max_retAge)),
					
					## NC and AL of UC
					# TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
					# NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
					# ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
					
					# # NC and AL of PUC
					# TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost
					# NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
					# ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
					
					
					## Under EAN methods, costs are spread up to r.max
					# NC and AL of EAN.CD
					NCx.EAN.CD.death = ifelse(age < max_retAge, PVFBx.death[age == min(age)]/ayx[age == max_retAge], 0),
					PVFNCx.EAN.CD.death =  NCx.EAN.CD.death * axR,
					ALx.EAN.CD.death = PVFBx.death - PVFNCx.EAN.CD.death,
					
					# NC and AL of EAN.CP
					NCx.EAN.CP.death   = ifelse(age < max_retAge, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == max_retAge]), 0),
					PVFNCx.EAN.CP.death = NCx.EAN.CP.death * axRs,
					ALx.EAN.CP.death   = PVFBx.death - PVFNCx.EAN.CP.death
	)
cat("......DONE\n")

# liab_active %>% filter(start_year == 2016, ea == 30) %>% 
# 	select(start_year, year, ea, age, yos, Bx.death, Bx.death1, elig_full, Bx.laca, ax.servRet)



#*******************************************************************************
#  4.2   Death benefit (before retirement): AL and benefit for recipients  #####
#*******************************************************************************

cat("Death Benefits - beneficiaries")

liab_death <- rbind(
	# grids for who die after year 1.
	expand_grid(ea        = range_ea[range_ea < max_retAge],
							age_death = min_age:max_retAge,
							start_year   = (init_year + 1 - (max_retAge - min_ea)):(init_year + nyear - 1),
							age          = min_age:max_retAge) %>% # WARNING: ONLY good for lump sum benefit!
		filter(age   >= ea,
					 age_death >= ea,
					 age   >= age_death,
					 start_year + (age_death - ea) >= init_year + 1, # retire after year 2, LHS is the year of retirement
					 start_year + age - ea >= init_year + 1)         # not really necessary since we already have age >= age.r
)

# %>%
# data.table(key = "start.year,ea,age.death,age")
# liab.death %<>% mutate(B.death = 0, ALx.death = 0)

# TODO: very slow, think about how to optimize
liab_death <- merge(liab_death,
										select(liab_active, start_year, ea, age, Bx.death, gx.death, ax.deathBen) %>% data.table(key = "ea,age,start_year"),
										all.x = TRUE,
										by = c("ea", "age","start_year")) %>%
	# arrange(start_year, ea, age_death) %>%
	as.data.frame



liab_death %<>% 
  as.data.frame  %>%
	group_by(start_year, ea, age_death) %>%
	mutate(
		
		# COLA.scale = (1 + cola)^(age - min(age)),         # COLA.scale in liab.active does not trace back long enough
		# ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale), # COLA.scale in liab.active does not trace back long enough
		
		year       = start_year + age - ea,
		year_death = start_year + age_death - ea, # year of death of the active
		Bx.death   = ifelse(is.na(Bx.death), 0, Bx.death),  # just for safety
		
		#  Lump sum death benefit 
		B.death    = ifelse(age == age_death, Bx.death, 0),   # Bx.death[age == age.death] * COLA.scale / COLA.scale[age == age.death],               # Benefits for retirees after year 1
		ALx.death  = ifelse(age == age_death, B.death, 0)                   # B.death * ax.deathBen                                                                # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
		
	) %>% ungroup %>%
	# select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
	filter(year %in% seq(init_year, len = nyear) ) %>%
	select(year, ea, age, year_death, age_death, start_year, B.death, ALx.death) %>%
	arrange(age_death, start_year, ea, age)

# liab.death %>% ungroup %>% arrange(start.year, ea, year.death, age) %>%  head(100)
cat("......DONE\n")




#*******************************************************************************
#                  5.  Constructing final outputs                          #####
#*******************************************************************************

liab_active %<>% ungroup %>% select(start_year, year, ea, age, everything())


# Choosing AL and NC variables corresponding to the chosen acturial method
ALx.servRet.laca.method     <- paste0("ALx.",    actuarial_method, ".servRet.laca")
NCx.servRet.laca.method     <- paste0("NCx.",    actuarial_method, ".servRet.laca")
PVFNCx.servRet.laca.method  <- paste0("PVFNCx.", actuarial_method, ".servRet.laca")

ALx.defrRet.method    <- paste0("ALx.",   actuarial_method, ".defrRet")
NCx.defrRet.method    <- paste0("NCx.",   actuarial_method, ".defrRet")
PVFNCx.defrRet.method <- paste0("PVFNCx.",actuarial_method, ".defrRet")

ALx.disbRet.method     <- paste0("ALx.",   actuarial_method, ".disbRet")
NCx.disbRet.method     <- paste0("NCx.",   actuarial_method, ".disbRet")
PVFNCx.disbRet.method  <- paste0("PVFNCx.",actuarial_method, ".disbRet")

ALx.death.method    <- paste0("ALx.",   actuarial_method, ".death")
NCx.death.method    <- paste0("NCx.",   actuarial_method, ".death")
PVFNCx.death.method <- paste0("PVFNCx.",actuarial_method, ".death")



var.names <- c(ALx.servRet.laca.method, NCx.servRet.laca.method, PVFNCx.servRet.laca.method,
							 ALx.defrRet.method,      NCx.defrRet.method,      PVFNCx.defrRet.method,
               ALx.death.method,        NCx.death.method,        PVFNCx.death.method,
               ALx.disbRet.method,      NCx.disbRet.method,      PVFNCx.disbRet.method,
                
							 "sx", 
							 #"EEC",
							 
						   "PVFBx.servRet.laca", 
						   "PVFBx.defrRet", 
						   "PVFBx.death", 
						   "PVFBx.disbRet",
						   
							 "PVFSx",
						   #"PVFEEC",
							 
							 "Bx",
							 "Bx.servRet.laca"
						   )


liab_active %<>% 
  filter(year %in% seq(init_year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename("ALx.servRet.laca"   = ALx.servRet.laca.method,  
  			 "NCx.servRet.laca"   = NCx.servRet.laca.method,  
  			 "PVFNCx.servRet.laca" = PVFNCx.servRet.laca.method, 
          
  			 "ALx.defrRet"      = ALx.defrRet.method,     
  			 "NCx.defrRet"      = NCx.defrRet.method,     
  			 "PVFNCx.defrRet" = PVFNCx.defrRet.method, 
  			 
  			 
  			 "ALx.death"  = ALx.death.method, 
  			 "NCx.death"  = NCx.death.method, 
  			 "PVFNCx.death"= PVFNCx.death.method,
          
  			 "ALx.disbRet"   = ALx.disbRet.method,  
  			 "NCx.disbRet"   = NCx.disbRet.method,  
  			 "PVFNCx.disbRet" = PVFNCx.disbRet.method
          )    



liab <- list(active    = liab_active, 
             servRet.la= liab_servRet.la, 
             defrRet   = liab_defrRet,
             disbRet   = liab_disbRet,
						 death     = liab_death
						 )

}








