# Simulation of the demograhics for a single tier of NYCTRS

## Modifications on the original model
  # 1. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
  # 2. The mortality for retirees are now retirement age dependent. (PSERS)


## What's new in version (2)
#   - adding function(s) for calculating new entrants with multiple tiers. 



get_demographics <- function(ls_tierData_,
                             Global_paramlist_ = Global_paramlist,
                             val_paramlist_    = val_paramlist) {
  
## TODO: update
# Inputs 
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - decrement.model:  Decrement table, from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 4) life annuitants
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   

# dev -- 
	
	# ls_tierData_        <- ls_tierData
	# Global_paramlist_ <- Global_paramlist
	# val_paramlist_    <- val_paramlist

	# dev -- 
	
## TODO

if ( length(names(ls_tierData_)) == 2){
 newEnt_byTier <- c(0, 1) #  for now only allow for new entrants in the second tier. 
 names(newEnt_byTier) <- names(ls_tierData_)  
}
  
if ( length(names(ls_tierData_)) == 1){
	newEnt_byTier <- 1 #  for now only allow for new entrants in the second tier. 
	names(newEnt_byTier) <- names(ls_tierData_)  
}
		
	
assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(val_paramlist_, envir = environment())  



#*******************************************************************************
#     Creating a list that stores all demographic data                      ####
#*******************************************************************************

ls_demo <- list()



#*******************************************************************************
#                  Creating arrays for each status                          ####
#*******************************************************************************

## In each 3D array, 
#  dimension 1(row) represents entry age, 
#  dimension 2(column) represents attained age,
#  dimension 3(depth) represents number of year, 
#  dimension 4, if applicable, represents the year of termination/retirement/disability/death. 

# The array of actives has 3 dimensions: ea x age x year 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, 
										range_age, 
										init_year:(init_year + nyear - 1))

# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.servRet.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.servRet.la <- list(range_ea, 
										        	 range_age, 
											         init_year:(init_year + nyear - 1), 
											         init_year:(init_year + nyear - 1))


# The array of vested terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.defrRet      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.defrRet <- list(range_ea, 
									          		 range_age, 
												         init_year:(init_year + nyear - 1), 
												         init_year:(init_year + nyear - 1))





# The array of disability retirees has 4 dimensions: ea x age x year x year of disability
wf_dim.disbRet      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disbRet <- list(range_ea, 
									    			range_age, 
												    init_year:(init_year + nyear - 1), 
												    init_year:(init_year + nyear - 1))

# The array of death beneficiaries has 4 dimensions: ea x age x year x year of death (of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, 
														 range_age, 
														 init_year:(init_year + nyear - 1), 
														 init_year:(init_year + nyear - 1))



for (tierName in names(ls_tierData_)){

ls_demo[[tierName]] <- list(
  wf_active         = array(0, wf_dim, dimnames = wf_dimnames),
  wf_dead           = array(0, wf_dim, dimnames = wf_dimnames),
  
  wf_defrRet   = array(0, wf_dim.defrRet,   dimnames = wf_dimnames.defrRet),

  wf_servRet.la     = array(0, wf_dim.servRet.la, dimnames = wf_dimnames.servRet.la),
  wf_disbRet        = array(0, wf_dim.disbRet,  dimnames = wf_dimnames.disbRet),
  wf_deathBen       = array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
)
}


## Legacy code
# newDeath.act  <- numeric(nyear)
# newDeath.ret  <- numeric(nyear)
# newDeath.term <- numeric(nyear)
# newDisb.act <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting initial population  ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at min_age and retired in year 1. 
# Altough this may produce yos greater than retAge_max - min_ea, it is irrelevant to the calculation since we do not care about initial retirees' yos.  


for (tierData in ls_tierData_){

 ls_demo[[tierData$tier_name]]$wf_active[, , 1] <- 
  expand.grid(ea = range_ea, age = range_age) %>% 
  left_join(select(tierData$df_n_actives, ea, age, nactives), by = c("ea", "age")) %>% 
  spread(age, nactives, fill = 0) %>% 
  select(-ea) %>% 
  as.matrix


ls_demo[[tierData$tier_name]]$wf_servRet.la[, , 1, 1] <- 
  expand.grid(ea = range_ea, age = range_age) %>% 
  left_join(select(tierData$df_n_servRet, ea, age, n_servRet), by = c("ea", "age")) %>% 
  spread(age, n_servRet, fill = 0) %>% 
  select(-ea) %>% 
  as.matrix


ls_demo[[tierData$tier_name]]$wf_disbRet[, , 1, 1] <- 0

  
ls_demo[[tierData$tier_name]]$wf_defrRet[, , 1, 1]  <- 0 # TODO that the initial terms are assigned to year.term = init_year - 1
}

 
# ls_demo$regularAll$wf_active
# ls_demo$sftyAll$wf_active

#*************************************************************************************************************
#                                     Defining population dynamics  ####
#*************************************************************************************************************

## Transition matrices ####
#   Assume the actual decrement rates are the same as the rates in decrement tables.
#   Later we may allow the actual decrement rates to differ from the assumed rates. 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transition matrix
  df %<>% select(age, ea, !!qx) %>% ungroup %>% spread(ea, !!qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transition matrices. 


for (tierData in ls_tierData_){

	# dev -- 
	# tierData <- ls_tierData_$regularAll
	# dev -- 
	
# 1. Where do the actives go
ls_demo[[tierData$tier_name]]$p_active2defrRet         <- make_dmat("qxt",        df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2servRet.la      <- make_dmat("qxr",        df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2disbRet         <- make_dmat("qxd",        df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2dead            <- make_dmat("qxm.pre",    df = filter(tierData$decrements_expanded, year == init_year))
ls_demo[[tierData$tier_name]]$p_active2deathBen        <- make_dmat("qxm.pre",    df = filter(tierData$decrements_expanded, year == init_year))



# 2. Where do the terminated go
# p_term2dead    <- make_dmat("qxm_terms", df = filter(decrement_wf, start_year == min(start_year))) 

ls_demo[[tierData$tier_name]]$p_defrRet2dead <-
  expand_grid(
    ea        = range_ea,
    age       = range_age,
    year      = init_year:(init_year + nyear - 1),
    year_defrRet = init_year:(init_year + nyear - 1)
  ) %>%
  mutate(age_defrRet = age - (year - year_defrRet)) %>%
  left_join(
    tierData$decrements_expanded %>%
      select(year, ea, age, qxm.defrRet) ,
    by = c("year", "ea", "age")
  ) %>%
  mutate(qxm.defrRet = na2zero(qxm.defrRet)) %>%
  select(year, year_defrRet, age, ea, qxm.defrRet) %>%
  arrange(year, year_defrRet, age, ea)  # Order must be maintained!




# 3. Where do the disabled go
#p_disbRet2dead    <- make_dmat("qxm_disbRet", df = filter(decrement_wf, start_year == min(start_year)))

ls_demo[[tierData$tier_name]]$p_disbRet2dead <-
  expand_grid(
    ea        = range_ea,
    age       = range_age,
    year         = init_year:(init_year + nyear - 1),
    year_disbRet = init_year:(init_year + nyear - 1)
  ) %>%
  mutate(age_disbRet = age - (year - year_disbRet)) %>%
  left_join(
    tierData$decrements_expanded %>%
      select(year, ea, age, qxmd.post) ,
    by = c("year", "ea", "age")
  ) %>%
  mutate(qxmd.post = na2zero(qxmd.post)) %>%
  select(year,  year_disbRet, age, ea, qxmd.post) %>%
  arrange(year, year_disbRet, age, ea)  # Order must be maintained!



# 4. Where do the retirees go 
# Before we find better approach, the age_servRet(retriement age) dependent mortality for retirees are given in a data frame containing all combos 
# of year, year_servRet (year of retirement), ea, and age that exist in wf_la. 

ls_demo[[tierData$tier_name]]$p_servRet.la2dead <-
  expand_grid(
    ea        = range_ea,
    age       = range_age,
    year         = init_year:(init_year + nyear - 1),
    year_servRet.la = init_year:(init_year + nyear - 1)
  ) %>%
  mutate(age_servRet.la = age - (year - year_servRet.la)) %>%
  left_join(
    tierData$decrements_expanded %>%
      select(year, ea, age, qxm.post) ,
    by = c("year", "ea", "age")
  ) %>%
  mutate(qxm.post = na2zero(qxm.post)) %>%
  select(year,  year_servRet.la, age, ea, qxm.post) %>%
  arrange(year, year_servRet.la, age, ea) # Order must be maintained!




# 5. Where do the death beneficiaries go
ls_demo[[tierData$tier_name]]$p_deathBen2dead <- make_dmat("qxm.post", df = filter(tierData$decrements_expanded, year == init_year)) # Simplified: weighted average of male and female mortality

}


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 



#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************


# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, new_entrants = TRUE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(max_retAge - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0 * (1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1   # number of workers need to hire
  ne <- size_hire * dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0
  NE[, ] <- 0
  
  if (new_entrants){ 
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
    
  } else {
    return(NE) 
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0, entrants_dist), na.rm = T)

# 
# ls_demo[[tierData$tier_name]]$temp$new_entrants <- 
#   calc_entrants(ls_demo[[tierData$tier_name]]$wf_active[, , j], 
#                 ls_demo[[tierData$tier_name]]$wf_active[, , j] - ls_demo[[tierData$tier_name]]$temp$out_active, 
#                 val_paramlist_$wf_growth, 
#                 dist = tierData$entrants_dist, 
#                 new_entrants = val_paramlist$new_entrants) # new entrants
# 
# 
# 
# define function for determining the number of new entrants

calc_entrants_allTiers <- function(newEnt_byTier_,
                                   delta,
                                   new_entrants = TRUE){
  # TODO: currently, it is assumed that there is only one active tier

  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce
  # growth rate.
  #
  # inputs:
  #   wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  #   wf1: a matrix of workforce after  decrement.
  #   delta: growth rate of workforce
  #   newEnt_byTier: named vector, proportion of new entrants entering each tier. names must be "t4a", "t4b", "t6"
  #
  # returns:
  #   a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  #   and 0 in all other cells.
  #
  # This function uses the following global variables:
  #   - ls_demo
  #   - ls_tierData_
  #   - j
  
  # ls_tierData_$miscAll$entrants_dist
  # newEnt_byTier_ <- c(1, 0) # for now only allow for new entrants in the first tier. 
  # names(newEnt_byTier_) <- names(ls_tierData_)
  # delta <- 0
  
  
  # working age
  working_age <- min(range_age):(max_retAge - 1) # FOR ALL TIERS
 
  
  # Temperary list
  ls_NE <- vector("list", length(ls_tierData_))
  names(ls_NE) <- names(ls_tierData_)
  
  
  # Step 1 compute the size of workforce before and after decrements
 
  for(tierName in names(ls_NE)){
    ls_NE[[tierName]]$size0 <- sum(ls_demo[[tierName]]$wf_active[, , j], na.rm = TRUE)
    ls_NE[[tierName]]$size1 <- sum(ls_demo[[tierName]]$wf_active[, , j] - ls_demo[[tierName]]$temp$out_active, na.rm = TRUE) 
  }
  
  
  # Step 2: computing the total number of new entrants needed
  
  size_target_tot <- (map(ls_NE, ~.x$size0) %>% unlist %>% sum) * (1 + delta)     # size of the workforce next year
  size_hire_tot   <- size_target_tot - (map(ls_NE, ~.x$size1) %>% unlist %>% sum) # number of workers need to hire
  
  
  # Step 3: Allocating new entrants to tiers. 
  
  #tierName <- "miscAll"
  for(tierName in names(ls_NE)){
    ls_NE[[tierName]]$ne <- ls_demo[[tierName]]$wf_active[, , j]
    ls_NE[[tierName]]$ne[,] <- 0 
    
    ls_NE[[tierName]]$ne[, rownames(ls_NE[[tierName]]$ne)] <- diag(size_hire_tot * newEnt_byTier_[tierName] * ls_tierData_[[tierName]]$entrants_dist)
    if(!new_entrants) ls_NE[[tierName]]$ne[ , ] <- 0
  }
  
  return(ls_NE)
}





#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************

# How the next slice of the array (array[, , i + 1]) is defined: 
 # wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
 # i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
  # j <- 1
  # compute the inflow to and outflow
  # with(ls_demo[[tierData$tier_name]], wf_active[, , 1] * p_active2servRet.la)
  
  
  ### Store all changes in run j in a temporary list
  for (tierName in names(ls_demo)){
  
  ls_demo[[tierName]]$temp <- list(
    
    ## Where do the actives go
    active2servRet.la     = with(ls_demo[[tierName]], wf_active[, , j] * p_active2servRet.la),
    # active2servRet      = with(ls_demo[[tierName]], wf_active[, , j] * p_active2servRet),  # This will be used to calculate the number of actives leaving the workforce
    active2defrRet        = with(ls_demo[[tierName]], wf_active[, , j] * p_active2defrRet),     # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2disbRet        = with(ls_demo[[tierName]], wf_active[, , j] * p_active2disbRet),
    
    active2dead           = with(ls_demo[[tierName]], wf_active[, , j] * p_active2dead),
    active2deathBen       = with(ls_demo[[tierName]], wf_active[, , j] * p_active2deathBen), 
    
    
    ## Where do the terminated go
    # term2dead  <- wf_term[, , j, ] * (p_term2dead %>% filter(year == j + init_year - 1))[["qxm_term"]] 
    defrRet2dead    = with(ls_demo[[tierName]], wf_defrRet[, , j, ]   * (p_defrRet2dead   %>% filter(year == j + init_year - 1))[["qxm.defrRet"]]), 
 
    
    ## Where do the retired go
    servRet.la2dead  = with(ls_demo[[tierName]], wf_servRet.la[, , j, ]   * (p_servRet.la2dead   %>% filter(year == j + init_year - 1))[["qxm.post"]]), 
    
    
    ## Where do the disabled la go
    # disbRet2dead  <- wf_disbRet[, , j, ] * (p_disbRet2dead %>% filter(year == j + init_year - 1))[["qxm_disbRet"]] 
    disbRet2dead    = with(ls_demo[[tierName]], wf_disbRet[, , j, ]   * (p_disbRet2dead   %>% filter(year == j + init_year - 1))[["qxmd.post"]]), 
    
    
    ## Where do the death beneficiaries go
    deathBen2dead  = with(ls_demo[[tierName]],  wf_deathBen[, , j, ] * as.vector(p_deathBen2dead))
  )
  }
  
  
  
  # Total inflow and outflow for non-active members
  
  for (tierName in names(ls_demo)){
  
  ls_demo[[tierName]]$temp$out_defrRet <- ls_demo[[tierName]]$temp$defrRet2dead    # This is a 3D array 
  ls_demo[[tierName]]$temp$in_defrRet  <- ls_demo[[tierName]]$temp$active2defrRet  # This is a matrix
  
  
  ls_demo[[tierName]]$temp$out_servRet.la <- ls_demo[[tierName]]$temp$servRet.la2dead       # This is a 3D array (ea x age x year.retire)
  ls_demo[[tierName]]$temp$in_servRet.la  <- ls_demo[[tierName]]$temp$active2servRet.la     # This is a matrix
  
  ls_demo[[tierName]]$temp$out_disbRet <- ls_demo[[tierName]]$temp$disbRet2dead    # This is a 3D array 
  ls_demo[[tierName]]$temp$in_disbRet  <- ls_demo[[tierName]]$temp$active2disbRet  # This is a matrix
  

  ls_demo[[tierName]]$temp$out_deathBen <- ls_demo[[tierName]]$temp$deathBen2dead        # This is a 3D array (ea x age x year.retire)
  ls_demo[[tierName]]$temp$in_deathBen  <- ls_demo[[tierName]]$temp$active2deathBen     # This is a matrix
  
  
  ls_demo[[tierName]]$temp$in_dead <- 
    ls_demo[[tierName]]$temp$active2dead +                                             
    apply(ls_demo[[tierName]]$temp$defrRet2dead,    c(1,2), sum) +   # 
    apply(ls_demo[[tierName]]$temp$servRet.la2dead,      c(1,2), sum) +   # get a matirix of ea x age by summing over year.term/year.retiree
    apply(ls_demo[[tierName]]$temp$disbRet2dead, c(1,2), sum) 
  
  }
  
  # Total inflow and outflow for active members
  for (tierName in names(ls_demo)){
    ls_demo[[tierName]]$temp$out_active <- with(ls_demo[[tierName]]$temp, active2defrRet + active2disbRet + active2servRet.la + active2dead) 
  }
  
  ls_NE <- calc_entrants_allTiers(newEnt_byTier_ = newEnt_byTier, 
                                  delta = val_paramlist_$wf_growth,
  																new_entrants)
  
  # ls_NE$miscAll$ne
  
  for (tierName in names(ls_demo)){
    ls_demo[[tierName]]$temp$new_entrants <- ls_NE[[tierName]]$ne
  }
  
  
  # ls_demo[[tierName]]$temp$new_entrants <- 
  #   calc_entrants(ls_demo[[tierName]]$wf_active[, , j], 
  #                 ls_demo[[tierName]]$wf_active[, , j] - ls_demo[[tierName]]$temp$out_active, 
  #                 val_paramlist_$wf_growth, 
  #                 dist = ls_tierData_[[tierName]]$entrants_dist, 
  #                 new_entrants = val_paramlist_$new_entrants) # new entrants
  # }
  
  
  
  ## Workforce for the next year. 
  
  for (tierName in names(ls_demo)){
    
  # active members  
  ls_demo[[tierName]]$wf_active[, , j + 1] <- 
    ((ls_demo[[tierName]]$wf_active[, , j] - ls_demo[[tierName]]$temp$out_active) %*% A + 
      ls_demo[[tierName]]$temp$new_entrants) 
  
  # service retirees
  ls_demo[[tierName]]$wf_servRet.la[, , j + 1, ] <- 
    apply((ls_demo[[tierName]]$wf_servRet.la[, , j, ] - ls_demo[[tierName]]$temp$out_servRet.la), 3, function(x) x %*% A) %>% 
    array(wf_dim.servRet.la[-3])
  
  ls_demo[[tierName]]$wf_servRet.la[, , j + 1, j + 1] <- ls_demo[[tierName]]$temp$in_servRet.la %*% A
  
  
  # terminated with vested benefits
  ls_demo[[tierName]]$wf_defrRet[, , j + 1, ] <- 
    apply((ls_demo[[tierName]]$wf_defrRet[, , j, ] - ls_demo[[tierName]]$temp$out_defrRet), 3, function(x) x %*% A) %>% 
    array(wf_dim.defrRet[-3])
  
  ls_demo[[tierName]]$wf_defrRet[, , j + 1, j + 1] <- ls_demo[[tierName]]$temp$in_defrRet %*% A     # Note that termination year j = 1 correponds to init_year - 1?
  

  
  # disability retirees
  ls_demo[[tierName]]$wf_disbRet[, , j + 1, ] <- 
    apply((ls_demo[[tierName]]$wf_disbRet[, , j, ] - ls_demo[[tierName]]$temp$out_disbRet), 3, function(x) x %*% A) %>% 
    array(wf_dim.disbRet[-3])
  
  ls_demo[[tierName]]$wf_disbRet[, , j + 1, j + 1] <- ls_demo[[tierName]]$temp$in_disbRet %*% A
  
  
  # dead members 
  ls_demo[[tierName]]$wf_dead[, ,   j + 1]    <- (ls_demo[[tierName]]$wf_dead[, , j] + ls_demo[[tierName]]$temp$in_dead) %*% A
  
  
  # death beneficiaries
  ls_demo[[tierName]]$wf_deathBen[, , j + 1, ] <- 
    apply((ls_demo[[tierName]]$wf_deathBen[, , j, ] - ls_demo[[tierName]]$temp$out_deathBen), 3, function(x) x %*% A) %>% 
    array(wf_dim.deathBen[-3])
  
  ls_demo[[tierName]]$wf_deathBen[, , j + 1, j + 1] <- ls_demo[[tierName]]$temp$in_deathBen %*% A
  }
  
  
  # Remove the temporary list
  for (tierName in names(ls_demo)) {
    ls_demo[[tierName]]$temp <- NULL
  }
}





#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

for (tierName in names(ls_demo)){

# Active members
ls_demo[[tierName]]$wf_active <- 
  adply(ls_demo[[tierName]]$wf_active, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
  rename(year = X1) %>%
  gather(age, nactives, -ea, -year) %>% 
  mutate(year = f2n(year), 
         age = as.numeric(age),
         yos = age - ea) %>% 
  filter(age >= ea) %>% 
  relocate(year, ea, age, yos)


# service retirees
ls_demo[[tierName]]$wf_servRet.la <- 
  data.frame(
    expand.grid(
      ea           = range_ea,
      age          = range_age,
      year         = init_year:(init_year + nyear - 1),
      year_servRet = init_year:(init_year + nyear - 1)
    ),
    n_servRet.la = as.vector(ls_demo[[tierName]]$wf_servRet.la)
  ) %>%
  filter(age >= ea)


# terminated with vested benefits
ls_demo[[tierName]]$wf_defrRet <- 
  data.frame(
    expand.grid(
      ea   = range_ea,
      age  = range_age,
      year = init_year:(init_year + nyear - 1),
      year_defrRet = (init_year):(init_year + nyear - 1)
    ),
    n_defrRet = as.vector(ls_demo[[tierName]]$wf_defrRet)
  ) %>%
  filter(age >= ea)




# disability retirees
ls_demo[[tierName]]$wf_disbRet <-
  data.frame(
    expand.grid(
      ea   = range_ea,
      age  = range_age,
      year = init_year:(init_year + nyear - 1),
      year_disbRet = (init_year):(init_year + nyear - 1)
    ),
    n_disbRet = as.vector(ls_demo[[tierName]]$wf_disbRet)
  ) %>%
  filter(age >= ea)


ls_demo[[tierName]]$wf_deathBen <-
  data.frame(
    expand.grid(
      ea = range_ea,
      age = range_age,
      year = init_year:(init_year + nyear - 1),
      year_death = (init_year):(init_year + nyear - 1)
    ),
    n_deathBen = as.vector(ls_demo[[tierName]]$wf_deathBen)
  ) %>%
  filter(age >= ea)
}


# Final outputs
return(ls_demo)

}


# pop <- get_Population()
# 
# x <- 
# left_join(
# ls_demo$miscAll$wf_active %>% group_by(year) %>% summarise(s = sum(nactives)),
# ls_demo$sftyAll$wf_active %>% group_by(year) %>% summarise(s = sum(nactives)),
# by = "year"
# ) %>% 
#   mutate(tot = s.x + s.y)
# x


