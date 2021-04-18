

# Inputs:
#	 inputs/data_proc/Data_MEPERS_demographics_20200630_raw.RData
#     - df_nactives_raw
#     - df_nretirees_raw
#     
#     - agecuts_actives
#     - yoscuts_actives
#     
#     - agecuts_retirees


# Outputs:
#    -  imputed member data in tidy format
#    - df_n_servRet_fillin
#    - df_n_disbRet_occ_fillin
#    - df_n_disbRet_nonocc_fillin
#    - df_n_beneficiaries_fillin



#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************
dir_data  <- "inputs/data_proc/"



#*******************************************************************************
#                      ##  Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_MEPERS_demographics_20200630_raw.RData"))



#*******************************************************************************
#              ## Local tools   ####
#*******************************************************************************

# Interpolation of actives
fillin.actives.spreadyos.splineage <- function(lactives) {
	# salary:
	#   first spread uniformly within age.cell-yos.cell group (same salary for all)
	#   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
	#   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
	#   then add ea to the data
	# nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
	
	lactives
	
	adf <- lactives$actives.yos
	agecuts <- lactives$agecuts
	yoscuts <- lactives$yoscuts
	#eacuts <- lactives$eacuts
	minage <- min(agecuts$agelb)
	maxage <- max(agecuts$ageub)
	minyos <- min(yoscuts$yoslb)
	maxyos <- max(yoscuts$yosub)
	
	planname <- paste0(adf$planname[1])
	
	AV_date <- adf$AV_date[1]
	
	# adf %>% select(age, ea, salary) %>% spread(ea, salary)
	# adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
	
	# create a master grouped data frame
	adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
		mutate(pay.cell=nactives.cell * salary.cell) %>%
		mutate(ageidx = findInterval(age.cell, agecuts$agelb),
					 age.lb = agecuts$agelb[ageidx],
					 age.ub = agecuts$ageub[ageidx],
					 yosidx = findInterval(yos.cell, yoscuts$yoslb),
					 yos.lb = yoscuts$yoslb[yosidx],
					 yos.ub = yoscuts$yosub[yosidx]) %>%
		select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  adf.g	

	# expand the grouped data frame to all allowable age-yos combinations 
	xpnd <- function(df) {
		# expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
		# keep the recrods with max ea
		df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
			mutate(ea=age - yos) %>%
			filter((ea >= 20) | (ea<20 & ea==max(ea))) %>%
			select(-ea)
		return(df2)
	}
	
	adf.x <- adf.g %>% rowwise() %>%
		do(cbind(., xpnd(.))) %>%
		ungroup %>%  # get rid of rowwise
		group_by(age.cell, yos.cell) %>%
		mutate(n.cell=n()) %>%
		select(age, yos, everything()) %>%
		arrange(age, yos)
	
	
	# work with the expanded data 
	
	# we have to anchor the endpoints with reasonable values BEFORE computing the spline
	adjustends <- function(age, salary) {
		# the basic idea is that if an endpoint is NA, insert a plausible value
		
		# simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
		# otherwise use the capped value
		firstnonna <- salary[which.min(is.na(salary))]
		lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
		bound <- .5
		firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
		lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
		cap <- function(sal, range) {
			cappedval <- max(sal, range[1])
			cappedval <- min(cappedval, range[2])
			return(cappedval)
		}
		
		salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
		salary.adjusted <- salary
		
		if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
		ilast <- length(salary)
		if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)
		
		return(salary.adjusted)
	}
	
	# test out adjustends
	# fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
	# # various salaries to try out
	# salary <- seq(20, 50, length.out = 10)
	# salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
	# salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
	# salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
	# salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
	# salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
	# age <- 21:30
	# d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
	#   mutate(sal.spline=fs(age, salary),
	#          saladj.spline=fs(age, saladj))
	# d
	# qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)
	
	
	spline.y2 <- function(age, salary, safesalary) {
		# safesalary is what we use if salary has no data
		if(all(is.na(salary))) {
			print("AllNA")
			salary <- safesalary
		}
		salary.adjusted <- adjustends(age, salary)
		
		sp.out <- spline(age, salary.adjusted, xout=age)
		salout <- sp.out$y
		return(salout)
	}
	
	adf.x3 <- adf.x %>% ungroup %>% # MUST be ungrouped or ifelse won't work if there is only one rec in a group
		mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
					 salary.group=ifelse(age==age.cell & yos==yos.cell, salary.cell, NA),
					 salary.group=ifelse(salary.group==0, NA, salary.group),
					 salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
		group_by(yos) %>%
		arrange(age) %>%
		mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
		group_by(age.cell, yos.cell) %>%
		mutate(planname=planname,
		       AV_date = AV_date,
					 pay.unadj=sum(salary.spline.adjep * nactives),
					 adjust=pay.cell / pay.unadj,
					 salary.final=salary.spline.adjep * adjust,
					 pay.adj=sum(salary.final * nactives),
					 ea=age - yos
					 #ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)]
		)
	
	return(adf.x3)
}

# data structure of the input list of fillin.actives.spreadyos.splineag
	# list name: lactives
	# $agecuts:
	#   - age.cell
	#   - agelb
	#   - ageub
	# $yoscuts:
	#   - yos.cell
	#   - yoslb
	#   - yosub
	# $actives.yos
	#   - age.cell
	#   - yos.cell
	#   - age
	#   - yos
	#   - planname
	#   - nactives
	#   - salary


# Interpolation of retirees
fillin.retirees.splineage <- function(list_data) {
	
  # list_data <- lretirees
  
	rdf     <- select(list_data$data, planname, age, N, V) # keep only the vars we want
	agecuts <- list_data$agecuts
	
	planname <- paste0(rdf$planname[1], "")
	# name_N <- list_data$varNames["name_N"]
	# name_V <- list_data$varNames["name_V"]
	
	AV_date <- list_data$data$AV_date[1]
	
	name_N <- list_data$varNames$name_N
	name_V <- list_data$varNames$name_V
	
	
	
	
	# add group ranges to the retirees data frame
	combo <- rdf %>%
		mutate(totben=N * V) %>%
		mutate(ageidx=findInterval(age, agecuts$agelb),
					 age.lb=agecuts$agelb[ageidx],
					 age.ub=agecuts$ageub[ageidx]) %>%
		arrange(age)
	
	# get avg benefits by age, via spline
	avgben <- splong(select(combo, age, V) %>% as.data.frame, "age", min(combo$age.lb):max(combo$age.ub))
	# force benefit to be non-negative DJB added 10/30/2015
	avgben <- avgben %>% mutate(V=ifelse(V<0, 0, V))
	
	guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
		mutate(ageidx=findInterval(age, agecuts$agelb),
					 age.cell=combo$age[match(ageidx, combo$ageidx)],
					 N.cell=combo$N[match(ageidx, combo$ageidx)],
					 V.cell=combo$V[match(ageidx, combo$ageidx)]) %>%
		group_by(age.cell) %>%
		mutate(n.cell=n(),
					 N=N.cell / n.cell, # spread nretirees evenly
					 adjV=avgben$V[match(age, avgben$age)], # get the spline-based avg benefit
					 adjtotben=N * adjV)
	
	# refine the guess by adjusting ensure that we hit the right total benefits in each group
	guessdf2 <- guessdf %>% group_by(age.cell) %>%
		mutate(adjust=mean(N.cell * V.cell) / sum(adjtotben),
					 V=adjV*adjust,
					 totben=N * V)
	
	rdf.fillin <- guessdf2 %>% 
	  mutate(AV_date = AV_date,
	         planname = planname) %>%
	  mutate(across(all_of(c("N", "V")), na2zero)) %>% 
		select(AV_date, planname, age.cell, age, N, V) %>%
		ungroup
	#plyr::rename(c("N" = list_data$varNames["name_N"])))
	
	names(rdf.fillin)[names(rdf.fillin) == "N"] <- name_N
	names(rdf.fillin)[names(rdf.fillin) == "V"] <- name_V
	
	return(rdf.fillin)
}

# data structure of the input list of fillin.actives.spreadyos.splineag
  # list name: ldata
  # $agecuts:
  #   - age.cell
  #   - agelb
  #   - ageub
  # $data
  #   - age.cell
  #   - age
  #   - planname
  #   - N
  #   - V
  # $varNames
  #   - name_N
  #   - name_V  


# get_agecuts <- function(df){
# 	df %>% 
# 		mutate(age.cell = (age_lb + age_ub)/ 2) %>% 
# 		select(age.cell, agelb = age_lb, ageub = age_ub) 
# }





#*******************************************************************************
#                     Initial actives   ####
#*******************************************************************************

# The output data frame includes active members of all tiers.
# df_nactives_raw
# agecuts_actives
# yoscuts_actives


# Prepare data for the interpolation function

make_lactives <- function(df, agecuts, yoscuts){
  lactives <- list(
  	agecuts = agecuts,
  	yoscuts = yoscuts,
  	actives.yos = 
  		df %>%
  		select(
  		  AV_date,
  			planname = grp,
  			age.cell,
  			yos.cell,
  			nactives,
  			salary,
  		) %>%
  		mutate(age = age.cell, yos = yos.cell)
  )
}

fillin_actives <- function(df){

		fillin.actives.spreadyos.splineage(df) %>% 
		ungroup %>%
		select(AV_date,
		       grp = planname, 
		       age, 
		       yos, 
		       ea,
					 nactives, 
					 salary = salary.final) %>% 
		#mutate_at(vars(nactives, salary), funs(na2zero))
	   mutate(across(all_of(c("nactives", "salary")), na2zero)) %>% 
     arrange(ea, yos)
}


## Try using a single group
# lactives <- filter(df_nactives_raw, grp == "inds") %>% make_lactives(agecuts_actives, yoscuts_actives)
# lactives
# 
# nactives_fillin <- fillin_actives(lactives)
# nactives_fillin


## looping through all groups
df_nactives_fillin <- 
  df_nactives_raw %>% 
  split(.$grp) %>% 
  map( ~ make_lactives(.x, agecuts_actives, yoscuts_actives) %>% fillin_actives) %>% 
  bind_rows()

df_nactives_fillin %>% 
	filter(grp == "regular", yos == 0, age <= 24) %>% 
	pull(nactives) %>% 
	sum

# Examine results
# actives_fillin_spread <-
#   df_nactives_fillin %>%
#   filter(grp == "regular") %>%
# 	select(ea, age, nactives) %>%
# 	spread(age, nactives)
# # 
# salary_fillin_spread <-
#   df_nactives_fillin %>%
#   filter(grp == "regular") %>%
# 	select(ea, age, salary) %>%
# 	spread(age, salary)
# 
# df_nactives_fillin %>%
# 	summarise(avg.sal = sum(nactives * salary) / sum(nactives))




#*******************************************************************************
#                    Initial retirees and beneficiaries   ####
#*******************************************************************************

## Local helper functions
make_lretirees <- function(df, agecuts, ben_type){
  
 
  # data structure of the input list of fillin.actives.spreadyos.splineag
  # list name: ldata
  # $agecuts:
  #   - age.cell
  #   - agelb
  #   - ageub
  # $data
  #   - age.cell
  #   - age
  #   - planname
  #   - N
  #   - V
  # $varNames
  #   - name_N
  #   - name_V 
  
  name_V <- paste0("benefit_", ben_type)
  name_N <- paste0("n_", ben_type)
  
  data <- list(
    agecuts = agecuts,
    
    data = 
      df %>%
      select(
        AV_date,
        planname = grp,
        age.cell,
        !!name_N,
        !!name_V
      ) %>%
      rename(N = !!name_N,
             V = !!name_V) %>% 
      mutate(age = age.cell),
    
    varNames = list(name_N = name_N, 
                    name_V = name_V)
  )
}

fillin_retirees <- function(df){
  
  fillin.retirees.splineage(df) %>% 
    ungroup %>%
    select(AV_date,
           grp = planname, 
           age, 
           everything()) %>% 
    # mutate_at(vars(nactives, salary), funs(na2zero))
    # mutate(across(all_of(c("nactives", "salary")), na2zero)) %>% 
    arrange(age)
}


#	There is only one group of retirees in AV of MEPERS, treat them all as servRets
df_nretirees_raw %<>% 
	rename(n_servRet = n_retirees_all,
				 benefit_servRet = benefit)



## Try using a single group
lretirees <- filter(df_nretirees_raw, grp == "regular") %>% make_lretirees(agecuts_retirees, "servRet")
lretirees %>% fillin_retirees()

## Loop through all groups for each type of benefit

df_n_servRet_fillin <- 
  df_nretirees_raw %>% 
  split(.$grp) %>% 
  map( ~ make_lretirees(.x, agecuts_retirees, "servRet") %>% fillin_retirees) %>% 
  bind_rows()


## check results
# df_n_servRet_fillin %>% 
# 	filter(grp == "special") %>% 
# 	pull(n_servRet) %>% 
# 	sum
# 
# df_n_servRet_fillin %>% 
# 	mutate(benefit_tot = n_servRet * benefit_servRet) %>% 
# 	pull(benefit_tot) %>% 
# 	sum
# 	




#*******************************************************************************
#                    Review and save results   ####
#*******************************************************************************

# df_nactives_fillin
# df_n_servRet_fillin
# df_n_disbRet_occ_fillin
# df_n_disbRet_nonocc_fillin
# df_n_beneficiaries_fillin


save(

  df_nactives_fillin,
  df_n_servRet_fillin,

	file = paste0(dir_data, "Data_MEPERS_demographics_20200630_fillin.RData")
)


## save df_n_servRet_fillin to be used as benefit scale for SDRS
df_n_servRet_fillin_MEPERS <- df_n_servRet_fillin 

save(
	df_n_servRet_fillin_MEPERS ,
	file = paste0(dir_data, "Data_MEPERS_servRet_20200630_fillin.RData")
)




