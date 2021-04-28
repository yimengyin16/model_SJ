
# This module create investment return series. 

gen_returns <- function( sim_paramlist_ = sim_paramlist,
                         Global_paramlist_ = Global_paramlist,
                         returnScenarios_ = returnScenarios
                         ){
   
  ## Unquote for development
  # sim_paramlist_ = sim_paramlist
  # Global_paramlist_ = Global_paramlist
  # returnScenarios_ = returnScenarios
  
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(sim_paramlist_,    envir = environment())

## Constant return distributions defined by mean and sd
  if(return_type == "simple"){
    set.seed(sim_paramlist$seed)
    i.r <- matrix(rnorm(nyear*nsim, ir.mean, ir.sd), nyear, nsim)
    i.r <- cbind(rep(ir.mean - ir.sd^2/2, nyear), i.r) # add deterministic returns
    colnames(i.r) <- 0:nsim
  }
  
## Time varying return distributions defined by scenarios   
  if (return_type == "internal"){
    # return_scenario <- "RS1"
    # nsim = 5
  	
    returnScenarios_local <- returnScenarios_ %>% filter(scenario == return_scenario)
    set.seed(sim_paramlist_$seed)
    i.r <- cbind(
      with(returnScenarios_local, create_returns(return_det, 0, period)), # add deterministic returns
      replicate(nsim, with(returnScenarios_local, create_returns(r.mean, r.sd, period)))
    )
    colnames(i.r) <- 0:nsim
  }
  

## T
i.r <- cbind(rep(i, nyear), i.r)  # add constant return that equals discount rate for checking model consistency 

i.shock <- i.r[, 2]
i.shock[3:6] <- c(-0.24, 0.12, 0.12, 0.12) 
i.r <- cbind(i.shock, i.r) 

colnames(i.r) <- c(-2:nsim)



return(i.r)
}

#gen_returns()[,2] 
