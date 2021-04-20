# This script calculate aggregate annual ALs, NCs and benefit payments.


get_aggLiab <- function(tn, 
                        pop_,
                        indivLiab_, 
                        val_paramlist_    =  val_paramlist,
                        Global_paramlist_ =  Global_paramlist
){

  # This function calculates total AL, NC and benefits.

    
  # Run the section below when developing new features.  
   
   # dev -- 
  
   # tn  = "regularAll"
   # pop_ = pop
   # indivLiab_ = indivLiab
   # val_paramlist_    =  val_paramlist
   # Global_paramlist_ =  Global_paramlist
   
   # dev -- 
   
   assign_parmsList(Global_paramlist_, envir = environment())
   assign_parmsList(val_paramlist_,    envir = environment())
  
   # tn <- names(pop_)[1] # tn stands for tier name
   

## Notes on naming conventions:
   # "cellsum" means total AL/NA/... in each year x age x ea cell
   # "yearsum" means sum of AL/NA/... across age x ea in each year. 
   # "actAll"  menas sum of variables related to active members, including life annuity, contingent annuity, and term benefits for actives. (now plus death benefit and disability benefit)
   
#*******************************************************************************
#               ## Liabilities and NCs for actives   ####
#*******************************************************************************

  # Join population data frames and liability data frames. 
  indivLiab_[[tn]]$active <- 
     left_join(pop_[[tn]]$wf_active %>% select(year, ea, age, nactives), 
               indivLiab_[[tn]]$active %>% select(!any_of("yos")), 
               by = c("year", "ea", "age")) %>% 
     mutate(across(
       everything(), na2zero  # replace NAs with 0, so summation involing missing values will not produce NAs. 
     ))
   # indivLiab_[[tn]]$active[-(1:3)] <- colwise(na2zero)(liab_$active[-(1:3)]) # 
  
   
   # Calculate cell totals 
   indivLiab_[[tn]]$active_yearsum <- 
     indivLiab_[[tn]]$active %>% 
     mutate(across(
       !c(year, ea, age, nactives), ~ .x * nactives
     )) %>% 
     group_by(year) %>% 
     # year total for each variable
     summarise(across(!c(ea, age), ~ sum(.x, na.rm = TRUE)),
               .groups = "drop") %>%  
     # sub-total for each type of variable
     mutate(ALx.active    = rowSums(select(., starts_with("ALx"))),
            NCx.active    = rowSums(select(., starts_with("NCx"))),
            PVFBx.active  = rowSums(select(., starts_with("PVFBx"))),
            PVFNCx.active = rowSums(select(., starts_with("PVFNCx"))),
            ) %>% 
     rename(PR = sx) %>% 
     as.matrix()

  indivLiab_[[tn]]$active_yearsum %>% as.data.frame %>% select(year, ends_with(".active"), PR)
  indivLiab_[[tn]]$active_yearsum %>% as.data.frame %>% select(year, ends_with(".servRet.laca"), PR) 
  
  # liab_$active %<>%  
  #   mutate(ALx.laca.cellsum     = ALx.laca * number.a,
  #          ALx.v.cellsum        = ALx.v    * number.a,
  #   			 ALx.disbRet.cellsum  = ALx.disbRet * number.a,
  #          ALx.death.cellsum    = ALx.death * number.a,
  #          ALx.actAll.cellsum   = ALx.laca.cellsum + ALx.v.cellsum + ALx.disbRet.cellsum + ALx.death.cellsum, 
  #          
  #          NCx.laca.cellsum     = NCx.laca * number.a,
  #          NCx.v.cellsum        = NCx.v    * number.a,
  #          NCx.disbRet.cellsum  = NCx.disbRet * number.a,
  #   			 NCx.death.cellsum    = NCx.death * number.a,
  #          NCx.actAll.cellsum   = NCx.laca.cellsum + NCx.v.cellsum + NCx.disbRet.cellsum + NCx.death.cellsum,
  #          
  #          PVFBx.laca.cellsum   = PVFBx.laca * number.a,
  #          PVFBx.v.cellsum      = PVFBx.v    * number.a,
  #   			 PVFBx.disbRet.cellsum= PVFBx.disbRet * number.a,
  #          PVFBx.death.cellsum  = PVFBx.death * number.a,
  #          PVFBx.actAll.cellsum = PVFBx.laca.cellsum + PVFBx.v.cellsum + PVFBx.disbRet.cellsum + PVFBx.death.cellsum,
  #   			 
  #   			 PVFNC.laca.cellsum   = PVFNC.laca * number.a,
  #   			 PVFNC.v.cellsum      = PVFNC.v    * number.a,
  #   			 PVFNC.disbRet.cellsum= PVFNC.disbRet * number.a,
  #   			 PVFNC.death.cellsum  = PVFNC.death * number.a,
  #   			 PVFNC.actAll.cellsum = PVFNC.laca.cellsum + PVFNC.v.cellsum + PVFNC.disbRet.cellsum + PVFNC.death.cellsum,
  #          
  #          PR.cellsum     = sx * number.a,
  #          EEC.cellsum    = EEC * number.a,
  #   			 PVFEEC.cellsum = PVFEEC * number.a,
  #   			 PVFSx.cellsum  = PVFSx * number.a,
  #   			 
  #          runname = runname)
  # 
  # 
  # active.agg <- liab_$active %>%  
  #   group_by(year) %>% 
  #   summarise(
  #     ALx.laca.yearsum    = sum(ALx.laca.cellsum,  na.rm = TRUE),
  #     ALx.v.yearsum       = sum(ALx.v.cellsum,     na.rm = TRUE),
  #     ALx.disbRet.yearsum = sum(ALx.disbRet.cellsum, na.rm = TRUE),
  #     ALx.death.yearsum   = sum(ALx.death.cellsum, na.rm = TRUE),
  #     ALx.actAll.yearsum  = sum(ALx.actAll.cellsum,  na.rm = TRUE), 
  #     
  #     NCx.laca.yearsum    = sum(NCx.laca.cellsum, na.rm = TRUE),
  #     NCx.v.yearsum       = sum(NCx.v.cellsum,    na.rm = TRUE),
  #     NCx.disbRet.yearsum = sum(NCx.disbRet.cellsum, na.rm = TRUE),
  #     NCx.death.yearsum   = sum(NCx.death.cellsum,na.rm = TRUE),
  #     NCx.actAll.yearsum  = sum(NCx.actAll.cellsum,   na.rm = TRUE),
  #     
  #     PVFBx.laca.yearsum    = sum(PVFBx.laca.cellsum, na.rm = TRUE),
  #     PVFBx.v.yearsum       = sum(PVFBx.v.cellsum,    na.rm = TRUE),
  #     PVFBx.disbRet.yearsum = sum(PVFBx.disbRet.cellsum, na.rm = TRUE),
  #     PVFBx.death.yearsum   = sum(PVFBx.death.cellsum,na.rm = TRUE),
  #     PVFBx.actAll.yearsum  = sum(PVFBx.actAll.cellsum,   na.rm = TRUE),
  #     
  #     PVFNC.laca.yearsum    = sum(PVFNC.laca.cellsum, na.rm = TRUE),
  #     PVFNC.v.yearsum       = sum(PVFNC.v.cellsum,    na.rm = TRUE),
  #     PVFNC.disbRet.yearsum = sum(PVFNC.disbRet.cellsum, na.rm = TRUE),
  #     PVFNC.death.yearsum   = sum(PVFNC.death.cellsum,na.rm = TRUE),
  #     PVFNC.actAll.yearsum  = sum(PVFNC.actAll.cellsum,   na.rm = TRUE),
  #     
  #     PR.yearsum     = sum(PR.cellsum,  na.rm = TRUE),
  #     EEC.yearsum    = sum(EEC.cellsum, na.rm = TRUE),
  #     PVFEEC.yearsum = sum(PVFEEC.cellsum, na.rm = TRUE),
  #     PVFSx.yearsum  = sum(PVFSx.cellsum, na.rm = TRUE),
  #     
  #     nactives  = sum(number.a,  na.rm = TRUE)) %>% 
  #     as.matrix # extracting elements from matrices is much faster than from data.frame
  
  # active.agg %>% as.data.frame()


#*******************************************************************************
#         ## Liabilities and benefits for retirees (life annuitants)        ####
#*******************************************************************************

  indivLiab_[[tn]]$servRet.la %<>% data.table(key = "ea,age,year,year_servRet")
  pop_[[tn]]$wf_servRet.la    %<>% data.table(key = "ea,age,year,year_servRet")
  indivLiab_[[tn]]$servRet.la <- 
    merge(
      indivLiab_[[tn]]$servRet.la,
      pop_[[tn]]$wf_servRet.la, 
      by = c("ea", "age","year", "year_servRet"), 
      all.x = TRUE
    ) %>% 
    as.data.frame() %>% 
    mutate(across(everything(), na2zero))  # replace NAs with 0, so summation involing missing values will not produce NAs. 
  
    
  indivLiab_[[tn]]$servRet.la_yearsum <-  
    indivLiab_[[tn]]$servRet.la %>% 
    select(any_of(c("ea", "age", "year", "year_servRet", "age_servRet", "start_year")), n_servRet.la, B.servRet.la, ALx.servRet.la) %>% 
    mutate(across(
      c(B.servRet.la, ALx.servRet.la), ~ .x * n_servRet.la
    )) %>% 
    group_by(year) %>% 
    # year total for each variable
    summarise(across(c(n_servRet.la, B.servRet.la, ALx.servRet.la), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(year <= init_year + nyear - 1) %>% 
    as.matrix
    
  indivLiab_[[tn]]$servRet.la_yearsum %>% as_tibble()  

  # liab_$la  <- data.table(liab_$la, key = "ea,age,year,year_servRet")
  # pop_$la   <- data.table(pop_$la,  key = "ea,age,year,year_servRet")
  # liab_$la  <- merge(pop_$la, liab_$la, by = c("ea", "age","year", "year_servRet"), all.x = TRUE)
  # liab_$la  <- as.data.frame(liab_$la)
  # 
  # liab_$la %<>% 
  #   mutate(ALx.la.cellsum  = ALx.la * number.la,
  #          B.la.cellsum    = B.la   * number.la,
  #          runname         = runname)
  # 
  # la.agg <- liab_$la %>% 
  #   group_by(year) %>% 
  #   summarise(ALx.la.yearsum = sum(ALx.la.cellsum, na.rm = TRUE),
  #             B.la.yearsum   = sum(B.la.cellsum  , na.rm = TRUE),
  #             nla            = sum(number.la , na.rm = TRUE)) %>% 
  #   as.matrix
  # 
 
  
#*******************************************************************************
#               ## Liabilities and benefits for vested terms.   ####
#*******************************************************************************
  
  # Save 10 seconds by using data.table to merge. Update 2/2016: the latest version of left_join looks fast enough.
  
  indivLiab_[[tn]]$defrRet <- 
    left_join(pop_[[tn]]$wf_defrRet %>% select(year, ea, age, year_defrRet = year_defrRet,  n_defrRet), 
              indivLiab_[[tn]]$defrRet %>% select(!any_of("yos")), 
              by = c("year", "ea", "age", "year_defrRet")) %>% 
    mutate(across(
      everything(), na2zero  # replace NAs with 0, so summation involing missing values will not produce NAs. 
    ))
  
  
  indivLiab_[[tn]]$defrRet_yearsum <-  
    indivLiab_[[tn]]$defrRet %>% 
    select(any_of(c("ea", "age", "year", "year_defrRet", "age_defrRet", "start_year")), n_defrRet, B.defrRet, ALx.defrRet) %>% 
    mutate(across(
      c(B.defrRet, ALx.defrRet), ~ .x * n_defrRet
    )) %>% 
    group_by(year) %>% 
    # year total for each variable
    summarise(across(c(n_defrRet, B.defrRet, ALx.defrRet), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(year <= init_year + nyear - 1) %>% 
    as.matrix
  
  indivLiab_[[tn]]$defrRet_yearsum %>% as_tibble() 
  

  # liab_$term <- left_join(pop_$term, liab_$term, by = c("year", "year_term", "ea", "age"))
  # 
  # 
  # liab_$term %<>%
  # 	mutate(ALx.v.cellsum = ALx.v * number.v,
  # 				 B.v.cellsum   = B.v   * number.v,
  # 				 runname = runname)
  # 
  # 
  # term.agg <- liab_$term %>%
  # 	group_by(year) %>%
  # 	summarise(ALx.v.yearsum   = sum(ALx.v.cellsum, na.rm = TRUE),
  # 						B.v.yearsum     = sum(B.v.cellsum  , na.rm = TRUE),
  # 						nterms          = sum(number.v , na.rm = TRUE)) %>%
  # 	# mutate(runname = runname) %>%
  # 	as.matrix
  
  
  
#*******************************************************************************
#    ## Liabilities and benefits for disability benefit (life annuitants)   ####
#*******************************************************************************
  
  indivLiab_[[tn]]$disbRet <- 
    left_join(pop_[[tn]]$wf_disbRet %>% select(year, ea, age, year_disbRet,  n_disbRet), 
              indivLiab_[[tn]]$disbRet %>% select(!any_of("yos")), 
              by = c("year", "ea", "age", "year_disbRet")) %>% 
    mutate(across(
      everything(), na2zero  # replace NAs with 0, so summation involing missing values will not produce NAs. 
    ))
  
  
  indivLiab_[[tn]]$disbRet_yearsum <-  
    indivLiab_[[tn]]$disbRet %>% 
    select(any_of(c("ea", "age", "year", "year_disbRet", "age_disbRet", "start_year")), n_disbRet, B.disbRet, ALx.disbRet) %>% 
    mutate(across(
      c(B.disbRet, ALx.disbRet), ~ .x * n_disbRet
    )) %>% 
    group_by(year) %>% 
    # year total for each variable
    summarise(across(c(n_disbRet, B.disbRet, ALx.disbRet), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(year <= init_year + nyear - 1) %>% 
    as.matrix
  
  indivLiab_[[tn]]$disbRet_yearsum %>% as_tibble() 
  
  
  
  # liab_$disbRet  <- data.table(liab_$disbRet, key = "ea,age,year,year_disbRet")
  # pop_$disbRet   <- data.table(pop_$disbRet,  key = "ea,age,year,year_disbRet")
  # liab_$disbRet  <- merge(pop_$disbRet, liab_$disbRet, by = c("ea", "age","year", "year_disbRet"), all.x = TRUE)
  # liab_$disbRet  <- as.data.frame(liab_$disbRet)
  # 
  # 
  # liab_$disbRet %<>%
  #   mutate(ALx.disbRet.cellsum = ALx.disbRet * number.disbRet,
  #          B.disbRet.cellsum   = B.disbRet   * number.disbRet,
  #          runname = runname)
  # 
  # disbRet.agg <- liab_$disbRet %>%
  #   group_by(year) %>%
  #   summarise(ALx.disbRet.yearsum   = sum(ALx.disbRet.cellsum, na.rm = TRUE),
  #             B.disbRet.yearsum     = sum(B.disbRet.cellsum  , na.rm = TRUE),
  #             ndisbRet              = sum(number.disbRet , na.rm = TRUE)) %>%
  #   # mutate(runname = runname) %>%
  #   as.matrix
  
  # disbRet.agg %>% as.data.frame()
  

#*******************************************************************************
#             ## Liabilities and benefits for death benefit   ####
#*******************************************************************************
  
  indivLiab_[[tn]]$death <- 
    left_join(pop_[[tn]]$wf_deathBen %>% select(year, ea, age, year_death,  n_deathBen), 
              indivLiab_[[tn]]$death %>% select(!any_of("yos")), 
              by = c("year", "ea", "age", "year_death")) %>% 
    mutate(across(
      everything(), na2zero  # replace NAs with 0, so summation involing missing values will not produce NAs. 
    ))
  
  
  indivLiab_[[tn]]$death_yearsum <-  
    indivLiab_[[tn]]$death %>% 
    select(any_of(c("ea", "age", "year", "year_death", "age_death", "start_year")), n_deathBen, B.death, ALx.death) %>% 
    mutate(across(
      c(B.death, ALx.death), ~ .x * n_deathBen
    )) %>% 
    group_by(year) %>% 
    # year total for each variable
    summarise(across(c(n_deathBen, B.death, ALx.death), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(year <= init_year + nyear - 1) %>% 
    as.matrix
  
  indivLiab_[[tn]]$death_yearsum %>% as_tibble() 
  
  
  
  
  # liab_$death   <- data.table(liab_$death,    key = "ea,age,year,year_death")
  # pop_$deathBen <- data.table(pop_$deathBen,  key = "ea,age,year,year_death")
  # liab_$death   <- merge(pop_$deathBen, liab_$death, by = c("ea", "age","year", "year_death"), all.x = TRUE)
  # liab_$death   <- as.data.frame(liab_$death)
  # 
  # liab_$death %<>%
  # 	mutate(ALx.death.cellsum = ALx.death * number.deathBen,
  # 				 B.death.cellsum   = B.death   * number.deathBen,
  # 				 runname = runname)
  # 
  # death.agg <- liab_$death %>%
  # 	group_by(year) %>%
  # 	summarise(ALx.death.yearsum   = sum(ALx.death.cellsum, na.rm = TRUE),
  # 						B.death.yearsum     = sum(B.death.cellsum  , na.rm = TRUE),
  # 						ndeathBen       = sum(number.deathBen , na.rm = TRUE)) %>%
  # 	# mutate(runname = runname) %>%
  # 	as.matrix
  
  #death.agg %>% as.data.frame
  

  
#*******************************************************************************
#             ## Save results   ####
#*******************************************************************************  
  
  

  aggLiab <- list(active     = indivLiab_[[tn]]$active_yearsum, 
                  servRet.la = indivLiab_[[tn]]$servRet.la_yearsum,
                  defrRet    = indivLiab_[[tn]]$defrRet_yearsum,
                  disbRet    = indivLiab_[[tn]]$disbRet_yearsum,
                  death      = indivLiab_[[tn]]$death_yearsum
                  )
                  
  
              # ind_active  = if(paramlist$save.indiv) .liab$active  else "Not saved", 
              # ind_retiree = if(paramlist$save.indiv) .liab$retiree else "Not saved",
              # ind_term    = if(paramlist$save.indiv) .liab$term    else "Not saved")
    
}




#*************************************************************************************************************
#                                     ## Summing up tier values to get total values of a plan   ####
#*************************************************************************************************************

get_AggLiab_sumTiers <- function(AggLiab.list){
 # This function create list of aggregate values of a plan from list of tiers.
 # AggLiab.list :  a series data list of tiers.

  #AggLiab.list <- aggLiab

  # AggLiab.list <- list(...)

  AggLiab.list %<>% lapply( function(List) lapply(List, as.data.frame))

  nTiers <- length(AggLiab.list)
  nTypes <- length(AggLiab.list[[1]])
  TypeNames <- names(AggLiab.list[[1]])

  AggLiab.list2 <- vector("list", nTypes)
  names(AggLiab.list2) <- TypeNames

  for (j in TypeNames){
    AggLiab.list2[[j]] <- lapply(AggLiab.list, function(df){df[[j]]})
  }

  # sum_tiers <- function(df){ df %<>% group_by(year) %>%
  #     summarise_each(funs(sum))
  # }
  
  sum_tiers <- function(df){ 
    df %<>% 
      group_by(year) %>%
      summarise(across(everything(), ~sum(.x)),
                .groups = "drop")
  }
  

  AggLiab_sumTiers <- AggLiab.list2 %>%
    lapply(bind_rows) %>%
    lapply(sum_tiers) %>%
    lapply(as.matrix)

  return(AggLiab_sumTiers)
}






  
  

# start_time_prep_loop <-  proc.time()
# 
# AggLiab <- get_AggLiab()
# 
# end_time_prep_loop <-  proc.time()
# Time_prep_loop <- end_time_prep_loop - start_time_prep_loop


