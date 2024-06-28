###################################################################################
# This code contains modules for the model
#
# @initialize_msm  -- Builds the initial network and assigns attributes to nodes
# @param_msm -- Enters all model parameters
# @control_msm -- Runs all modules in a particular order, determines number and length of runs
# @simnet_msm -- Controls the breaking and creating of sexual contact at each timestep
# @edges_correct_msm is function which simnet_msm relies on
# @infect_msm2 determines new infections at each timestep
# @discord_edgelist and @discord_edgelist_asym determine susceptible/infected pairs
# @progress_msm determines state transition for infected individuals
# @prevalence_msm - the code breaks if we don't leave this in. 
# @vax_msm - this code deals with vaccination of nodes + waning immunity from vaccination
###################################################################################


# Parameters ####
param_msm <- function(inf.prob = prob_infection_i,                     # Probability of infection during sexual contact
                      seeds_pre_pride = seeding_pre_pride_i,           # Seeds pre-pride 2022  
                      percent_asymp = prop_asymp_i,                    # Fraction under-detected
                      asymp = asymp_switch_i,
                      
                      reduce_asymp = reduce_asymp_i,                   # Reduce under-detection T/F
                      reduce_asymp_timing = reduce_asymp_timing_i,     # If reducing under-detection -- when?
                      
                      reinfect.switch = reinfection_i,                 # Reinfections T/F
                      reinfection.time = reinfection_time_i,
                      
                      vaxwane.logical = vax_wane_i,                    # Waning of vaccine-derived immunity T/F
                      months_waned_partial = vaxwane_partial_mon_i,    # Mean length of a partial vaccine protection
                      months_waned_full = vaxwane_full_mon_i,          # Mean length of a full vaccine protection
                      
                      post_pride_seeds = post_pride_seeds_i,           # Additional seeding T/F (NOTE: 1 seed at the beginning of each month)
                       
                      seeds_pride_wknd = 10,                           # Number of sides on each day of the city's pride weekend
                      init.hiv.age = c(0.044, 0.109, 0.154, 0.183), 
                      population.size = 10000,
                      behavior.change = TRUE,

                      act.rate.main = 1.54/7,
                      act.rate.casual = 0.96/7,
                      act.rate.instant = 1,

                      e.to.a.rate = 1/6.6,      # Transition from latent (E) to Asymp (A)
                      e.to.i.rate = 1/6.6,      # Transition from latent (E) to Infectious (I)
                      i.to.r.rate = 1/8,        # Clearance rate for Infectious (i.e., Symptomatic)
                      a.to.r.rate = 1/27,       # Clearance rate for Asymptomatic

                      init.vacc1 = 0,
                      init.vacc2 = 0,
                      vacc.effect1 = 0.37,
                      vacc.effect2 = 0.69,
                      vacc.effect.delayedVax = 0.54,
                      vax_mechanism = "leaky",
                      ...) {

  ## Process parameters
  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))
  p$vd <- vd
  p$adjust_matrix <- adjust_matrix
  class(p) <- "param.net"
  return(p)

}

# Control Settings ####
control_msm <- function(simno = 1,#,
                        nsteps =  759, #183 (For fitting),
                        start = 1,
                        nsims = 1,
                        ncores = 1,
                        cumulative.edgelist = TRUE,
                        truncate.el.cuml = 0,
                        initialize.FUN = initialize_msm,
                        progress.FUN = progress_msm,
                        infection.FUN = infect_msm2,
                        vax.FUN = vax_msm,
                        resim_nets.FUN = simnet_msm,
                        prev.FUN = prevalence_msm,
                        verbose.FUN = verbose.net,
                        module.order = NULL,
                        save.nwstats = TRUE,
                        save.other = c("el", "attr"),
                        tergmLite = TRUE,
                        tergmLite.track.duration = FALSE, # CPN2
                        set.control.ergm = control.simulate.formula(MCMC.burnin = 2e5),
                        set.control.stergm = control.simulate.network(),
                        verbose = TRUE,
                        skip.check = TRUE,
                        ...) {
  
  formal.args <- formals(sys.function())
  dot.args <- list(...)
  p <- get_args(formal.args, dot.args)
  
  p$skip.check <- TRUE
  p$save.transmat <- FALSE
  
  bi.mods <- grep(".FUN", names(formal.args), value = TRUE)
  bi.mods <- bi.mods[which(sapply(bi.mods, function(x) !is.null(eval(parse(text = x))),
                                  USE.NAMES = FALSE) == TRUE)]
  p$bi.mods <- bi.mods
  p$user.mods <- grep(".FUN", names(dot.args), value = TRUE)
  p[["f.names"]] <- c(p[["bi.mods"]], p[["user.mods"]])
  p$save.other <- c("attr", "temp", "el", "p", "mpx_degdist")
  
  p$save.network <- FALSE
  if (is.null(p$verbose.int)) {
    p$verbose.int <- 1
  }
  #p <- rep(list(p), num.nsims) # Overcome the warning when saving the output
  
  p <- set.control.class("control.net", p) # CPN2
  return(p)
}


##################### Critical Network Modules #####################################

#### Initialize Module #####
# gets called once at the beginning of each simulation to construct networks
# and master dat object 

initialize_msm <- function(x, param, init, control, s, s.wane, v1, v2) { #So this is what sets up the network that is then changed by simnet
  
  dat <- create_dat_object(param, init, control) #Ah, this is why everything depends on init
  
  #### Network Setup ####
  # Initial network setup
  # Simulate each network on the basis of their fits
  # Add to dat object 
  
  dat[["nw"]] <- list()
  nnets <- 3
  for (i in 1:nnets) {
    dat[["nw"]][[i]] <- simulate( 
      x[[i]][["fit"]],
      basis = x[[i]][["fit"]][["newnetwork"]],
      dynamic = FALSE
    )
  }
  nw <- dat[["nw"]]
  
  # Pull Network parameters
  dat[["nwparam"]] <- list()
  for (i in 1:nnets) {
    dat[["nwparam"]][i] <- list(x[[i]][-which(names(x[[i]]) == "fit")])
  }
  
  # Convert to tergmLite method
  dat <- init_tergmLite(dat)
  
  #### Nodal Attributes Setup ####
  dat[["attr"]] <- param[["netstats"]][["attr"]]
  
  num <- network.size(nw[[1]])
  dat <- append_core_attr(dat, 1, num)
  
  # Pull in attributes on network. 
  # We pull from the one-time network because this has both deg.main and deg.pers attributes 
  # (see estimation script)
  nwattr.all <- names(nw[[3]][["val"]][[3]])
  nwattr.use <- nwattr.all[!nwattr.all %in% c("na", "vertex.names")]
  for (i in seq_along(nwattr.use)) {
    dat$attr[[nwattr.use[i]]] <- get.vertex.attribute(nw[[3]], nwattr.use[i])
  }
  
  # Add other attributes 
  
  # First we need some initial conditions parameters
  init.vacc1 <- get_param(dat, "init.vacc1")
  init.vacc2 <- get_param(dat, "init.vacc2")

  waning_logical <- get_param(dat, "vaxwane.logical");
  reinfect.switch <- get_param(dat, "reinfect.switch");
  reinfection.time <- get_param(dat, "reinfection.time");
  
  months_waned_partial <-     get_param(dat, "months_waned_partial")
  months_waned_full <-     get_param(dat, "months_waned_full")
  
  # Generate status vector based on nums init vaccinated and init infected (non-overlapping groups)

  status <- rep("s", num)
  waned_1 <- rep("no", num)
  waned_2 <- rep("no", num)
  riskg <- get_attr(dat, "riskg")
  
  # initial vaccinations - 1 dose
  vaccinated1 = sample(1:num, init.vacc1, replace=FALSE)
  status[vaccinated1] = "v1" 
  
  #initial vaccinations - 2 doses
  vaccinated2 = sample(1:num, init.vacc2, replace=FALSE)
  status[vaccinated2] = "v2" 
  
  #initial waned individuals
  waned.people_1 = sample(1:num, 0, replace=FALSE)
  status[waned.people_1] = "s.wane.1" 
  
  waned.people_2 = sample(1:num, 0, replace=FALSE)
  status[waned.people_2] = "s.wane.2" 
  
  if(waning_logical == TRUE){

    waned_time_partial = round(rweibull(num, shape = 5, scale = (6.5 * 30)),0);
    waned_time_full = round(rweibull(num, shape = 5, scale = (10 * 30)),0);
    
  }else{
    
    waned_time_partial = rep((365 * 1000),num);   # Set to 1000 years (i.e., > life-long)
    waned_time_full = rep((365 * 1000),num);      # Set to 1000 years (i.e., > life-long)
  
  }
  
  if(reinfect.switch == TRUE){

    if(reinfection.time == 6){
      # Approximately median = 6 months
      scale_param_i = 7;
      
    }else if(reinfection.time == 12){
      
      scale_param_i = 14;
      # Approximately median = 12 months
      
    }else if(reinfection.time == 18){
      
      scale_param_i = 21.3; 
      # Approximately median = 18 months
      
    }else if(reinfection.time == 24){
      scale_param_i = 28.2;
      # Approximately median = 24 months

    }
    
    wane_inf_time = round(rweibull(num, 2.32, scale = (30.437 * scale_param_i)),0);
    
  }else{
    wane_inf_time = rep((365 * 1000),num);      # Set to 1000 years (i.e., > life-long)
  }

  infWaneTime <- rep(NA, num);
  
  # vaccinated time vector (NA until vaccinated)
  vaccTime1 <- rep(NA, num)
  vaccTime1[vaccinated1] <- 1  
  
  vaccTime2 <- rep(NA, num)
  vaccTime2[vaccinated2] <- 1
  
  vaccWaneTime_1 <- rep(NA, num)
  vaccWaneTime_1[waned.people_1] <- 1 
  
  vaccWaneTime_2 <- rep(NA, num)
  vaccWaneTime_2[waned.people_2] <- 1
  
  
  # infection time vector (NA until infected)
  infTime <- rep(NA, num)
  infTime[infected] <- 1
  
  # infection time vector (NA until infected)
  infTrvl <- rep(NA, num)

  # secondary infections from node vector (NA until an infection is transmitted from node to partner)
  secInfs <- rep(NA, num)
  secInfs[infected] <- 0
  
  # which generation is this infection
  infGen <- rep(NA, num)
  infGen[infected] <- 1

  # Pull sqrt age to get age 
  sqrt.age <- get_attr(dat, "sqrt.age")
  age <- sqrt.age^2
  
  # Set exit time
  exitTime <- rep(NA, num)

  # # Set nodal attribute for tx seeking if nodal.tx=TRUE
  # if (dat$param$nodal.tx){
  #   tx.prob <- get_param(dat, "treatment.prob")
  #   tx.seek <- rep(0, num)
  #   seek <- sample(1:num, tx.prob*num, replace=FALSE)
  #   tx.seek[seek] <- 1
  #   dat <- set_attr(dat, "tx.seek", tx.seek)
  # }
  
  # set attributes
  dat <- set_attr(dat, "secInfs", secInfs)                        # record how many infections per infection
  dat <- set_attr(dat, "infTime", infTime)                        # record time step of infection
  dat <- set_attr(dat, "infGen", infGen)                          # generation of infection
  dat <- set_attr(dat, "vaccTime1", vaccTime1)                    # record time step of partial vaccination
  dat <- set_attr(dat, "vaccTime2", vaccTime2)                    # record time step of full vaccination
  dat <- set_attr(dat, "vaccWaneTime_1", vaccWaneTime_1)          # record time step of partial vaccine waning
  dat <- set_attr(dat, "vaccWaneTime_2", vaccWaneTime_2)          # record time step of full vaccine waning
  dat <- set_attr(dat, "waned_time_partial", waned_time_partial)  # the randomly assigned times for vax wane - partial
  dat <- set_attr(dat, "waned_time_full", waned_time_full)        # the randomly assigned times for vax wane - full
  dat <- set_attr(dat, "status", status)                          # initial disease-related status (S/E/I/A/V...)
  dat <- set_attr(dat, "waned_1", waned_1)                        # initial waned status, partial (e.g., no one)
  dat <- set_attr(dat, "waned_2", waned_2)                        # initial waned status, full (e.g., no one)
  dat <- set_attr(dat, "age", sqrt.age ^ 2)                       # age attribute to accompany the sqrt.age attribute
  dat <- set_attr(dat, "recTime", rep(NA, num))                   # infection recovery time
  dat <- set_attr(dat, "vax_protect", rep(NA, num))               # infection recovery time
  dat <- set_attr(dat, "infWaneTime", infWaneTime)                # timestep for when someone waned
  dat <- set_attr(dat, "infWaneTiming", rep(NA, num))             # # days since recovery when someone waned
  dat <- set_attr(dat, "wane_inf_time", wane_inf_time)            # randomly assigned time for inf-derived immunity waning
  
  #### Other Setup ####
  dat[["stats"]] <- list()
  dat[["stats"]][["nwstats"]] <- list()
  dat[["temp"]] <- list()
  dat[["epi"]] <- list()
  dat[["mpx_degdist"]] <- list()
  
  dat <- set_epi(dat, "num", at = 1,  num)
  dat <- set_epi(dat, "cuml.infs", at = 1, 0)
  dat <- set_epi(dat, "cuml.cases", at = 1, 0)
  dat <- set_epi(dat, "cuml.v1", at = 1, init.vacc1)
  dat <- set_epi(dat, "cuml.v2", at = 1, init.vacc2)
  dat <- set_epi(dat, "cuml.wane.1", at = 1, 0)
  dat <- set_epi(dat, "cuml.wane.2", at = 1, 0)
  dat <- set_epi(dat, "cuml.infimmune.wane", at = 1, 0)
  dat <- set_epi(dat, "prev", at = 1, 0)
  dat <- set_epi(dat, "num.one.time.p", at = 1, 0)
  dat <- set_epi(dat, "num.new.second.infections",  at = 1, 0)
  
  # Setup Partner List for all 3 networks 
  # (only gets updated if tracking turned on in control function)
  for (n_network in seq_len(3)) {
    dat <- update_cumulative_edgelist(dat, n_network)
  }
  
  # Network statistics
  if (dat[["control"]][["save.nwstats"]]) {
    for (i in seq_along(x)) {
      nwL <- networkLite(dat[["el"]][[i]], dat[["attr"]])
      nwstats <- summary(
        dat[["control"]][["nwstats.formulas"]][[i]],
        basis = nwL,
        term.options = dat[["control"]][["mcmc.control"]][[i]][["term.options"]],
        dynamic = i < 3
      )
      
      dat[["stats"]][["nwstats"]][[i]] <- matrix(
        nwstats, 
        nrow = 1, ncol = length(nwstats),
        dimnames = list(NULL, names(nwstats))
      )
      
      dat[["stats"]][["nwstats"]][[i]] <- 
        as.data.frame(dat[["stats"]][["nwstats"]][[i]])
    }
  }
  
  class(dat) <- "dat"
  return(dat)
  
}

#### Simnet -- Simulate Networks & Update Coefs based on pop size changes ###############
simnet_msm <- function(dat, at) {
  
  #########################
  ## Nest the edges_correct function
  # (workaround for some functions not being accessible when running in parallel even if in global environ)
  
  edges_correct_msm <- function(dat, at) {
    
    behavior.change  <- get_param(dat, "behavior.change")
    
    if(behavior.change == TRUE){    
      old.num <- dat$epi$num[at - 1]              #Get number of nodes at prior timestep
      new.num <- sum(dat$attr$active == 1, na.rm = TRUE) #Get number of nodes at this timestep
      
      for (i in 1:length(dat$nwparam)) {
        
        adjust.val = adjust_matrix[at,'adjust_coef']
        adjust <- log(new.num) - log(old.num)              #calculates log difference between those two
        
        if(i == 3){adjust = log(new.num*adjust.val) - log(old.num)}
        #if(at == 75 & i == 3){adjust <- log(new.num*0.6) - log(old.num)}
        
        #get formation coefficient
        coef.form1 <- get_nwparam(dat, network = i)$coef.form 
        
        # If first timestep and int. network, grab the value so we can set this as a static value
        static_coef <- -19.05107
        
        # Adjust the formation coefficient
        # If inst. network, set the value to the adjusted value + original starting value
        ifelse(i==3, coef.form1[1] <- static_coef + adjust, coef.form1[1] <- coef.form1[1] + adjust)
        
        dat$nwparam[[i]]$coef.form <- coef.form1              #re-records this number
      }
      
      return(dat)}
    
    else if (behavior.change == FALSE){
      old.num <- dat$epi$num[at - 1]                     #sets number of nodes at prior timestep
      new.num <- sum(dat$attr$active == 1, na.rm = TRUE) #sets number of nodes at this timestep
      adjust <- log(old.num) - log(new.num)              #calculates log difference between those two
      
      for (i in 1:length(dat$nwparam)) {
        
        coef.form1 <- get_nwparam(dat, network = i)$coef.form #get formation coefficient
        coef.form1[1] <- coef.form1[1] + adjust               #says to increase or decrease formation of edges
        dat$nwparam[[i]]$coef.form <- coef.form1              #re-records this number
      }
      
      return(dat)}
 }
  
  ##end nesting##
  #########################

  ## Grab parameters from dat object 
  cumulative.edgelist <- get_control(dat, "cumulative.edgelist") # are we tracking the cumulative edgelist (T/F)
  truncate.el.cuml <- get_control(dat, "truncate.el.cuml")       # how long in the past do we keep edgelist
  set.control.stergm <- get_control(dat, "set.control.stergm")   # specific control settings for network simulation
  set.control.ergm <- get_control(dat, "set.control.ergm")       # specific control settings for network simulation
  
  # Update edges coefficients
  dat <- edges_correct_msm(dat, at)    # decides what the formation coefficient is
  
  ## Main network
  for (i in 1:length(dat$el)) {    #I believe this is where loops through overlapping networks
    nwparam <- EpiModel::get_nwparam(dat, network = i)   #get parameters of this network
    isTERGM <- ifelse(nwparam$coef.diss$duration > 1, TRUE, FALSE) #set isTERGM to true is relationships non-instantaneous
    
    nwL <- networkLite(dat[["el"]][[i]], dat[["attr"]])
    
    if (get_control(dat, "tergmLite.track.duration") == TRUE) { #figure out how long relationships have lasted
      nwL %n% "time" <- dat[["nw"]][[i]] %n% "time"
      nwL %n% "lasttoggle" <- dat[["nw"]][[i]] %n% "lasttoggle"
    }
    
    if (isTERGM == TRUE) { #The following happens only if tergm (relationships last)
      dat[["nw"]][[i]] <- simulate( #forms, dissolves contacts. generic function. simulate distribution corresponding to fitted model object
        nwL, #what are the attributes of each node
        formation = nwparam[["formation"]],
        dissolution = nwparam[["coef.diss"]][["dissolution"]],
        coef.form = nwparam[["coef.form"]],
        coef.diss = nwparam[["coef.diss"]][["coef.adj"]],
        constraints = nwparam[["constraints"]],
        time.start = at - 1,
        time.slices = 1,
        time.offset = 1,
        control = set.control.stergm,
        output = "final"
      )
    } else {  #The following happens if tergm is false, e.g. instantaneous relationships
      dat[["nw"]][[i]] <- simulate( #forms contacts
        basis = nwL,
        object = nwparam[["formation"]],
        coef = nwparam[["coef.form"]],
        constraints = nwparam[["constraints"]],
        control = set.control.ergm,
        dynamic = FALSE,
        nsim = 1,
        output = "network"
      )
    }
    
    dat[["el"]][[i]] <- as.edgelist(dat[["nw"]][[i]])
    
    if (get_control(dat, "save.nwstats") == TRUE) {
      term.options <- if (isTERGM == TRUE) {
        set.control.stergm$term.options
      } else {
        set.control.ergm$term.options
      }
      dat$stats$nwstats[[i]] <- rbind(dat$stats$nwstats[[i]],
                                      summary(dat$control$nwstats.formulas[[i]],
                                              basis = nwL,
                                              term.options = term.options,
                                              dynamic = isTERGM))
    }
    
  }
  
  if (get_control(dat, "cumulative.edgelist") == TRUE) {
    for (n_network in seq_len(3)) {
      dat <- update_cumulative_edgelist(dat, n_network, truncate.el.cuml)
    }
  }
  
  # update main degree (nodal attribute based on network status)
  dat$attr$deg.main <- rep(0,length(dat$attr$deg.main))
  el <- get_edgelist(dat, 1)
  if (nrow(el) > 0) {
    el <- el[sample(1:nrow(el)), , drop = FALSE]
    for(j in 1:nrow(el)){
      dat$attr$deg.main[el[j,1]] <- 1
      dat$attr$deg.main[el[j,2]] <- 1
    }
  }
  
  # adjust pers degree
  dat$attr$deg.pers <- rep(0,length(dat$attr$deg.pers))
  el <- get_edgelist(dat, 2)
  if (nrow(el) > 0) {
    el <- el[sample(1:nrow(el)), , drop = FALSE]
    for(j in 1:nrow(el)){
      dat$attr$deg.pers[el[j,1]] <- dat$attr$deg.pers[el[j,1]] + 1
      if(dat$attr$deg.pers[el[j,1]] > 2){dat$attr$deg.pers[el[j,1]] <- 2}
      dat$attr$deg.pers[el[j,2]] <- dat$attr$deg.pers[el[j,2]] + 1
      if(dat$attr$deg.pers[el[j,2]] > 2){dat$attr$deg.pers[el[j,2]] <- 2}
    }
  }
  
  return(dat)
}

##### VACCINATION MODULE #########
vax_msm <- function(dat, at) {
  
  ## Get Params & Attributes 
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  recTime <- get_attr(dat, "recTime")
  vaccTime1 <- get_attr(dat, "vaccTime1")
  vaccTime2 <- get_attr(dat, "vaccTime2")
  vaccWaneTime_1 <- get_attr(dat, "vaccWaneTime_1")
  vaccWaneTime_2 <- get_attr(dat, "vaccWaneTime_2")
  
  waned_time_partial <- get_attr(dat, "waned_time_partial")   # Time when someone wanes from partial vaccination
  waned_time_full <- get_attr(dat, "waned_time_full")   # Time when someone wanes from full vaccination
  
  waned_1 <- get_attr(dat, "waned_1")
  waned_2 <- get_attr(dat, "waned_2")
  vaxwane.logical  <- get_param(dat, "vaxwane.logical")
  vax_protect <- get_attr(dat, "vax_protect")
  vacc.effect1      <- get_param(dat, "vacc.effect1")
  vacc.effect2      <- get_param(dat, "vacc.effect2")
  vacc.effect.delayedVax  <- get_param(dat, "vacc.effect.delayedVax")
  
  pop.size <- network.size(dat[["nw"]][[1]])

  ##### Waning immunity #####
  vd2 <- dat$param$vd
  
  if(vaxwane.logical==TRUE){
    ### Susceptible to Vaccine Dose 1 ####
    nV1 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV1 = which(active == 1 & status == "s")  #Go through those who are active and susceptible in the time step
    nEligV1 = length(idsEligV1) #Return the length of this ID
    nEligV1.adj = round(as.numeric(vd2[vd2$TIME_STEP == at,'FD_ACTUAL_NORM'])*pop.size, digits=0)
    if (nEligV1 > 0) {
      if(nEligV1 < nEligV1.adj){
        nEligV1.adj=nEligV1
        vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      else{
        vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      if (length(vecV1) > 0){
        idsV1 = vecV1
        nV1 = length(idsV1)
        status[idsV1] = "v1"    #Assign status of V1
        vaccTime1[idsV1] = at
        verbose = TRUE
        vax_protect[idsV1] = rbinom(nV1, 1, prob=vacc.effect1)
      }
    }
    
    ### Vaccine Dose 1 back to Susceptible ####
    nWane_1 = 0     #Initiate with 0 people who are vaccinated with 1 dose and waning
    idsEligWane_1 = which(active == 1 & status == "v1")  #Go through those who are active and vaccinated in the time step
    nEligWane_1 = length(idsEligWane_1) #Return the length of this ID
    vecWane_1 = c() #Set an empty vector of those who will be waned
    
    if (nEligWane_1 > 0) {
      
      # which(ifelse(now-vaccTime1[idsEligWane_1] >= waned_time_partial[idsEligWane_1],1,0) == 1)
      vecWane_1 = ifelse(at-vaccTime1[idsEligWane_1] >= waned_time_partial[idsEligWane_1],1,0)
      vecWane_1 = which(vecWane_1 == 1)   # Restrict to those who only are considered waned
      
      if (length(vecWane_1) > 0){
        idsWane_1 = idsEligWane_1[vecWane_1]
        nWane_1 = length(idsWane_1)
        status[idsWane_1] = "s.wane.1"   #Assign a status back to susceptible
        waned_1[idsWane_1] = "yes"
        vaccWaneTime_1[idsWane_1] = at
      }
    }
    
    vd2 <- dat$param$vd
    ### Vaccine Dose 1 to Dose 2 ####
    nV2 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV2 = which(active == 1 & status == "v1" & (at-vaccTime1)>=28 |
                        active == 1 & status == "s.wane.1" & (at-vaccTime1)>=28)  #Go through those who are active and susceptible in the time step
    nEligV2 = length(idsEligV2) #Return the length of this ID
    nEligV2.adj = round(as.numeric(vd2[vd2$TIME_STEP == at,'SD_ACTUAL_NORM'])*pop.size, digits=0)
    if (nEligV2 > 0) {
      if(nEligV2 < nEligV2.adj){
        vecV2 = sample(idsEligV2, nEligV2, replace=FALSE)  #Probability of moving from Susc to V1
      }
      else{
        vecV2 = sample(idsEligV2, nEligV2.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      
      if (length(vecV2) > 0) {
        idsV2 = vecV2
        nV2 = length(idsV2)
        status[idsV2] = "v2"    #Assign status of V1
        vaccTime2[idsV2] = at
        vax_protect[idsV2] = rbinom(nV2, 1, prob=vacc.effect2) #
      }
    }
    
    ### Vaccine Dose 2 back to Susceptible ####
    nWane_2 = 0     #Initiate with 0 people who are vaccinated with 1 dose and waning
    idsEligWane_2 = which(active == 1 & status == "v2")  #Go through those who are active and vaccinated in the time step
    nEligWane_2 = length(idsEligWane_2) #Return the length of this ID
    vecWane_2 = c() #Set an empty vector of those who will be waned
    
    if (nEligWane_2 > 0) {

      vecWane_2 = ifelse(at-vaccTime2[idsEligWane_2] >= waned_time_full[idsEligWane_2],1,0)
      vecWane_2 = which(vecWane_2 == 1)   # Restrict to those who only are considered waned
      
      if (length(vecWane_2) > 0){
        idsWane_2 = idsEligWane_2[vecWane_2]
        nWane_2 = length(idsWane_2)
        status[idsWane_2] = "s.wane.2"   #Assign a status back to susceptible
        waned_2[idsWane_2] = "yes"
        vaccWaneTime_2[idsWane_2] = at
      }
    }
    
  }
  
  ###### No waning immunity ####
  
  else if(vaxwane.logical==FALSE){
    nWane = 0
    nWane_1 = 0
    nWane_2 = 0
    
    vd2 <- dat$param$vd
    ###### Susceptible to Vaccine Dose 1 ####
    nV1 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV1 = which(active == 1 & status == "s")  #Go through those who are active and susceptible in the time step
    nEligV1 = length(idsEligV1) #Return the length of this ID
    nEligV1.adj = round(as.numeric(vd2[vd2$TIME_STEP == at,'FD_ACTUAL_NORM'])*pop.size, digits=0)
    
    if (nEligV1 > 0) {
      if(nEligV1 < nEligV1.adj){
        nEligV1.adj=nEligV1
        vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      else{
        vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      
      if (length(vecV1) > 0){
        idsV1 = vecV1
        nV1 = length(idsV1)
        status[idsV1] = "v1"    #Assign status of V1
        vaccTime1[idsV1] = at
        verbose = TRUE
      }
    }
    
    ###### Vaccine Dose 1 to Dose 2 ####
    vd2 <- dat$param$vd
    
    nV2 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV2 = which(active == 1 & status == "v1" & (at-vaccTime1)>=28)  #Go through those who are active and susceptible in the time step
    nEligV2 = length(idsEligV2) #Return the length of this ID
    nEligV2.adj = round(as.numeric(vd2[vd2$TIME_STEP == at,'SD_ACTUAL_NORM'])*pop.size, digits=0)
    if (nEligV2 > 0) {
      # In some instances depending on the rate of vaccination, we may start to run out of people to vaccinate, therefore,
      # we need to add some logic to bascially take the maximum number of people we can vaccinate
      if(nEligV2.adj > nEligV2){
        vecV2 = sample(idsEligV2, nEligV2, replace=FALSE)  #Probability of moving from Susc to V1
      }
      else if(nEligV2.adj <= nEligV2){
        vecV2 = sample(idsEligV2, nEligV2.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      if (length(vecV2) > 0) {
        idsV2 = vecV2
        nV2 = length(idsV2)
        status[idsV2] = "v2"    #Assign status of V1
        vaccTime2[idsV2] = at
        verbose = TRUE
      }
    }
  }
  
  
  # Update attributes
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "recTime", recTime)
  dat <- set_attr(dat, "vaccTime1", vaccTime1)
  dat <- set_attr(dat, "vaccTime2", vaccTime2)
  dat <- set_attr(dat, "vaccWaneTime_1", vaccWaneTime_1)
  dat <- set_attr(dat, "vaccWaneTime_2", vaccWaneTime_2)
  dat <- set_attr(dat, "waned_1", waned_1)
  dat <- set_attr(dat, "waned_2", waned_2)
  dat <- set_attr(dat, "vax_protect", vax_protect)
  
  # Update epidemiology trackers 
  prev.vax1 <-  get_epi(dat, "cuml.v1", at=at-1)
  prev.vax2 <-  get_epi(dat, "cuml.v2", at=at-1)
  prev.wane_1 <-  get_epi(dat, "cuml.wane.1", at=at-1)
  prev.wane_2 <-  get_epi(dat, "cuml.wane.2", at=at-1)
  dat <-  set_epi(dat, "cuml.v1", at, prev.vax1 + nV1)
  dat <-  set_epi(dat, "cuml.v2", at, prev.vax2 + nV2)
  dat <-  set_epi(dat, "cuml.wane.1", at, prev.wane_1 + nWane_1)
  dat <-  set_epi(dat, "cuml.wane.2", at, prev.wane_2 + nWane_2)
  
  dat <-  set_epi(dat, "v1.flow", at, nV1)
  dat <-  set_epi(dat, "v2.flow", at, nV2)
  dat <- set_epi(dat, "num.v1", at, sum(active == 1 & status == "v1", na.rm = TRUE))
  dat <- set_epi(dat, "num.v2", at, sum(active == 1 & status == "v2", na.rm = TRUE))
  dat <- set_epi(dat, "s.wane.1", at, sum(active == 1 & status == "s.wane.1", na.rm = TRUE))
  dat <- set_epi(dat, "s.wane.2", at, sum(active == 1 & status == "s.wane.2", na.rm = TRUE))
  dat <- set_epi(dat, "ratio.v1.v2", at, (sum(active == 1 & status == "v2", na.rm = TRUE)/sum(active == 1 & status == "v1", na.rm = TRUE)))
  
  return(dat)
}

# Infection ####
##Infection
infect_msm2 <- function(dat, at) {
  
  # model-specific discordant edgelist function
  discord_edgelist_mpx <- function (dat, at, network){ 
    status <- get_attr(dat, "status")
    active <- get_attr(dat, "active")
    el <- get_edgelist(dat, network)
    del <- NULL
    if (nrow(el) > 0) {
      el <- el[sample(1:nrow(el)), , drop = FALSE]
      stat <- matrix(status[el], ncol = 2) 
      isInf <- matrix(stat %in% c("i", "a"), ncol = 2)
      isSus <- matrix(stat %in% c("s", "s.wane.1", "s.wane.2", "v1", "v2"), ncol = 2)  
      SIpairs <- el[isSus[, 1] * isInf[, 2] == 1, , drop = FALSE]
      ISpairs <- el[isSus[, 2] * isInf[, 1] == 1, , drop = FALSE]
      pairs <- rbind(SIpairs, ISpairs[, 2:1])
      if (nrow(pairs) > 0) {
        sus <- pairs[, 1]
        inf <- pairs[, 2]
        del <- data.frame(at, sus, inf)
        keep <- rowSums(matrix(c(active[del$sus], active[del$inf]), 
                               ncol = 2)) == 2
        del <- del[keep, ]
        if (nrow(del) < 1) {
          del <- NULL
        }
      }
    }
    return(del)
  }
  
  
  #### Setup ####
  
  # Get attributes and parameters 
  active    <- get_attr(dat, "active")
  status    <- get_attr(dat, "status")
  waned_1    <- get_attr(dat, "waned_1")
  waned_2    <- get_attr(dat, "waned_2")
  secInfs   <- get_attr(dat, "secInfs")
  infTime   <- get_attr(dat, "infTime")
  infGen    <- get_attr(dat, "infGen")
  riskgroup <- get_attr(dat, "riskg")
  unique_id <- get_attr(dat, "unique_id")
  
  inf.prob         <- get_param(dat, "inf.prob")
  act.rate.main    <- get_param(dat, "act.rate.main")
  act.rate.casual  <- get_param(dat, "act.rate.casual")
  act.rate.instant <- get_param(dat, "act.rate.instant")
  vacc.effect1      <- get_param(dat, "vacc.effect1")
  vacc.effect2      <- get_param(dat, "vacc.effect2")
  vacc.effect.delayedVax  <- get_param(dat, "vacc.effect.delayedVax")
  
  
  # Set up trackers 
  nInf <- 0
  idsInf <- NULL

  # By risk group 
  nInf.q1 <- 0
  nInf.q2 <- 0
  nInf.q3 <- 0
  nInf.q4 <- 0
  nInf.q5 <- 0
  nInf.q6 <- 0
  
  # New infections in each network
  nInfsMain <- 0
  nInfsPers <- 0
  nInfsInst   <- 0
  
  ## Get the number of one-time partnerships for this time step
  num_one_time_p <- get_edgelist(dat, network = 3)
  num_one_time_p <- nrow(num_one_time_p);
  
  ## The number of re-infections
  n_SecondInfections = 0
  
  #### Transmissions ####
  # Loop through discordant edgelists in each network
  for(nw.transmit in 1:3){ 
    
    if(nw.transmit == 1){act.rate <- act.rate.main}
    if(nw.transmit == 2){act.rate <- act.rate.casual}
    if(nw.transmit == 3){act.rate <- act.rate.instant}
    
    # get discordant edgelist
    del <- discord_edgelist_mpx(dat, at, network = nw.transmit)
    
    if (!(is.null(del))) {
      
      # add status column for sus 
      del$statusSus <- status[del$sus]
      del$statusWaned_1 <- waned_1[del$sus]
      del$statusWaned_2 <- waned_2[del$sus]
      
      del$transProb <- inf.prob
      
      if(dat$param$vax_mechanism == "all or nothing"){
        del$transProb[del$statusSus=="v1" & del$vax_protect == 1] <- inf.prob * 0
        del$transProb[del$statusSus=="v1" & del$vax_protect == 0] <- inf.prob
        
        del$transProb[del$statusSus=="v2" & del$vax_protect == 1] <- inf.prob * 0
        del$transProb[del$statusSus=="v2" & del$vax_protect == 0] <- inf.prob
        
      }else if(dat$param$vax_mechanism == "leaky") {
        del$transProb[del$statusSus=="v1"] <- inf.prob * (1-vacc.effect1)
        del$transProb[del$statusSus=="v2"] <- inf.prob * (1-vacc.effect2)
        del$transProb[del$statusSus=="v2" & del$statusWaned=="Yes"] <- inf.prob * (1-vacc.effect.delayedVax)
      }
      
      del$transProb[del$statusSus=="s.wane.1"] <- inf.prob
      del$transProb[del$statusSus=="s.wane.2"] <- inf.prob
      
      # Acquisition rate
      del$actRate <- act.rate
      
      # Final transmission probability =  1 - (1 - transProb) ^ (actRate)
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate
      
      # filter down to which pairs transmitted infection
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      
      idsNewInf <- unique(del$sus)
      idsOldInf <- unique(del$inf)
      
      if (length(idsNewInf) > 0) {
        
        status[idsNewInf]  <- "e"
        
        # Get those who are infected a second time for tracking purposes
        ids_SecondInfections = which(unique_id %in% idsNewInf & is.na(infTime) == F)  #Go through those who are active and susceptible in the time step
        
        # Also we don't want to reset their sec. infections, as this will be used later to estimate Rt
        if(length(ids_SecondInfections) > 0){
          n_SecondInfections = length(ids_SecondInfections);
      
        }

        secInfs[idsNewInf] <- 0  # 
        infTime[idsNewInf] <- at
        
        # in case any id infections more than 1 person during this time step
        if (length(idsOldInf) < length(idsNewInf)) {
          
          infGen[del$sus]  <- infGen[del$inf] + 1
          
          # count how many secondary infections per infecting id 
          tab <- table(del$inf)
          ids <- as.numeric(names(tab))
          infs <- as.numeric(tab)
          secInfs[ids] <- secInfs[ids] + infs
          
        } 
        
        # in case any susceptible ids get infected by more than one person at this time step
        # we pick the first time they show up in the edgelist
        if (length(idsNewInf) < length(idsOldInf)) {
          
          tab <- table(del$sus)
          ids <- as.numeric(names(which(tab>1)))
          
          for (i in 1:length(ids)){
            #if(length(ids)>1) {          browser()}
            d <- del[del$sus %in% ids[i],]
            if(i==1){ 
              first <- d[1,]
            } else {
              first <- rbind(first, d[1,])
            }
          }
          
          single_del <- del[-c(which(del$sus %in% ids)),]
          newdel <- rbind(single_del, first)
          
          idsNewInf <- newdel$sus
          idsOldInf <- newdel$inf
          
          secInfs[idsOldInf] <- secInfs[idsOldInf] + 1 
          infGen[idsNewInf]  <- infGen[idsOldInf] + 1 
          
        }
        
        
        if (length(idsNewInf) == length(idsOldInf)){ 
          
          secInfs[idsOldInf] <- secInfs[idsOldInf] + 1 
          infGen[idsNewInf]  <- infGen[idsOldInf] + 1 
        }
        
        if(nw.transmit == 1){nInfsMain <- length(idsNewInf)}
        if(nw.transmit == 2){nInfsPers <- length(idsNewInf)}
        if(nw.transmit == 3){nInfsInst <- length(idsNewInf)}
        
        idsInf <- c(idsInf, idsNewInf)
        
        nInf <- nInf + length(idsNewInf)
        infected_riskgrp <- riskgroup[idsNewInf]
        nInf.q1 <- nInf.q1 + length(infected_riskgrp[which(infected_riskgrp == 1)])
        nInf.q2 <- nInf.q2 + length(infected_riskgrp[which(infected_riskgrp == 2)])
        nInf.q3 <- nInf.q3 + length(infected_riskgrp[which(infected_riskgrp == 3)])
        nInf.q4 <- nInf.q4 + length(infected_riskgrp[which(infected_riskgrp == 4)])
        nInf.q5 <- nInf.q5 + length(infected_riskgrp[which(infected_riskgrp == 5)])
        nInf.q6 <- nInf.q6 + length(infected_riskgrp[which(infected_riskgrp == 6)])
        
      }
    } 
  }
  
  # Degree of Infected vs Non-Infected ####
  
  # get and combine edgelist
  
  d1 <- get_degree(dat$el[[1]])
  d2 <- get_degree(dat$el[[2]])
  d3 <- get_degree(dat$el[[3]])
  alld <- d1 + d2 + d3
  
  # extract how many times new infecteds show up and their degree
  inf.dist <- NULL
  inf.dist <- alld[unique(idsInf)] 
  
  # sample 50 susceptibles and their degree (including those deg 0, not in edgelists)
  # or however many susceptibles there are 
  
  non.inf.dist <- NULL
  sus <- which(status=="s")
  if (length(sus)>50) {
    sus_sample <- sample(sus, 50, replace=FALSE)
    non.inf.dist <- alld[sus_sample]
  } else {
    if (length(sus)>0) {
      non.inf.dist <- alld[sus]
    }
  }
  
  # track 
  degdist <- dat$mpx_degdist
  degdist[[at]] <- list()
  degdist[[at]][[1]] <- inf.dist
  degdist[[at]][[2]] <- non.inf.dist
  
  dat$mpx_degdist <- degdist
  
  # Seeding initial infecteds ####
  
  #' Seed initial infections for first two weeks
  
  riskg <- get_attr(dat, "riskg")
  
  seeds_pre_pride = get_param(dat,"seeds_pre_pride") # Frequency of single seeds per week
  seeds_pride_wknd = get_param(dat,"seeds_pride_wknd") # Number of seeds per day for 3 day span of pride weekend
  post_pride_seeds = get_param(dat,"post_pride_seeds") # Seeding turned on post-pride 2022
  
  #' Seeding pre-pride
  if(at %in% seq(1,30,by = seeds_pre_pride)){
    ppl_to_infect <- which(status == "s" & (riskg %in% 2:6))
    if (length(ppl_to_infect) > 1) {
      # Randomly infect 1 person
      infected <- sample(ppl_to_infect, 1, replace=FALSE)
    } # Error catch, in case there is truly no one in 2-6
    else {infected <- sample(which(status=="s"), 1, replace=FALSE)}
    
    infTime[infected] <- at
    secInfs[infected] <- 0
    infGen[infected] <- 1
    status[infected] = "a"
    
  }else if(at %in% c(30+31+24:26)){
    ppl_to_infect <- which(status == "s" & (riskg %in% 2:6))
    if (length(ppl_to_infect) > 1) {
      infected <- sample(ppl_to_infect, seeds_pride_wknd, replace=FALSE)
    }
    else {infected <- sample(which(status=="s"), 3, replace=FALSE)}
    
    infTime[infected] <- at
    secInfs[infected] <- 0
    infGen[infected] <- 1
    status[infected] = "a"
    
  }else if(post_pride_seeds == TRUE){
    # Monthly seeding post-fitting period, assuming every month
    if(at %in% as.numeric(seq(as.Date("2022-10-01"),as.Date("2024-05-01"),by = "1 month") - as.Date("2022-04-01"))){
      ppl_to_infect <- which(status == "s" & (riskg %in% 2:6))
      if (length(ppl_to_infect) > 1) {
        # Randomly infect 1 person
        infected <- sample(ppl_to_infect, 1, replace=FALSE)
      } # Error catch, in case there is truly no one in 2-6
      else {infected <- sample(which(status=="s"), 1, replace=FALSE)}
      
      infTime[infected] <- at
      secInfs[infected] <- 0
      infGen[infected] <- 1
      status[infected] = "a"
    }
  }
  

  
  # Update Attrs and Trackers ####
  # nodal attributes 
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "infTime", infTime)
  dat <- set_attr(dat, "infGen", infGen)
  dat <- set_attr(dat, "secInfs", secInfs)
  
  # update epi trackers 
  dat <- set_epi(dat, "se.flow", at, nInf)
  
  prev.infs <- get_epi(dat, "cuml.infs", at=at-1)
  dat <- set_epi(dat, "cuml.infs", at, prev.infs + nInf)

  dat <- set_epi(dat, "se.flow.q1", at, nInf.q1)
  dat <- set_epi(dat, "se.flow.q2", at, nInf.q2)
  dat <- set_epi(dat, "se.flow.q3", at, nInf.q3)
  dat <- set_epi(dat, "se.flow.q4", at, nInf.q4)
  dat <- set_epi(dat, "se.flow.q5", at, nInf.q5)
  dat <- set_epi(dat, "se.flow.q6", at, nInf.q6)
  dat <- set_epi(dat, "se.flow.main", at, nInfsMain)
  dat <- set_epi(dat, "se.flow.pers", at, nInfsPers)
  dat <- set_epi(dat, "se.flow.ot", at, nInfsInst)
  dat <- set_epi(dat, "num.one.time.p", at, num_one_time_p)
  dat <- set_epi(dat, "num.new.second.infections", at, n_SecondInfections)
  
  return(dat)
}


# Disease Progression ####
progress_msm <- function(dat, at) {
  
  # e  - latent class
  # a  - asymptomatic and infectious
  # i  - symptomatic and infectious
  # r  - recovered and immune 
  
  ## Get Params & Attributes 
  active <- get_attr(dat, "active")                 # Are nodes active or not?
  status <- get_attr(dat, "status")                 # Current status (i.e., where they are along SEIR model)
  recTime <- get_attr(dat, "recTime")               # Recovered time
  vaccTime1 <- get_attr(dat, "vaccTime1")           # Vaccination time partial
  vaccTime2 <- get_attr(dat, "vaccTime2")           # Vaccination time full
  infWaneTime <- get_attr(dat, "infWaneTime")       # Tracker to keep track of when people waned at which time step
  infWaneTiming <- get_attr(dat, "infWaneTiming")   # Tracker to keep track of when people waned relative to their recovery time
  wane_inf_time <- get_attr(dat, "wane_inf_time")   # Randomly assigned waning time of inf-derived immunity
  
  e.to.a.rate <-     get_param(dat, "e.to.a.rate")  # 
  e.to.i.rate <-     get_param(dat, "e.to.i.rate")  # 
  i.to.r.rate <-     get_param(dat, "i.to.r.rate")
  a.to.r.rate <-     get_param(dat, "a.to.r.rate")

  reinfect.switch <-     get_param(dat, "reinfect.switch")
  reduce_asymp <-     get_param(dat, "reduce_asymp")
  reduce_asymp_timing <-     get_param(dat, "reduce_asymp_timing")
  
  
  ## LOGICAL CHECK TO SEE IF ASYMPTOMATIC INFECTIONS ARE ENABLED
  if (dat$param$asymp == TRUE) {
    
    ## IF YES, CHECK IF WE ARE ALSO ALLOWING FOR A REDUCTION IN THE %
    ## THAT GO UNDETECTED AFTER SOME TIME "REDUCE_ASYMP_TIMING"
    
    if(reduce_asymp == TRUE){
      if(at > reduce_asymp_timing){
        prop.asy <-  get_param(dat, "percent_asymp")
        prop.asy <- prop.asy * 0.50
      }else{
        prop.asy <-  get_param(dat, "percent_asymp")
      }
    }else{
      prop.asy <-  get_param(dat, "percent_asymp")
    }
    
    ## ASYMPTOMATIC BUT INFECTIOUS (A) -> RECOVERED (R)
    nAtoR       <- 0
    idsEligRec <- which(active == 1 & status == "a") 
    nEligRec   <- length(idsEligRec)
    
    if (nEligRec > 0) {
      vecRec <- which(rbinom(nEligRec, 1, a.to.r.rate) == 1)
      if (length(vecRec) > 0) {
        idsRec <- idsEligRec[vecRec]
        nAtoR   <- length(idsRec)
        status[idsRec] <- "r"
        recTime[idsRec] <- at
      }
    }
    
    ## LATENT (E) -> ASYMPTOMATIC BUT INFECTIOUS (A)
    nAsy       <- 0
    idsEligAsy <- which(active == 1 & status == "e")
    nEligAsy   <- length(idsEligAsy)
    
    if (nEligAsy > 0) {
      vecAsy <- which(rbinom(nEligAsy, 1, e.to.a.rate*prop.asy) == 1) #
      if (length(vecAsy) > 0) {
        idsAsy <- idsEligAsy[vecAsy]
        nAsy   <- length(idsAsy)
        status[idsAsy] <- "a"
        
      }
    }
    
    
  }else if(dat$param$asymp == FALSE){
    prop.asy = 0;   # IF ASYMP TURNED OFF, THEN SET PROP.ASY to ZERO
    nAsy = 0;
    nAtoR = 0;
  }
  
  ## LATENT (E) -> INFECTIOUS AND SYMPTOMATIC (I)
  nInf <- 0
  idsEligInf <- which(active == 1 & status == "e")
  nEligInf <- length(idsEligInf)
  
  # For epi trackers by vac status
  nInf.partial.vac <- 0
  nInf.full.vac <- 0
  nInf.unvacc <- 0
  
  if (nEligInf > 0) {
    vecInf <- which(rbinom(nEligInf, 1, e.to.i.rate*(1-prop.asy)) == 1) 
    if (length(vecInf) > 0) {
      idsInf <- idsEligInf[vecInf]
      nInf <- length(idsInf)
      status[idsInf] <- "i"
      
      #' Trackers for the number of infected people, based on prior vaccination status...
      nInf.partial.vac <- length(which(is.na(vaccTime1[idsInf]) == FALSE & is.na(vaccTime2[idsInf]) == TRUE))  # 
      nInf.full.vac <- length(which(is.na(vaccTime2[idsInf]) == FALSE))
      nInf.unvacc <- length(which(is.na(vaccTime1[idsInf]) == TRUE))  
    }
  }
  
  
  nItoR       <- 0
  idsEligRec <- which(active == 1 & status == "i") 
  nEligRec   <- length(idsEligRec)
    
  if (nEligRec > 0) {
      vecRec <- which(rbinom(nEligRec, 1, i.to.r.rate) == 1)
      if (length(vecRec) > 0) {
        idsRec <- idsEligRec[vecRec]
        nItoR   <- length(idsRec)
        status[idsRec] <- "r"
        recTime[idsRec] <- at
      }
  }
    
  # If waning for recovered
  if (reinfect.switch == TRUE) {
    
    nRtoS <- 0
    idsEligRecovereds <- which(active == 1 & status == "r")
    nEligRecovereds <- length(idsEligRecovereds)
    
    if (nEligRecovereds > 0) {
      vecRtoS = ifelse(at-recTime[idsEligRecovereds] >= wane_inf_time[idsEligRecovereds],1,0)
      vecRtoS = which(vecRtoS == 1)   # Restrict to those who only are considered waned
      
      if (length(nRtoS) > 0){
        ids_Inf_Waned <- idsEligRecovereds[vecRtoS]
        nRtoS <- length(ids_Inf_Waned)
        status[ids_Inf_Waned] <- "s"
        infWaneTime[ids_Inf_Waned] = at                             # Time stamp as to when they waned
        infWaneTiming[ids_Inf_Waned] = at-recTime[ids_Inf_Waned]    # How many days until they waned...
      }
    }
  }else if(reinfect.switch == FALSE){
    nRtoS <- 0
  }
  
  # Update attributes
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "recTime", recTime)
  dat <- set_attr(dat, "infWaneTime", infWaneTime)
  dat <- set_attr(dat, "infWaneTiming", infWaneTiming)
  
  # Update epidemiology trackers 
  dat <- set_epi(dat, "ea.flow", at, nAsy)
  dat <- set_epi(dat, "ei.flow", at, nInf)
  dat <- set_epi(dat, "ar.flow", at, nAtoR)
  dat <- set_epi(dat, "ir.flow", at, nItoR)
  dat <- set_epi(dat, "rs.flow", at, nRtoS)
  
  # New cases by vaccination status
  dat <- set_epi(dat, "new.i.partial.vac", at, nInf.partial.vac)
  dat <- set_epi(dat, "new.i.full.vac", at, nInf.full.vac)
  dat <- set_epi(dat, "new.i.ever.vac", at, nInf.partial.vac + nInf.full.vac)
  dat <- set_epi(dat, "new.i.unvac", at, nInf.unvacc)
  
  prev.cases <- get_epi(dat, "cuml.cases", at-1)
  new.cases <- prev.cases + nInf
  dat <- set_epi(dat, "cuml.cases", at, new.cases)
  
  prev.infection.wane <- get_epi(dat, "cuml.infimmune.wane", at-1)
  new.infection.wane <- prev.infection.wane + nRtoS
  dat <- set_epi(dat, "cuml.infimmune.wane", at, new.infection.wane)
  
  return(dat)
}


# Track Prevalence & Other Metrics ####
prevalence_msm <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  vaccTime1 <- get_attr(dat, "vaccTime1")
  vaccTime2 <- get_attr(dat, "vaccTime2")
  
  nsteps <- dat$control$nsteps
  rNA <- rep(NA, nsteps)
  
  dat <- set_epi(dat, "num",   at, sum(active == 1))
  dat <- set_epi(dat, "num.e", at, sum(active == 1 & status == "e", na.rm = TRUE))
  dat <- set_epi(dat, "num.a", at, sum(active == 1 & status == "a", na.rm = TRUE))
  dat <- set_epi(dat, "num.i", at, sum(active == 1 & (status == "i"), na.rm = TRUE))
  
  ## Get active cases, stratified by vaccination status
  dat <- set_epi(dat, "num.i.total", at, sum(active == 1 & (status == "a" | status == "i"), na.rm = TRUE))
  dat <- set_epi(dat, "num.i.partial.vac", at, sum(active == 1 & status == "i" & is.na(vaccTime1) == FALSE & is.na(vaccTime2) == TRUE, na.rm = TRUE))
  dat <- set_epi(dat, "num.i.full.vac", at, sum(active == 1 & status == "i" & is.na(vaccTime2) == FALSE, na.rm = TRUE))
  dat <- set_epi(dat, "num.i.ever.vac", at, sum(active == 1 & status == "i" & (is.na(vaccTime2) == FALSE | is.na(vaccTime2) == FALSE), na.rm = TRUE))
  dat <- set_epi(dat, "num.i.unvac", at, sum(active == 1 & status == "i" & is.na(vaccTime1) == TRUE, na.rm = TRUE))
  
  ## Get other statuses
  dat <- set_epi(dat, "num.r", at, sum(active == 1 & status == "r", na.rm = TRUE))
  dat <- set_epi(dat, "num.s", at, sum(active == 1 & status == "s", na.rm = TRUE))
  dat <- set_epi(dat, "num.wane.1", at, sum(active == 1 & status == "s.wane.1", na.rm = TRUE))
  dat <- set_epi(dat, "num.wane.2", at, sum(active == 1 & status == "s.wane.2", na.rm = TRUE))
  dat <- set_epi(dat, "total.s", at, sum(active == 1 & status == "s", na.rm = TRUE) + 
                   sum(active == 1 & status == "s.wane.1", na.rm = TRUE) + 
                   sum(active == 1 & status == "s.wane.2", na.rm = TRUE))
  dat <- set_epi(dat, "num.dose1", at, sum(active == 1 & status == "v1", na.rm = TRUE))
  dat <- set_epi(dat, "num.dose2", at, sum(active == 1 & status == "v2", na.rm = TRUE))
  dat <- set_epi(dat, "prev",  at, sum(active == 1 & status=="e") + 
                   sum(active == 1 & status=="a") + 
                   sum(active == 1 & status=="i"))
  
  # growth rate & doubling time based on actual infections 
  
  nt <- get_epi(dat,"cuml.infs", at)
  n0 <- get_epi(dat, "prev", 1)
  
  r <- log(nt/n0)/at
  dtime <- log(2)/r
  
  dat <- set_epi(dat, "growth.rate", at, r)
  dat <- set_epi(dat, "doubling.time", at, dtime)
  
  # growth rate & doubling time based on treated cases 
  # nRec is the number of cases that receive a positive diagnosis per day
  # we assume they immediately begin treatment/remove themselves from sexual population
  
  nt2 <- get_epi(dat, "cuml.cases", at)
  n02 <- 1
  
  if (nt2==0) {
    r2=0
    dtime2=NA
  } else {
    r2 <- log(nt2/n02)/at
    if (r2==0) {dtime2=NA} else {dtime2 <- log(2)/r2}
  }
  
  dat <- set_epi(dat, "growth.rate.diag", at, r2)
  dat <- set_epi(dat, "doubling.time.diag", at, dtime2)
  
  # effective reproduction number R(t)
  # calculated as the mean number of secondary cases among those infections that cleared at time t
  
  recTime <- get_attr(dat, "recTime")
  secInfs <- get_attr(dat, "secInfs")
  status <- get_attr(dat, "status")
  
  # pull those who recovered this time step 
  recs <- which(recTime==at)
  
  # how many did they on average infect
  if (length(recs)>0){
    meanSec <- mean(secInfs[recs], na.rm=T)
  } else { meanSec <- NA }
  
  dat <- set_epi(dat, "rt", at, meanSec)
  
  # 5-day moving average
  vec <- c("rt", "growth.rate", "growth.rate.diag", "doubling.time", "doubling.time.diag")
  
  if (at > 7){  
    #browser()
    time <- (at-6):at
    
    for (i in 1:length(vec)){
      x <- get_epi(dat, vec[i], time)
      dat <- set_epi(dat, paste0(vec[i], ".avg"), at, mean(x, na.rm=T))
    }
    
  } 
  
  return(dat)
}
