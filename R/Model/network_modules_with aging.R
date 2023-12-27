
#######
#This code contains modules for the model
#
# initialize_msm builds the initial network and assigns attributes to nodes
# param_msm enters all model parameters
# control_msm runs all modules in a particular order, determines number and length of runs
# simnet_msm controls the breaking and creating of sexual contact at each timestep
# edges_correct_msm is function which simnet_msm relies on
# infect_msm2 determines new infections at each timestep
# discord_edgelist and discord_edgelist_asym determine susceptible/infected pairs
# progress2 determines state transition for infected individuals
# prevalence_msm - the code breaks if we don't leave this in. 


##################### Netsim Setup Functions #####################################

# Parameters ####
param_msm <- function(init.inf = 10,                                # initial number of infected people 
                      init.hiv.age = c(0.044, 0.109, 0.154, 0.183), # prev rate among 18:24, 25:29, 30:34, 35:39
                      population.size = 10000,
                      behavior.change = behavior.change.switch,
                      wane.change = wane.change.switch,
                      travel.change = travel.change.switch,

                      act.rate.main = 1.54/7, 
                      act.rate.casual = 0.96/7, 
                      act.rate.instant = 1, 
                      e.to.a.rate = 1/6.6,     # transition rate from latent (e) to asymptomatically infectious (a)
                      a.to.i.rate = 1/1,       # transition rate from a to symptomatic. Currently have it set so that a does not transmit
                      treatment.rate = 1/8,    # will take 8 days on average (median) for symptomatic person to seek treatment
                      a.rate = 1/18/365,

                      param.num = 1,
                      param.set = param.num,       # which parameter set used 
                      inf.prob = 0.8,       # probability of infection upon sexual contact
                      treatment.prob = 0.6, # prob of inf. person seeking treatment at all
                      i.to.r.rate = 1/27,  # natural clearance rate
                      
                      nodal.tx = TRUE, 
                      init.vacc1 = 0,
                      init.vacc2 = 0,
                      vacc.effect1 = vacEffect1,
                      vacc.effect2 = vacEffect2,
                      vacc.effect.delayedVax = vacEffectdelayedVax,
                      ...) {
  
  ## Process parameters
  
  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))
  
  class(p) <- "param.net"
  return(p)
  
}

# Control Settings ####
control_msm <- function(simno = 1,
                        nsteps = 730,
                        start = 1,
                        nsims = 10,
                        ncores = 10,
                        cumulative.edgelist = FALSE,
                        truncate.el.cuml = 0,
                        initialize.FUN = initialize_msm,
                        progress.FUN = progress_msm,
                        travel.FUN = travelers,
                        infection.FUN = infect_msm2,
                        new_adults.FUN = new_adults,
                        aging.FUN = aging,
                        vax.FUN = vax_msm,
                        resim_nets.FUN = simnet_msm,
                        prev.FUN = prevalence_msm,
                        verbose.FUN = verbose.net,
                        module.order = NULL,
                        save.nwstats = FALSE,
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
  init.inf <- get_param(dat, "init.inf")
  
  
  # Generate status vector based on nums init vaccinated and init infected (non-overlapping groups)
  # starting values s / i / v
  # initial vaccinations are currently random (if vaccine module is list in control function)
  # initial infections among highest-activity groups unless init size is > than size of those groups 
  
  status <- rep("s", num)
  waned <- rep("no", num)
  riskg <- get_attr(dat, "riskg")
  
  # initial vaccinations - 1 dose
  vaccinated1 = sample(1:num, init.vacc1, replace=FALSE)
  status[vaccinated1] = "v1" 
  
  #initial vaccinations - 2 doses
  vaccinated2 = sample(1:num, init.vacc2, replace=FALSE)
  status[vaccinated2] = "v2" 
  
  #initial waned individuals
  waned.people = sample(1:num, 0, replace=FALSE)
  status[waned.people] = "s.wane" 
  
  # initial infecteds
  risk.high <- which(status == "s" & (riskg == "5" | riskg == "6"))
  
  if (length(risk.high) > init.inf) {
    infected <- sample(risk.high, init.inf, replace=FALSE)
  }
  else {infected <- sample(which(status=="s"), init.inf, replace=FALSE)}
  
  status[infected] <- "i"
  # status[vaccinated1] <- "v1"
  # status[vaccinated2] <- "v2"
  # status[waned.people] <- "s.wane"
  
  # vaccinated time vector (NA until vaccinated)
  vaccTime1 <- rep(NA, num)
  vaccTime1[vaccinated1] <- 1
  
  vaccTime2 <- rep(NA, num)
  vaccTime2[vaccinated2] <- 1
  
  vaccWaneTime <- rep(NA, num)
  vaccWaneTime[waned.people] <- 1
  
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
  
  # which generation is this infection
  vax_protect <- rep(NA, num)

  # Pull sqrt age to get age 
  sqrt.age <- get_attr(dat, "sqrt.age")
  age <- sqrt.age^2
  
  # Set nodal attribute for tx seeking if nodal.tx=TRUE
  if (dat$param$nodal.tx){
    tx.prob <- get_param(dat, "treatment.prob")
    tx.seek <- rep(0, num)
    seek <- sample(1:num, tx.prob*num, replace=FALSE)
    tx.seek[seek] <- 1
    dat <- set_attr(dat, "tx.seek", tx.seek)
  }
  
  # set attributes
  dat <- set_attr(dat, "secInfs", secInfs)     # record how many infections per infection
  dat <- set_attr(dat, "infTime", infTime)     # record time step of infection
  dat <- set_attr(dat, "infTravel", infTrvl)  # record time of travel-associated infection
  dat <- set_attr(dat, "infGen", infGen)       # generation of infection
  dat <- set_attr(dat, "vaccTime1", vaccTime1)   # record time step of vaccination
  dat <- set_attr(dat, "vaccTime2", vaccTime2)   # record time step of vaccination
  dat <- set_attr(dat, "vaccWaneTime", vaccWaneTime)   # record time step of vaccination
  dat <- set_attr(dat, "vax_protect", vax_protect)   # record time step of vaccination
  dat <- set_attr(dat, "status", status)       # initial status
  dat <- set_attr(dat, "waned", waned)       # initial status
  dat <- set_attr(dat, "age", sqrt.age^2)      # age attribute to accompany the sqrt.age attribute
  dat <- set_attr(dat, "recTime", rep(NA,num)) # infection recovery time 
  
  #### Other Setup ####
  dat[["stats"]] <- list()
  dat[["stats"]][["nwstats"]] <- list()
  dat[["temp"]] <- list()
  dat[["epi"]] <- list()
  dat[["mpx_degdist"]] <- list()
  
  dat <- set_epi(dat, "num", at = 1,  num)
  dat <- set_epi(dat, "cuml.infs", at = 1, init.inf)
  dat <- set_epi(dat, "cuml.cases", at = 1, 0)
  dat <- set_epi(dat, "cuml.v1", at = 1, init.vacc1)
  dat <- set_epi(dat, "cuml.v2", at = 1, init.vacc2)
  dat <- set_epi(dat, "cuml.wane", at = 1, 0)
  dat <- set_epi(dat, "prev", at = 1, init.inf)
  
  
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
      old.num <- dat$epi$num[at - 1]              #sets number of nodes at prior timestep
      new.num <- sum(dat$attr$active == 1, na.rm = TRUE) #sets number of nodes at this timestep
      
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

# Adding New Adults  ------------------------

new_adults <- function(dat, at) {
  
  ## Parameters ##
  n <- get_epi(dat, "num", at - 1)
  a.rate <- get_param(dat, "arrival.rate")
  
  ## Process ##
  nArrivalsExp <- n * a.rate
  nArrivals <- rpois(1, nArrivalsExp)
  
  # Update attributes
  if (nArrivals > 0) {
    dat <- append_core_attr(dat, at = at, n.new = nArrivals)
    dat <- append_attr(dat, "status", "s", nArrivals)
    dat <- append_attr(dat, "infTime", NA, nArrivals)
    dat <- append_attr(dat, "age", 18, nArrivals)
    dat <- append_attr(dat, "vaccTime1", NA, nArrivals)
    dat <- append_attr(dat, "vaccTime2", NA, nArrivals)
    dat <- append_attr(dat, "vaccWaneTime", NA, nArrivals)
    dat <- append_attr(dat, "vax_protect", NA, nArrivals)
  }
  
  ## Summary statistics ##
  dat <- set_epi(dat, "a.flow", at, nArrivals)
  
  return(dat)
}

# Aging ---------------------------

aging <- function(dat, at) {
  
  age <- get_attr(dat, "age")
  age <- age + 1/365
  dat <- set_attr(dat, "age", age)
  
  dat <- set_epi(dat, "meanAge", at, mean(age, na.rm = TRUE))
  
  return(dat)
}

# VACCINATION MODULE -----------------
vax_msm <- function(dat, at) {
  
  # Get Params & Attributes 
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  recTime <- get_attr(dat, "recTime")
  vaccTime1 <- get_attr(dat, "vaccTime1")
  vaccTime2 <- get_attr(dat, "vaccTime2")
  vaccWaneTime <- get_attr(dat, "vaccWaneTime")
  waned <- get_attr(dat, "waned")
  wane.change  <- get_param(dat, "wane.change")

  ### Waning immunity ---------
  
  if(wane.change==TRUE){
    ### Susceptible to Vaccine Dose 1  ---------
    nV1 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV1 = which(active == 1 & status == "s")  #Go through those who are active and susceptible in the time step
    nEligV1 = length(idsEligV1) #Return the length of this ID
    nEligV1.adj = round(as.numeric(vd[vd$TIME_STEP == at,'FD_ACTUAL_NORM'])*10000, digits=0)
    
    if (nEligV1 > 0) {
      vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      if (length(vecV1) > 0){
        idsV1 = vecV1
        nV1 = length(idsV1)
        status[idsV1] = "v1"    #Assign status of V1
        vaccTime1[idsV1] = at
        verbose = TRUE
      }
    }
    
    ### Vaccine Dose 1 back to Susceptible ---------
    nWane = 0     #Initiate with 0 people who are vaccinated with 1 dose and wanining
    idsEligWane = which(active == 1 & status == "v1")  #Go through those who are active and vaccinated in the time step
    nEligWane = length(idsEligWane) #Return the length of this ID
    vecWane = c() #Set an empty vector of those who will be waned
    
    if (nEligWane > 0) {
      for (i in 1:nEligWane){    # For loop - goes through each eligible person in the list
        person = idsEligWane[i]   # Get the ID of person i and assign to this placeholder variable
        person.time.vac = at-vaccTime1[person]  # Calculate the time since first vaccine dose and assign to this placeholder variable
        vecWane = append(vecWane, rbinom(1, 1, prob=waning.prob[waning.prob$time == person.time.vac, 'prob_out']))  # Add to vector of whether or not they are waned
      }
      
      vecWane = which(vecWane == 1)   # Restrict to those who only are considered waned
      
      if (length(vecWane) > 0){
        idsWane = idsEligWane[vecWane]
        nWane = length(idsWane)
        status[idsWane] = "s.wane"   #Assign a status back to susceptible
        waned[idsWane] = "yes"
        vaccWaneTime[idsWane] = at
      }
    }
    ### Vaccine Dose 1 to Dose 2 ---------
    nV2 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV2 = which(active == 1 & status == "v1" & (at-vaccTime1)>=28 |
                        active == 1 & status == "s.wane" & (at-vaccTime1)>=28)  #Go through those who are active and susceptible in the time step
    nEligV2 = length(idsEligV2) #Return the length of this ID
    nEligV2.adj = round(as.numeric(vd[vd$TIME_STEP == at,'SD_ACTUAL_NORM'])*10000, digits=0)
    if (nEligV2 > 0) {
      if(nEligV2 < nEligV2.adj){
        nEligV2.adj=nEligV2
        vecV2 = sample(idsEligV2, nEligV2.adj, replace=FALSE)  #Probability of moving from Susc to V1
      }
      else{
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
  
  ## No waning immunity ---------
  
  else if(wane.change==FALSE){
    nWane = 0
    
    ### Susceptible to Vaccine Dose 1  ---------
    nV1 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV1 = which(active == 1 & status == "s")  #Go through those who are active and susceptible in the time step
    nEligV1 = length(idsEligV1) #Return the length of this ID
    nEligV1.adj = round(as.numeric(vd[vd$TIME_STEP == at,'FD_ACTUAL_NORM'])*10000, digits=0)
    
    if (nEligV1 > 0) {
      vecV1 = sample(idsEligV1, nEligV1.adj, replace=FALSE)  #Probability of moving from Susc to V1
      if (length(vecV1) > 0){
        idsV1 = vecV1
        nV1 = length(idsV1)
        status[idsV1] = "v1"    #Assign status of V1
        vaccTime1[idsV1] = at
        verbose = TRUE
      }
    }
    
    ### Vaccine Dose 1 to Dose 2   ---------
    nV2 = 0     #Initiate with 0 people who are vaccinated with 1 dose
    idsEligV2 = which(active == 1 & status == "v1" & (at-vaccTime1)>=28 |
                        active == 1 & status == "s.wane" & (at-vaccTime1)>=28)  #Go through those who are active and susceptible in the time step
    nEligV2 = length(idsEligV2) #Return the length of this ID
    nEligV2.adj = round(as.numeric(vd[vd$TIME_STEP == at,'SD_ACTUAL_NORM'])*10000, digits=0)
    if (nEligV2 > 0) {
      vecV2 = sample(idsEligV2, nEligV2.adj, replace=FALSE)  #Probability of moving from Susc to V1
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
  dat <- set_attr(dat, "vaccWaneTime", vaccWaneTime)
  dat <- set_attr(dat, "waned", waned)
  
  # Update epidemiology trackers 
  prev.vax1 <-  get_epi(dat, "cuml.v1", at=at-1)
  prev.vax2 <-  get_epi(dat, "cuml.v2", at=at-1)
  prev.wane <-  get_epi(dat, "cuml.wane", at=at-1)
  dat <-  set_epi(dat, "cuml.v1", at, prev.vax1 + nV1)
  dat <-  set_epi(dat, "cuml.v2", at, prev.vax2 + nV2)
  dat <-  set_epi(dat, "cuml.wane", at, prev.wane + nWane)
  
  dat <-  set_epi(dat, "v1.flow", at, nV1)
  dat <-  set_epi(dat, "v2.flow", at, nV2)
  dat <- set_epi(dat, "num.v1", at, sum(active == 1 & status == "v1", na.rm = TRUE))
  dat <- set_epi(dat, "num.v2", at, sum(active == 1 & status == "v2", na.rm = TRUE))
  dat <- set_epi(dat, "s.wane", at, sum(active == 1 & status == "s.wane", na.rm = TRUE))
  dat <- set_epi(dat, "ratio.v1.v2", at, (sum(active == 1 & status == "v2", na.rm = TRUE)/sum(active == 1 & status == "v1", na.rm = TRUE)))
  
  return(dat)
}



################### DISEASE RELATED MODULES ############################
#### Travel-Associated Infections #####
travelers = function(dat, at){
  
  ## Load relevant parameters and attributes
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  infTime   <- get_attr(dat, "infTime")
  infTrvl <- get_attr(dat, "infTravel")
  secInfs   <- get_attr(dat, "secInfs")
  travel.change  <- get_param(dat, "travel.change")
  
  #### Travel is turned ON ####
  if(travel.change==TRUE){
    nsteps <- dat$control$nsteps
    
    nInfTrv <- 0     #Initiate with 0 people who will be travelling / become infected
    idsEligInfTrv <- which(active == 1 & status == "s")  #Go through those who are active and susceptible in the time step
    nInfTrv_A <-  length(idsEligInfTrv) #Return the length of this ID
    
    if (nInfTrv_A > 0) {
      vecInfTrv <-  sample(idsEligInfTrv, travelers.months.to.days[at], replace=FALSE)  #Those who get sampled to become infected
      if (length(vecInfTrv) > 0){
        idsInfTrv <- vecInfTrv     # The node IDs that become infected
        nInfTrv <-  length(idsInfTrv)       # The number of travel-associated infections for the time step
        status[idsInfTrv] <-  "e"    #Assign status of exposed but not yet infectious for current timestep
        secInfs[idsInfTrv] <-  0
        infTime[idsInfTrv] <-  at      # Record timestamp of infection
        infTrvl[idsInfTrv] <-  at      # Record timestamp of travel-associated infection to make counting easier
        verbose = TRUE
      }
    }
  }
  #### Travel is turned OFF ####
  else if(travel.change==FALSE){
    nInfTrv <- 0
  }
  
  ## Update nodal attributes
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "infTime", infTime)
  dat <- set_attr(dat, "infTravel", infTrvl)
  dat <- set_attr(dat, "secInfs", secInfs)
  
  ## Update epi trackers
  ## Note, each run, the model will check how many are in 'e' state -- therefore to not run into 
  ## Issues with overwriting trackers, will let the end of the model to sum over all those with e status 
  ## in current timestep
  dat <- set_epi(dat, "travelers", at, nInfTrv)
  
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
      isInf <- matrix(stat %in% c("i", "im", "a"), ncol = 2)
      isSus <- matrix(stat %in% c("s", "s.wane", "v1", "v2"), ncol = 2)
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
  waned    <- get_attr(dat, "waned")
  secInfs   <- get_attr(dat, "secInfs")
  infTime   <- get_attr(dat, "infTime")
  infGen    <- get_attr(dat, "infGen")
  riskgroup <- get_attr(dat, "riskg")
  
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
      del$statusWaned <-waned[del$sus]

      # alter inf prob by vaccination status
      del$transProb <- inf.prob
      del$transProb[del$statusSus=="v1"] <- inf.prob * (1-vacc.effect1)
      # Make the trans Prob time variant, where the vacc.effect is peak at 14 days post 1st dose and then wanes
      
      del$transProb[del$statusSus=="s.wane"] <- inf.prob
      del$transProb[del$statusSus=="v2" & del$statusWaned=="Yes"] <- inf.prob * (1-(vacc.effect2*vacc.effect.delayedVax))
      del$transProb[del$statusSus=="v2"] <- inf.prob * (1-vacc.effect2)
      
      # act rate 
      del$actRate <- act.rate
      
      # final transmission probability 
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate
      
      # filter down to which pairs transmitted infection
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      
      idsNewInf <- unique(del$sus)
      idsOldInf <- unique(del$inf)
      
      if (length(idsNewInf) > 0) {
        
        
        status[idsNewInf]  <- "e"
        secInfs[idsNewInf] <- 0
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
  
  return(dat)
}


# Disease Progression ####
progress_msm <- function(dat, at) {
  
  # e  - latent class
  # a  - asymptomatic and infectious
  # i  - symptomatic and infectious
  # im - infectious but won't seek treatment (not symptomatic enough, no access to care, etc) 
  # r  - recovered and immune 

  ## Get Params & Attributes 
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  recTime <- get_attr(dat, "recTime")
  
  e.to.a.rate <-     get_param(dat, "e.to.a.rate")
  a.to.i.rate <-     get_param(dat, "a.to.i.rate")
  i.to.r.rate <-     get_param(dat, "i.to.r.rate")
  treatment.rate <-  get_param(dat, "treatment.rate")
  treatment.prob <-  get_param(dat, "treatment.prob")
  
  # if treatment seeking is a nodal attribute: 
  if (dat$param$nodal.tx){
    
    tx.seek <- get_attr(dat, "tx.seek")
    
    # natural recovery among infected 
    
    ## Natural Recovery among i & im 
    nRec       <- 0
    idsEligRec <- which(active == 1 & (status == "i" | status == "im"))
    nEligRec   <- length(idsEligRec)
    
    if (nEligRec > 0) {
      vecRec <- which(rbinom(nEligRec, 1, i.to.r.rate) == 1) # vector of who recovers without treatment
      if (length(vecRec) > 0) {
        idsRec <- idsEligRec[vecRec]
        nRec   <- length(idsRec)
        status[idsRec] <- "r"
        recTime[idsRec] <- at
      }
    }
    
    ## Recovery due to treatment 
    nTreat       <- 0
    idsEligTreat <- which(active == 1 & status == "i")
    nEligTreat   <- length(idsEligTreat)
    
    if (nEligTreat > 0) {
      vecTreat <- which(rbinom(nEligTreat, 1, treatment.rate) == 1) # vector of who recovers WITH treatment
      if (length(vecTreat) > 0) {
        idsTreat <- idsEligTreat[vecTreat]
        nTreat   <- length(idsTreat)
        status[idsTreat] <- "r"
        recTime[idsTreat] <- at
      }
    }
    
    ## Asympt. to Infectious but will not seek treatment
    nInf_m       <- 0
    idsEligInf <- which(active == 1 & status == "a" & tx.seek == 0)
    nEligInf   <- length(idsEligInf)
    
    if (nEligInf > 0) {
      vecInf <- which(rbinom(nEligInf, 1, a.to.i.rate) == 1) 
      if (length(vecInf) > 0) {
        idsInf <- idsEligInf[vecInf]
        nInf_m  <- length(idsInf)
        status[idsInf] <- "im"
      }
    }
    
    ## Asympt to Infectious 
    nInf <- 0
    idsEligInf <- which(active == 1 & status == "a" & tx.seek == 1)
    nEligInf <- length(idsEligInf)
    
    if (nEligInf > 0) {
      vecInf <- which(rbinom(nEligInf, 1, a.to.i.rate) == 1) 
      if (length(vecInf) > 0) {
        idsInf <- idsEligInf[vecInf]
        nInf <- length(idsInf)
        status[idsInf] <- "i"
      }
    }
    
  } else {   # if treatment seeking is NOT a nodal attribute: 
    
    ## Natural Recovery among i
    nRec       <- 0
    idsEligRec <- which(active == 1 & (status == "i" | status == "im"))
    nEligRec   <- length(idsEligRec)
    
    if (nEligRec > 0) {
      vecRec <- which(rbinom(nEligRec, 1, i.to.r.rate) == 1) # vector of who recovers without treatment
      if (length(vecRec) > 0) {
        idsRec <- idsEligRec[vecRec]
        nRec   <- length(idsRec)
        status[idsRec] <- "r"
        recTime[idsRec] <- at
      }
    }
    
    ## Recovery due to treatment 
    nTreat       <- 0
    idsEligTreat <- which(active == 1 & status == "i")
    nEligTreat   <- length(idsEligTreat)
    
    if (nEligTreat > 0) {
      vecTreat <- which(rbinom(nEligTreat, 1, treatment.rate) == 1) # vector of who recovers WITH treatment
      if (length(vecTreat) > 0) {
        idsTreat <- idsEligTreat[vecTreat]
        nTreat   <- length(idsTreat)
        status[idsTreat] <- "r"
        recTime[idsTreat] <- at
      }
    }
    
    ## Asympt. to Infectious but will not seek treatment
    nInf_m       <- 0
    idsEligInf <- which(active == 1 & status == "a")
    nEligInf   <- length(idsEligInf)
    
    if (nEligInf > 0) {
      vecInf <- which(rbinom(nEligInf, 1, a.to.i.rate*(1-treatment.prob)) == 1) 
      if (length(vecInf) > 0) {
        idsInf <- idsEligInf[vecInf]
        nInf_m  <- length(idsInf)
        status[idsInf] <- "im"
      }
    }
    
    ## Asympt to Infectious 
    nInf <- 0
    idsEligInf <- which(active == 1 & status == "a")
    nEligInf <- length(idsEligInf)
    
    if (nEligInf > 0) {
      vecInf <- which(rbinom(nEligInf, 1, a.to.i.rate*treatment.prob) == 1) 
      if (length(vecInf) > 0) {
        idsInf <- idsEligInf[vecInf]
        nInf <- length(idsInf)
        status[idsInf] <- "i"
      }
    }
    
  }
  
  ## Latent to Asymptomatically Infectious 
  nAsy       <- 0
  idsEligAsy <- which(active == 1 & status == "e")
  nEligAsy   <- length(idsEligAsy)
  
  if (nEligAsy > 0) {
    vecAsy <- which(rbinom(nEligAsy, 1, e.to.a.rate) == 1)
    if (length(vecAsy) > 0) {
      idsAsy <- idsEligAsy[vecAsy]
      nAsy   <- length(idsAsy)
      status[idsAsy] <- "a"
    }
  }
  
  # Update attributes
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "recTime", recTime)
  
  # Update epidemiology trackers 
  dat <- set_epi(dat, "ea.flow", at, nAsy)
  dat <- set_epi(dat, "ai.flow", at, nInf + nInf_m)
  dat <- set_epi(dat, "ir.flow", at, nRec + nTreat)
  dat <- set_epi(dat, "rec.flow", at, nRec)
  dat <- set_epi(dat, "tx.flow", at, nTreat)

  prev.cases <- get_epi(dat, "cuml.cases", at-1)
  new.cases <- prev.cases + nTreat
  dat <- set_epi(dat, "cuml.cases", at, new.cases)
  
  return(dat)
}


# Track Prevalence & Other Metrics ####
prevalence_msm <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  
  nsteps <- dat$control$nsteps
  rNA <- rep(NA, nsteps)
  
  dat <- set_epi(dat, "num",   at, sum(active == 1))
  dat <- set_epi(dat, "num.e", at, sum(active == 1 & status == "e", na.rm = TRUE))
  dat <- set_epi(dat, "num.a", at, sum(active == 1 & status == "a", na.rm = TRUE))
  dat <- set_epi(dat, "num.i", at, sum(active == 1 & (status == "i"), na.rm = TRUE))
  dat <- set_epi(dat, "num.im", at, sum(active == 1 & (status == "im"), na.rm = TRUE))
  dat <- set_epi(dat, "num.i.total", at, sum(active == 1 & (status == "i" | status == "im"), na.rm = TRUE))
  dat <- set_epi(dat, "num.r", at, sum(active == 1 & status == "r", na.rm = TRUE))
  dat <- set_epi(dat, "num.s", at, sum(active == 1 & status == "s", na.rm = TRUE))
  dat <- set_epi(dat, "num.wane", at, sum(active == 1 & status == "s.wane", na.rm = TRUE))
  dat <- set_epi(dat, "total.s", at, sum(active == 1 & status == "s", na.rm = TRUE) + sum(active == 1 & status == "s.wane", na.rm = TRUE))
  dat <- set_epi(dat, "num.dose1", at, sum(active == 1 & status == "v1", na.rm = TRUE))
  dat <- set_epi(dat, "num.dose2", at, sum(active == 1 & status == "v2", na.rm = TRUE))
  dat <- set_epi(dat, "prev",  at, sum(active == 1 & status=="e") + 
                   sum(active == 1 & status=="a") + 
                   sum(active == 1 & status=="i") + 
                   sum(active == 1 & status=="im"))
  
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
  
  if (at > 5){
    #browser()
    time <- (at-4):at
    
    for (i in 1:length(vec)){
      x <- get_epi(dat, vec[i], time)
      dat <- set_epi(dat, paste0(vec[i], ".avg"), at, mean(x, na.rm=T))
    }
    
  } #else(
  #for (i in length(vec)){
  #  #x <- get_epi(dat, vec[i], at)
  #  dat <- set_epi(dat, paste0(vec[i], ".avg"), at, x)
  #}
  
  # )
  
  return(dat)
}



# # create function to extract median and IQR at each time step for all outputs
# extract_med_iqr <- function(sim){
#   epi <- sim$epi
#   timesteps <- sim$control$nsteps
#   vars <- names(epi)
#   x <- NULL
#   d <- matrix(NA, nrow=timesteps, ncol=3)
#   
#   for (i in vars) {
#     #browser()
#     for (time in 1:timesteps){
#       t <- summary(as.numeric(epi[[i]][time,]))
#       vals <- cbind(t[3], t[2], t[5])
#       d[time,] <- vals
#     }
#     #browser()
#     colnames(d) <- c(paste0(i, ".med"), paste0(i, ".iqr1"), paste0(i, ".iqr3"))
#     x <- cbind(x, d)
#     d <- matrix(NA, nrow=timesteps, ncol=3)
#   }
#   
#   x <- x[-1,]
#   x <- as.data.frame(x)
#   x$Day <- 1:nrow(x)
#   
#   return(x)
# }


# create function to extract mean and IQR at each time step for all outputs
# extract_mean_iqr <- function(sim){
#   epi <- sim$epi
#   timesteps <- sim$control$nsteps
#   vars <- names(epi)
#   x <- NULL
#   d <- matrix(NA, nrow=timesteps, ncol=1)
#   
#   for (i in vars) {
#     #browser()
#     for (time in 1:timesteps){
#       t <- summary(as.numeric(epi[[i]][time,]))
#       vals <- cbind(t[4])
#       d[time,] <- vals
#     }
#     #browser()
#     colnames(d) <- c(paste0(i, ".mean"))
#     x <- cbind(x, d)
#     d <- matrix(NA, nrow=timesteps, ncol=1)
#   }
#   
#   x <- x[-1,]
#   x <- as.data.frame(x)
#   x$Day <- 1:nrow(x)
#   
#   return(x)
# }
