shiny_plot_fit <- function(prof_data, par_list, fit_list, ntraj =1000) {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = total_list = simdat_list = list()

  # loop on all diseases
  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    prof_init_par = par_list[[ip]]

    # location name
    reg_name = mydata$loc_name

    # location population
    pop = mydata$population

    # disease name (covid or flu)
    disease = mydata$disease

    # hospitalization incidence - fitted
    inc = mydata$data_fit$inc

    # number of values for FOI

    nb = nb_vec[ip]

    # dates - fitted

    dates  = mydata$data_fit$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    simdat <- array(0, c(ntraj, ntimes))

    # retrieve posterior for disease

    tab = tab_list[[disease]]

    nlines = dim(tab)[1]

    nparamtot = dim(tab)[2] -1 # it includes the LLK hence the -1

    nparam = nparamtot - 2 * nb

    t0 = 0

    dt = 1./15.

    z <- round(nlines*2/3):nlines
    ind <- sample(z, round( 1.2* ntraj))

    ind <- order(tab[,'llk'])

    # retrieve initial conditions for disease

    state0 = state0_list[[disease]]

    # set the model to 'sirh' or 'seirh' based on disease

    model = prof_init_par$model

    # observations - fitted

    obs = mydata$data_fit$inc

    # print information for the User

    cat("\nCreating Fit Plots For", reg_name, ' ', toupper(disease),'\n')

    if (model == 'sirh') {
      # Here using a time-dependent version
      # parnames_td_sirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2",
      #                      "pop","pH","S0",'I0',"R0", "rho","baseline")

      # pomp(data=data.frame(time = 1:ntimes),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.sirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
      #      paramnames=parnames_td_sirh) -> flu_sirh

      # pomp(data=data.frame(time = 1:ntimes),
      #      times="time",t0=0,
      #      skeleton = vectorfield(td.sirh.ode),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
      #      paramnames=parnames_td_sirh) -> flu_sirh

      state_names = c('S', 'I', 'R', 'H1', 'H2', 'Ih')
      nstates = length(state_names)

      accum = c('Ih')
      iaccum = which(state_names %in% accum)
      naccum = length(accum)

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]

        state0["I0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - state0$I0

        # coef(flu_sirh) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                     mypar['mu_H1H2'], mypar['pH'], mypar['pop'],
        #                     state0["I0"], state0["S0"], state0["R0"],
        #                     mypar['rho'], round(mypar['baseline']), wl = wl)
        # # generate simulation data with the parameters defined above
        #
        # model.pred <- trajectory(flu_sirh, format="data.frame")
        # model.pred$cases <- rpois(ntimes, model.pred[,'Ih'] * mypar[['rho']] + mypar[['baseline']])
        #
        #
        # # model.pred <- simulate(flu_sirh, format="data.frame", nsim = 1)
        # # if (model.pred$cases[which.max(obs)] > round(mypar['baseline'])) {
        #   icount = icount + 1
        #   simdat[icount,] <- model.pred$cases

        yinit = c(state0$S0, state0$I0, 0, 0, 0, 0)

        traj = array(0, c(ntimes, nstates))
        out <- .Fortran('detsirh', nstates = as.integer(nstates), init = as.double(yinit), param = as.double(mypar),
                        nparam = as.integer(nparam), nb = as.integer(nb), time = as.double(times), ntimes = as.integer(length(times)),
                        t0 = as.double(t0), dt = as.double(dt),
                        naccum = as.integer(naccum), iaccum = as.integer(iaccum),
                        traj = as.double(traj), wl = as.double(wl))
        #
        traj = array(out$traj, c(ntimes, nstates))
        colnames(traj) <- state_names
        Ih = traj[,'Ih']
        #
        cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])

        # parms = c(mypar, 'wl' = wl)
        # time0 = parms['time0']
        # results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'lsoda', func=td_sirh_dynamics, parms = parms)
        # results0 <- results0[,-1]
        # yinit0 <- as.numeric(results0[nrow(results0),])
        #
        # if (nb == 2) {
        #   results <- ode(y=yinit0, t = times, method='lsoda', func=td2_sirh_dynamics, parms = parms)
        # } else {
        #   results <- ode(y=yinit0, t = times, method='lsoda', func=td3_sirh_dynamics, parms = parms)
        # }
        #
        # model.pred = results[,-1] # remove the time column
        # colnames(model.pred) = c('S', 'I', 'R', 'H1', 'H2', 'Ih')
        #
        # # generate simulation data with the parameters defined above
        #
        # Ih = c(0, diff(model.pred[,'Ih']))
        # cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])

        icount = icount + 1
        simdat[icount,] <- cases

        if (icount == ntraj) break

      }

      simdat = simdat[1:icount,]

    } else {

      # parnames_td_seirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2","mu_EI", "pop","pH","S0","E0",'I0',"R0", "rho","baseline")
      #
      # pomp(data=data.frame(time = 1:ntimes),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.seirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init.seirh,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","E","I","R","H1","H2","Ic","Ih", 'time'),
      #      paramnames=parnames_td_seirh) -> covid_seir
      state_names = c('S', 'E', 'I', 'R', 'H1', 'H2', 'Ih')
      nstates = length(state_names)

      accum = c('Ih')
      iaccum = which(state_names %in% accum)
      naccum = length(accum)

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]

        state0["I0"] = mypar['I0']
        state0["E0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - (state0$I0+state0$E0)

        # coef(covid_seir) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                       mypar['mu_H1H2'], mypar['mu_EI'], mypar['pH'], mypar['pop'],
        #                       state0['I0'], state0['S0'], state0['E0'], state0['R0'],
        #                       mypar['rho'], round(mypar['baseline']), wl = wl)
        #
        # # generate simulation data with the parameters defined above
        #
        # model.pred <- simulate(covid_seir, format="data.frame", nsim = 1)


        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0, 0)

        traj = array(0, c(ntimes, nstates))
        out <- .Fortran('detseirh', nstates = as.integer(nstates), init = as.double(yinit), param = as.double(mypar),
                        nparam = as.integer(nparam), nb = as.integer(nb), time = as.double(times), ntimes = as.integer(length(times)),
                        t0 = as.double(t0), dt = as.double(dt),
                        naccum = as.integer(naccum), iaccum = as.integer(iaccum),
                        traj = as.double(traj), wl = as.double(wl))
        #
        traj = array(out$traj, c(ntimes, nstates))
        colnames(traj) <- state_names
        Ih = traj[,'Ih']
        #
        cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])

        # parms = c(mypar, 'wl' = wl)
        # time0 = parms['time0']
        # results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'lsoda', func=td_seirh_dynamics, parms = parms)
        # results0 <- results0[,-1]
        # yinit0 <- as.numeric(results0[nrow(results0),])
        # if (nb == 2) {
        #   results <- ode(y=yinit0, t = times, method='lsoda', func=td2_seirh_dynamics, parms = parms)
        # } else {
        #   results <- ode(y=yinit0, t = times, method='lsoda', func=td3_seirh_dynamics, parms = parms)
        # }
        #
        # model.pred = results[,-1] # remove the time column
        # colnames(model.pred) = c('S', 'E', 'I', 'R', 'H1', 'H2', 'Ih')
        #
        # # generate simulation data with the parameters defined above
        #
        # Ih = c(0, diff(model.pred[,'Ih']))

        cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])

        if (max(cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- cases
        }

        if (icount == ntraj) break

      }

      simdat = simdat[1:icount,]

    }

    simdat_list[[disease]] = simdat

     apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = obs)

    total = as.data.frame(total)

    total_list[[disease]] = total

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    # y-label only on left most plot
    if (disease == disease_list[[1]]) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    mycolor = mycolor_list[[disease]]
    mytitle = paste0(reg_name,' - ', toupper(disease),' Mechanistic Fit')

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1
    xlab = paste0(start_year,' - ', end_year)



    pl[[disease]] <- ggplot(data=total,
                            mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color=mycolor)+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.4)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.7)+
      geom_point(aes(y=reported),color='black', alpha = 1., size = 0.5)+
      # geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", linewidth = 1.5) +
      labs(y=ylab,x=xlab)+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.8)) +
      annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)

  } #end of loop over diseases


 # save the results along with the input in an rds file

  filename = paste0(reg_name,'_fit_results_',Sys.Date(),'.rds')
  results = list(prof_data = prof_data, par_list = par_list, fit_list=fit_list, simdat_list = simdat_list)

 # check to see if directory exists
 directory_path = paste0('data/',toupper(reg_name))

 if (!dir.exists(directory_path)) {
   # If it doesn't exist, create the directory
   dir.create(directory_path, recursive = TRUE)  # Use recursive = TRUE to create parent directories if they don't exist
   print(paste("Directory", directory_path, "created."))
 }
 saveRDS(results, file=paste0(directory_path,'/',filename))
 print(paste0('Compartmental Mechanistic Results Saved to data/',toupper(reg_name),'/', filename))

  interactive_plot <- list()
  for (ip in 1:npath) {
    disease = disease_list[ip]
    interactive_plot[[disease]] <- ggplotly(pl[[disease]])
  }

  # interactive_plot <- pl
  cat("\nMaking Plots\n\n")


  if (npath == 2) {
    # suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
    arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], nrows = 1, titleX = TRUE, titleY = TRUE)

  } else {
    # suppressWarnings(pl[[1]])
    arrange_plot <- interactive_plot[[1]]

  }
  return(list(arrange_plot = arrange_plot, total_list = total_list))


}


shiny_plot_stat_fit <- function(prof_data, diseases) {

  ntraj = 1000

  npath = length(diseases)

  disease_list = diseases

  pl = total_list = list()

  # loop on all diseases
  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    # dates - fitted
    dates  = mydata$data_fit_stat$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    # observations - fitted only

    obs_fit = mydata$data_fit_stat$inc

    cat('\nCreating ',toupper(disease),' Statistical Fit for ', reg_name,'\n\n')

    simdat = stat_fit(obs_fit, ntraj)

    reported_fit = obs_fit

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)

    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = reported_fit)

    total = as.data.frame(total)

    total_list[[disease]] = total

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    # y-label only on left most plot
    if (disease == disease_list[[1]]) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    mycolor = mycolor_list[[disease]]

    mytitle = paste0(reg_name,' - ', toupper(disease),' Statistical Baseline Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1
    xlab = paste0(start_year,' - ', end_year)

    pl[[disease]] <- ggplot(data=total,
                                             mapping=aes(x=date))+
                                        geom_line(aes(y=`50%`),color=mycolor)+
                                        geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.4)+
                                        geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.7)+
                                        geom_point(aes(y=reported),color='black', alpha = 1., size = 0.5)+
                                        # geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", linewidth = 1.5) +
                                        labs(y=ylab,x=xlab)+
                                        theme(plot.title = element_text(hjust = 0.5, vjust = 0.8)) +
                                        annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)


  } #end of loop over diseases

  interactive_plot <- list()
  for (ip in 1:npath) {
    disease = disease_list[ip]
    interactive_plot[[disease]] <- ggplotly(pl[[disease]])
  }

  # interactive_plot <- pl
  cat("\nMaking Plots\n\n")


  if (npath == 2) {
    # suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
    arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], nrows = 1, titleX = TRUE, titleY = TRUE)

  } else {
    # suppressWarnings(pl[[1]])
    arrange_plot <- interactive_plot[[1]]

  }
  return(list(arrange_plot = arrange_plot, total_list= total_list))


}


shiny_plot_forecast <- function(prof_data, par_list, fit_list, ntraj =1000, nfrcst, err_cor, method_name="semi_sorted_randA") {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list = dates_frcst_list = total_list = list()

  forecast_traj = list()

  reported_list = reported_fit_list = list()

  # loop on all diseases

  wis_df = list()

  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    prof_init_par = par_list[[ip]]

    # location name
    reg_name = mydata$loc_name

    # location population
    pop = mydata$population

    # disease name (covid or flu)
    disease = mydata$disease

    # Number of values for FOI
    nb = nb_vec[ip]

    # dates - fitted
    dates_fit  = mydata$data_fit$date

    ndates = length(dates_fit)

    # using the date array build an integer day array

    times = dates_to_int(dates_fit)

    ntimes = length(times)

    cadence = as.numeric(dates_fit[2]-dates_fit[1])

    ntimes_frcst= ndates + nfrcst/cadence

    # build also the arrays for the forecasts

    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 week')
      #check that nfrcst is a product of seven
      if (nfrcst %% 7 != 0) {
        print("\nError: For weekly data nfrcst must be a prodcut of seven (e.g., 7, 14, 21..) \n")
        return()
      }
    }

    times_frcst = dates_to_int(dates_frcst)

    dates_frcst_list[[ip]] = dates_frcst

    # retrieve posterior for disease

    tab = tab_list[[disease]]

    nlines = dim(tab)[1]

    nparamtot = dim(tab)[2] -1 # it includes the LLK hence the -1

    nparam = nparamtot - 2*nb

    z <- round(nlines*2/3):nlines

    ind <- sample(z, round( 1.2* ntraj))

    ind <- order(tab[,'llk'])

    simdat <- array(0, c(ntraj, ntimes_frcst))

    # retrieve initial conditions for disease

    state0 = state0_list[[disease]]

    # set the model to 'sirh' or 'seirh' based on disease

    model = prof_init_par$model

    # observations - all data stream

    obs = mydata$data$inc

    dates = mydata$data$date

    # start date of obs may not be the same as start date of obs_fit

    keep_ind = which(dates >= dates_fit[1])

    # trim
    obs = obs[keep_ind]
    dates = dates[keep_ind]

    obs_fit = mydata$data_fit$inc
    #

    t0 = 0

    dt = 1./15.

    #print information to the User
    cat("\nCreating Forecast: ", nfrcst/cadence," ", print_lab, " Forward for ", reg_name, ' ', toupper(disease),'\n')

    if (model == 'sirh') {
      # Here using a time-dependent version
      # parnames_td_sirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2",
      #                      "pop","pH","S0",'I0',"R0", "rho","baseline")

      # pomp(data=data.frame(time = 1:ntimes_frcst),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.sirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
      #      paramnames=parnames_td_sirh) -> flu_sirh

      # pomp(data=data.frame(time = 1:ntimes_frcst),
      #      times="time",t0=0,
      #      skeleton = vectorfield(td.sirh.ode),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
      #      paramnames=parnames_td_sirh) -> flu_sirh

      state_names = c('S', 'I', 'R', 'H1', 'H2', 'Ih')
      nstates = length(state_names)

      accum = c('Ih')
      iaccum = which(state_names %in% accum)
      naccum = length(accum)

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]
        state0["I0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - state0$I0

        # coef(flu_sirh) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                     mypar['mu_H1H2'], mypar['pH'], mypar['pop'],
        #                     state0["I0"], state0["S0"], state0["R0"],
        #                     mypar['rho'], round(mypar['baseline']), wl = wl)
        # # generate simulation data with the parameters defined above
        # model.pred <- trajectory(flu_sirh, format="data.frame")
        # model.pred$cases <- rpois(ntimes_frcst, model.pred[,'Ih'] * mypar[['rho']] + mypar[['baseline']])


        # model.pred <- simulate(flu_sirh, format="data.frame", nsim = 1)

        # if (model.pred$cases[which.max(obs)] > round(mypar['baseline']) *2){

        yinit = c(state0$S0, state0$I0, 0, 0, 0, 0)

        traj = array(0, c(ntimes_frcst, nstates))
        out <- .Fortran('detsirh', nstates = as.integer(nstates), init = as.double(yinit), param = as.double(mypar),
                        nparam = as.integer(nparam), nb = as.integer(nb), time = as.double(times_frcst), ntimes = as.integer(ntimes_frcst),
                        t0 = as.double(t0), dt = as.double(dt),
                        naccum = as.integer(naccum), iaccum = as.integer(iaccum),
                        traj = as.double(traj), wl = as.double(wl))
        #
        traj = array(out$traj, c(ntimes_frcst, nstates))
        colnames(traj) <- state_names
        Ih = traj[,'Ih']
        #
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])


        # parms = c(mypar, 'wl' =wl)
        # time0 = parms['time0']
        # results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'lsoda', func=td_sirh_dynamics, parms = parms)
        # results0 <- results0[,-1]
        # yinit0 <- as.numeric(results0[nrow(results0),])
        # if (nb == 2) {
        #   results <- ode(y=yinit0, t = times_frcst, method='lsoda', func=td2_sirh_dynamics, parms = parms)
        # } else {
        #   results <- ode(y=yinit0, t = times_frcst, method='lsoda', func=td3_sirh_dynamics, parms = parms)
        #   # calling H2 Ih here
        # }
        #
        # model.pred = results[,-1] # remove the time column
        # colnames(model.pred) = c('S', 'I', 'R', 'H1', 'H2', 'Ih')
        #
        # # generate simulation data with the parameters defined above
        #
        # Ih = c(0, diff(model.pred[,'Ih']))
        # cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        if (cases[which.max(obs)] > round(mypar['baseline'])) {
          icount = icount + 1
          simdat[icount,] <- cases
         }

        if (icount == ntraj) break

       }

    } else {

      # parnames_td_seirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2","mu_EI", "pop","pH","S0","E0",'I0',"R0", "rho","baseline")
      #
      # pomp(data=data.frame(time = 1:ntimes_frcst),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.seirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init.seirh,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","E","I","R","H1","H2","Ic","Ih", 'time'),
      #      paramnames=parnames_td_seirh) -> covid_seir

      state_names = c('S', 'E', 'I', 'R', 'H1', 'H2', 'Ih')
      nstates = length(state_names)

      accum = c('Ih')
      iaccum = which(state_names %in% accum)
      naccum = length(accum)

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]

        state0["I0"] = mypar['I0']
        state0["E0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - (state0$I0+state0$E0)

        # coef(covid_seir) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                       mypar['mu_H1H2'], mypar['mu_EI'], mypar['pH'], mypar['pop'],
        #                       state0['I0'], state0['S0'], state0['E0'], state0['R0'],
        #                       mypar['rho'], round(mypar['baseline']), wl = wl)
        #
        # # generate simulation data with the parameters defined above
        #
        # model.pred <- simulate(covid_seir, format="data.frame", nsim = 1)

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0, 0)

        traj = array(0, c(ntimes_frcst, nstates))
        out <- .Fortran('detseirh', nstates = as.integer(nstates), init = as.double(yinit), param = as.double(mypar),
                        nparam = as.integer(nparam), nb = as.integer(nb), time = as.double(times_frcst), ntimes = as.integer(ntimes_frcst),
                        t0 = as.double(t0), dt = as.double(dt),
                        naccum = as.integer(naccum), iaccum = as.integer(iaccum),
                        traj = as.double(traj), wl = as.double(wl))
        #
        traj = array(out$traj, c(ntimes_frcst, nstates))
        colnames(traj) <- state_names
        Ih = traj[,'Ih']
        #
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        # parms = c(mypar, 'wl' =wl)
        # time0 = parms['time0']
        # results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'lsoda', func=td_seirh_dynamics, parms = parms)
        # results0 <- results0[,-1]
        # yinit0 <- as.numeric(results0[nrow(results0),])
        # if (nb == 2) {
        #   results <- ode(y=yinit0, t = times_frcst, method='lsoda', func=td2_seirh_dynamics, parms = parms)
        # } else {
        #   results <- ode(y=yinit0, t = times_frcst, method='lsoda', func=td3_seirh_dynamics, parms = parms)
        #   # calling H2 Ih here
        # }
        #
        # model.pred = results[,-1] # remove the time column
        # colnames(model.pred) = c('S', 'E', 'I', 'R', 'H1', 'H2', 'Ih')
        #
        # # generate simulation data with the parameters defined above
        #
        # Ih = c(0, diff(model.pred[,'Ih']))
        #
        # cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        if (max(cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- cases

        }

        if (icount == ntraj) break

      }

    }

    simdat = simdat[1:icount,]

    last_non_na_index <- max(which(!is.na(obs_fit)))
    if (cadence == 7) {
      obs_mean = obs_fit[last_non_na_index]
      obs_model = mean(simdat[,last_non_na_index])
    } else {
      obs_mean = mean(obs_fit[(last_non_na_index-7+1):last_non_na_index])
      obs_model = mean(simdat[,(last_non_na_index-7+1):last_non_na_index])
    }

    shift = obs_mean - obs_model

    for (ii in 1:icount) simdat[ii, ] = pmax(simdat[ii, ] + shift,0)

    simdat_list[[ip]] = simdat

    # we can score the forecast if the data overlaps the forecast
    #

    if (length(dates) > length(dates_fit)) {
      dates_frcst_only = anti_join(data.frame(date=dates_frcst), data.frame(date=dates_fit), by = "date")$date #Forecast only dates
      # which of the forecast only dates are also in the observation dates
      dates_frcst_only_in_obs = semi_join(data.frame(date=dates_frcst_only), data.frame(date=dates), by = "date")$date
      keep_ind_obs = match(dates_frcst_only_in_obs, dates)
      keep_ind_model = match(dates_frcst_only_in_obs, dates_frcst)
      nscore = length(dates_frcst_only_in_obs)
      obs_score = obs[keep_ind_obs]
      dates_score = dates[keep_ind_obs]

      # find the subset from the model that we are going to score
      sim_score = simdat[, keep_ind_model]

      wis_arr = score_forecast(obs = obs_score, simdat = sim_score)

      wis_df[[disease]] = data.frame(date=dates_score, wis = wis_arr, disease = disease, model = 'mech')

    }

    npad = nfrcst/cadence - length(obs)
    if (npad > 0) {
      reported = c(obs, rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst/cadence - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit, rep(NA, npad_fit))
    } else {
      reported_fit = obs_fit[1:length(dates_frcst)]
    }

    forecast_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates_frcst, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = times_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    total_list[[disease]] = total

    reported_list[[disease]] = reported
    reported_fit_list[[disease]] = reported_fit

    copy_total = total

    total[1:length(obs_fit), c('2.5%','25%','50%','75%','97.5%')] <- NA

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    mytitle = paste0(reg_name,' - ', toupper(disease))

    # y-label only on left most plot
    if (disease == disease_list[[1]]) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    mycolor = mycolor_list_with_transparency[[disease]]
    mytitle = paste0(reg_name,' - ', toupper(disease), ' Mechanistic Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1

    xlab = ''

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, max(total[,c("reported","97.5%")]))  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[disease]] <- ggplot(data=total,aes(x=date))+
      # geom_col(aes(y=reported_fit), fill = mycolor, alpha = 1.) +
      geom_col(aes(y=reported), fill = mycolor, alpha = 1.) +
      # geom_col(aes(y=reported), stat = "identity", fill = default_colors[(ip)], alpha = 0.4) +
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8)+
      geom_line(aes(y=`50%`),color='black')+
      # geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total$date), y = 0.93*max(total[,c('reported',"97.5%")], na.rm=TRUE), label = mytitle, size = 4)


  } #end of loop over diseases

  interactive_plot <- list()

  if (length(wis_df) != 0) {
    long_df = bind_rows(wis_df)
  } else {
    long_df = NULL
  }


  # if only a single pathogen was selected - create plot and return

  if (npath == 1) {
    interactive_plot[[1]] <- ggplotly(pl[[1]])
    arrange_plot <- interactive_plot[[1]]
    return(list(arrange_plot = arrange_plot, total_list = total_list, wis_df = long_df))
  }

  # If more than one pathogen

  # Combine forecasts
  cat("\nCombining Forecasts \n")

  # combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

  combined_frcst_ecor <- combine_fore_err_corr(prof_data, dates_frcst_list, simdat_list,
                                               err_corr=err_cor, nfrcst=nfrcst/cadence,
                                               method_name=method_name)
  combined_frcst_rand <- combine_fore_err_corr(prof_data,
                                               dates_frcst_list, simdat_list,
                                               err_corr=0, nfrcst=nfrcst/cadence,
                                               method_name=method_name)

  combined_frcst = combined_frcst_ecor
  combined_frcst$simdat_both[[2]] = combined_frcst_rand$simdat_both[[1]]

  combined_names <- c("err_cor", "random")

  names(combined_frcst$simdat_both) = combined_names

  obs_each_list = combined_frcst$obs_each_list

  obs_fit_each_list = combined_frcst$obs_fit_each_list

  simdat_both = combined_frcst$simdat_both

  dates_both_data  = combined_frcst$dates_both_data
  dates_fore = combined_frcst$dates_fore

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  # npad = length(dates_both) - length(obs_both)
  npad = length(dates_fore)
  if (npad > 0) {
    reported_both = c(obs_both, rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_both_data)]
  }

  npad_fit = length(dates_both_data)

  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both, rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_both_data)]
  }

  # find maximum in simdat_both of random and sorted and use

  both_max = 0.0
  for (ip in 1:length(combined_names)) {
    both_max = max(both_max, round(max(simdat_both[[ip]])))
    both_max = max(both_max, reported_both, na.rm = TRUE)
  }

  # create a long data-frame with the reported values for both pathogens
  data_df_list = list()
  plot_dates = c(dates_both_data, dates_fore)

  for (ip in 1:npath) {
    obs_each = obs_each_list[[ip]]
    if (length(obs_each) > length(plot_dates)) {
      reported = obs_each[1:length(plot_dates)]
    } else {
      reported = c(obs_each, rep(NA, length(plot_dates)-length(obs_each_list[[1]])))
    }
    data_df_list[[ip]] = data.frame(
      # date = as.Date(dates_both, format = '%Y-%m-%d'),
      date = plot_dates,
      disease = rep(disease_list[[ip]], length(plot_dates)),
      reported = reported
    )
  }
  data_df = rbind(data_df_list[[1]], data_df_list[[2]])

  for (ic in 1:length(combined_names)) {

    apply(simdat_both[[ic]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)
    quantiles_pad = as.data.frame(matrix(data=NA, nrow=npad_fit,
                                         ncol=ncol(quantiles_both))
    )
    names(quantiles_pad) = names(quantiles_both)
    quantiles_both = rbind(quantiles_pad, quantiles_both)

    forecast_traj[[combined_names[[ic]]]] = list(traj = simdat_both[[ic]],
                                                 date = plot_dates,
                                                 reported_both = reported_both[1:length(plot_dates)],
                                                 reported_fit_both = reported_fit_both)

    total_both=cbind(date = plot_dates,
                     quantiles_both,
                     reported = reported_both[1:length(plot_dates)],
                     reported_fit = reported_fit_both)

    total_list[[combined_names[[ic]]]] = total_both

    # copy_total_both = total_both
    # total_both[1:length(obs_fit_both), c('2.5%', '25%', '50%', '75%', '97.5%')] <- NA

    if (combined_names[ic]=="err_cor") {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=', err_cor, ')')
    } else {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=0.0)')
    }


    # y-label only on left most plot
    if (ic == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor = mycolor_list_with_transparency[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[combined_names[ic]]] <- ggplot(data=total_both,
                                       mapping=aes(x=date))+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5) +
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8) +
      geom_line(aes(y=`50%`),color='black') +
      geom_col(data = data_df, aes(x = date, y=reported, fill = disease), alpha = 0.5, inherit.aes = FALSE) +
      coord_cartesian(ylim=c(0, both_max)) +
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total_both$date), y = 0.93*both_max, label = mytitle, size = 4)

  }

  for (ip in 1:length(pl)) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")

  arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], interactive_plot[[3]], interactive_plot[[4]],
                          nrows = 2, titleX = TRUE, titleY = TRUE, shareX = FALSE, shareY = FALSE, margin = c(0.02, 0.02, 0.07, 0.07))
  return(list(arrange_plot = arrange_plot, total_list = total_list, wis_df = long_df))

}


shiny_plot_stat_forecast <- function(prof_data, diseases, nfrcst, err_cor, method_name="semi_sorted_randA") {

  ntraj = 1000

  if (is.null(nfrcst)) nfrcst = 28

  npath = length(diseases)

  disease_list = diseases

  pl = simdat_list = dates_frcst_list = total_list = list()

  forecast_traj = list()

  reported_list = reported_fit_list = list()

  wis_df = list()

  # loop on all diseases
  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    # hospitalization incidence - fitted
    inc = mydata$data_fit_stat$inc


    # dates - fitted
    dates_fit  = mydata$data_fit_stat$date

    ndates = length(dates_fit)

    # using the date array build an integer day array

    times = dates_to_int(dates_fit)

    ntimes = length(times)

    cadence = as.numeric(dates_fit[2]-dates_fit[1])

    ntimes_frcst= ndates + nfrcst/cadence

    # build also the arrays for the forecasts

    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 week')
    }

    times_frcst = dates_to_int(dates_frcst)

    dates_frcst_list[[ip]] = dates_frcst

    # observations - all data stream

    obs = mydata$data$inc

    # obs may not have the same start date as obs_fit hence need to trim
    keep_ind = which(mydata$data$date >= dates_fit[1])

    obs = obs[keep_ind]
    dates = mydata$data$date[keep_ind]

    obs_fit = mydata$data_fit_stat$inc

    cat('\nCreating ',toupper(disease),' Statistical Forecast for ', reg_name,' ', nfrcst,' Days Forward\n\n')

    simdat = stat_forecast(mydata, ntraj, nfrcst/cadence)

    simdat_list[[ip]] = simdat

    # we can score the forecast if the data overlaps the forecast
    #

    if (length(dates) > length(dates_fit)) {
      dates_frcst_only = anti_join(data.frame(date=dates_frcst), data.frame(date=dates_fit), by = "date")$date #Forecast only dates
      # which of the forecast only dates are also in the observation dates
      dates_frcst_only_in_obs = semi_join(data.frame(date=dates_frcst_only), data.frame(date=dates), by = "date")$date
      keep_ind_obs = match(dates_frcst_only_in_obs, dates)
      keep_ind_model = match(dates_frcst_only_in_obs, dates_frcst)
      nscore = length(dates_frcst_only_in_obs)
      obs_score = obs[keep_ind_obs]
      dates_score = dates[keep_ind_obs]

      # find the subset from the model that we are going to score
      sim_score = simdat[, keep_ind_model]

      wis_arr = score_forecast(obs = obs_score, simdat = sim_score)

      wis_df[[disease]] = data.frame(date=dates_score, wis = wis_arr, disease = disease, model = 'stat')

    }

    npad = nfrcst/cadence - length(obs)
    if (npad > 0) {
      reported = c(obs, rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst/cadence - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit, rep(NA, npad_fit))
    } else {
      reported_fit = obs_fit[1:length(dates_frcst)]
    }

    forecast_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates_frcst, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = times_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    total_list[[disease]] = total

    total = as.data.frame(total)

    reported_list[[disease]] = reported
    reported_fit_list[[disease]] = reported_fit

    copy_total = total

    total[1:length(obs_fit), c('2.5%','25%','50%','75%','97.5%')] <- NA

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    # y-label only on left most plot
    if (ip == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
      #xlab = paste0(start_year,' - ', end_year)
    } else {
      ylab = ''
    }
    xlab = ''

    mycolor = mycolor_list_with_transparency[[disease]]

    mytitle = paste0(reg_name,' - ', toupper(disease), ' Statistical Baseline Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, max(total[,"97.5%"]))  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[disease]] <- ggplot(data=total,aes(x=date))+
                                        geom_col(aes(y=reported), fill = mycolor, alpha = 1.) +
                                        geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5)+
                                        geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8)+
                                        geom_line(aes(y=`50%`),color='black')+
                                        labs(y=ylab,x=xlab) +
                                        theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
                                        annotate("text", x = median(total$date), y = 0.93*max(total[,c('reported',"97.5%")], na.rm=TRUE), label = mytitle, size = 4)



  } #end of loop over diseases


  if (length(wis_df) !=0) {
    long_df = bind_rows(wis_df)
  } else {
    long_df = NULL
  }


  if (npath == 1) {
    interactive_plot <- list()
    interactive_plot[[1]] <- ggplotly(pl[[1]])
    arrange_plot <- interactive_plot[[1]]
    return(list(arrange_plot = arrange_plot, total_list = total_list, wis_df = long_df))
  }


  # Combine forecasts
  cat("Combining Forecasts \n")

  combined_frcst_ecor <- combine_fore_err_corr(prof_data, dates_frcst_list, simdat_list,
                                               err_corr=err_cor, nfrcst=nfrcst/cadence,
                                               method_name=method_name)
  combined_frcst_rand <- combine_fore_err_corr(prof_data,
                                               dates_frcst_list, simdat_list,
                                               err_corr=0, nfrcst=nfrcst/cadence,
                                               method_name=method_name)


  combined_frcst = combined_frcst_ecor
  combined_frcst$simdat_both[[2]] = combined_frcst_rand$simdat_both[[1]]

  combined_names <- c("err_cor", "random")
  names(combined_frcst$simdat_both) = combined_names

  # combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

  obs_each_list = combined_frcst$obs_each_list

  obs_fit_each_list = combined_frcst$obs_fit_each_list

  simdat_both = combined_frcst$simdat_both

  dates_both_data  = combined_frcst$dates_both_data
  dates_fore = combined_frcst$dates_fore

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  # npad = nfrcst/cadence - length(obs_both)
  npad = length(dates_fore)

  if (npad > 0) {
    reported_both = c(obs_both, rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_both_data)]
  }

  npad_fit = length(dates_both_data)

  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both, rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_both_data)]
  }


  # combined_names <- c('random', 'sorted')

  # find maximum in simdat_both of random and sorted and use

  both_max = 0.0
  for (ip in 1:length(combined_names)) {
    both_max = max(both_max, round(max(simdat_both[[ip]])))
    both_max = max(both_max, reported_both, na.rm = TRUE)
  }

  # create a long data-frame with the reported values for both pathogens
  data_df_list = list()
  plot_dates = c(dates_both_data, dates_fore)

  for (ip in 1:npath) {
    obs_each = obs_each_list[[ip]]
    if (length(obs_each) > length(plot_dates)) {
      reported = obs_each[1:length(plot_dates)]
    } else {
      reported = c(obs_each, rep(NA, length(plot_dates)-length(obs_each_list[[1]])))
    }
    data_df_list[[ip]] = data.frame(
      # date = as.Date(dates_both, format = '%Y-%m-%d'),
      date = plot_dates,
      disease = rep(disease_list[[ip]], length(plot_dates)),
      reported = reported
    )
  }

  data_df = rbind(data_df_list[[1]], data_df_list[[2]])

  for (ic in 1:length(combined_names)) {

    apply(simdat_both[[ic]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)
    quantiles_pad = as.data.frame(matrix(data=NA, nrow=npad_fit,
                                         ncol=ncol(quantiles_both))
    )
    names(quantiles_pad) = names(quantiles_both)
    quantiles_both = rbind(quantiles_pad, quantiles_both)

    forecast_traj[[combined_names[[ic]]]] = list(traj = simdat_both[[ic]],
                                                 date = plot_dates,
                                                 reported_both = reported_both[1:length(plot_dates)],
                                                 reported_fit_both = reported_fit_both)

    total_both=cbind(date = plot_dates,
                     quantiles_both,
                     reported = reported_both[1:length(plot_dates)],
                     # reported = c(obs_both, rep(NA, npad)),
                     reported_fit = reported_fit_both)
                     # reported_fit = c(obs_fit_both, rep(NA, npad)))

    total_list[[combined_names[[ic]]]] = total_both

    # apply(simdat_both[[ic]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both
    #
    # quantiles_both <- t(quantiles_both)
    # quantiles_both <- as.data.frame(quantiles_both)
    #
    # forecast_traj[[combined_names[[ic]]]] = list(traj = simdat_both[[ic]],
    #                                              date = as.Date(dates_both, format = '%Y-%m-%d'),
    #                                              reported_both = reported_both,
    #                                              reported_fit_both = reported_fit_both)
    #
    # total_both=cbind(date = as.Date(dates_both, format = '%Y-%m-%d'),quantiles_both,
    #                  reported = c(obs_both, rep(NA, length(dates_both)-length(obs_both))),
    #                  reported_fit = c(obs_fit_both, rep(NA, length(dates_both)-length(obs_fit_both))))
    #
    # total_list[[combined_names[ic]]] = total_both
    #
    # copy_total_both = total_both
    # total_both[1:length(obs_fit_both), c('2.5%', '25%', '50%', '75%', '97.5%')] <- NA

    if (combined_names[ic]=="err_cor") {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=', err_cor, ')')
    } else {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=0.0)')
    }

    # y-label only on left most plot
    if (ic == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor =mycolor_list_with_transparency[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )


    pl[[combined_names[ic]]] <- ggplot(data=data_df,
                                       mapping=aes(x=date))+
      geom_col(aes(y=reported, fill = disease, alpha = 0.5), alpha = 0.5) +
      geom_ribbon(data=total_both, aes(x=date, ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5, inherit.aes = FALSE) +
      geom_ribbon(data=total_both, aes(x=date, ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8, inherit.aes = FALSE) +
      geom_line(data=total_both, aes(x= date, y=`50%`),color='black',  inherit.aes = FALSE) +
      coord_cartesian(ylim=c(0, both_max)) +
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total_both$date), y = 0.93*both_max, label = mytitle, size = 4)


  }

  cat("\nMaking Plots\n\n")

  interactive_plot <- list()

  for (ip in 1:length(pl)) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }


  arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], interactive_plot[[3]], interactive_plot[[4]],
                            nrows = 2, titleX = TRUE, titleY = TRUE, shareX = FALSE, shareY = FALSE, margin = c(0.02, 0.02, 0.07, 0.07))
  return(list(arrange_plot = arrange_plot, total_list = total_list, wis_df = long_df))

}

shiny_plot_wis <- function(wis_data, loc = NA) {

  wis_name = names(wis_data)
  nwis = length(wis_name)

  if (nwis == 1){
    wis_df = wis_data[[wis_name[1]]]
  } else {
    wis_df = bind_rows(wis_data)
  }

  pl = list()

  mydiseases= unique(wis_df$disease)

  wis_df$wis = round(wis_df$wis, 2)


  for (jj in 1:length(mydiseases)) {
    mydisease = mydiseases[jj]
    data = subset(wis_df, disease == mydisease)
    title = paste0(toupper(loc), ' WIS SCORE ')
    pl[[jj]] <- ggplot(data, aes(x = date, y = wis, fill = model)) +
      geom_bar(stat = "identity", position = "dodge") +
      annotate("text", x = data$date[2], y = max(data$wis)*0.9, label = toupper(mydisease), size = 5) +
      labs(title = title,
           x = "Date",
           y = "WIS Score",
           fill = "Model")

  }

  if (length(pl) == 1) {
    interactive_plot <- list()
    interactive_plot[[1]] <- ggplotly(pl[[1]])
    arrange_plot <- interactive_plot[[1]]
    return(list(arrange_plot = arrange_plot, wis_df = wis_df))
  }

  if (length(pl) == 2)
    arrange_plot <- subplot(pl[[1]], pl[[2]],
                            nrows = 2, titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = FALSE, margin = c(0.02, 0.02, 0.1, 0.07))
  # Render the plot

  return(list(arrange_plot = arrange_plot, wis_df = wis_df))
}


