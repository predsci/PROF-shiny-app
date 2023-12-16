library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(PROF)
library(pomp)

shiny_plot_fit <- function(prof_data, par_list, fit_list, ntraj =1000) {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = list()

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

        yinit = c(state0$S0, state0$I0, 0, 0, 0)
        parms = c(mypar, 'wl' = wl)


        if (nb == 2) {
          results <- ode(y=yinit, t = times, method='rk4', func=td2_sirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = times, method='rk4', func=td3_sirh_dynamics, parms = parms)
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,4]
        cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])
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

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0)
        parms = c(mypar, 'wl' = wl)

        if (nb == 2) {
          results <- ode(y=yinit, t = times, method='rk4', func=td2_seirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = times, method='rk4', func=td3_seirh_dynamics, parms = parms)
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,5]
        cases <- rpois(ntimes, Ih * mypar[['rho']] + mypar[['baseline']])

        if (max(cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- cases
        }

        if (icount == ntraj) break

      }

      simdat = simdat[1:icount,]

    }

     apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = obs)

    total = as.data.frame(total)

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

    # pl[[disease]] <- ggplot(data=total,
    #                         mapping=aes(x=date))+
    #   geom_line(aes(y=`50%`),color='red')+
    #   geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
    #   geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='red',alpha=0.4)+
    #   geom_point(aes(y=reported),color='black')+
    #   labs(y=ylab,x=xlab) + ggtitle(title)


    pl[[disease]] <- ggplot(data=total,
                            mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color=mycolor)+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.3)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.5)+
      geom_point(aes(y=reported),color='black', alpha = 1., size = 0.5)+
      geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", linewidth = 1.5) +
      labs(y=ylab,x=xlab)+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.8)) +
      annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)

  } #end of loop over diseases

  # cat("\nMaking Plots\n\n")
  #
  # if (npath == 2) {
  #   suppressWarnings(print(grid.arrange(pl[[1]],  pl[[2]], ncol = 2)))
  #   } else {
  #   suppressWarnings(pl[[1]])
  #
  #   }

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
  return(arrange_plot)


}


shiny_plot_stat_fit <- function(prof_data, diseases) {

  ntraj = 1000

  npath = length(diseases)

  disease_list = diseases

  pl = list()

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

    # observations - all data stream

    obs = mydata$data$inc

    obs_fit = mydata$data_fit_stat$inc

    cat('\nCreating ',toupper(disease),' Statistical Fit for ', reg_name,'\n\n')

    simdat = stat_fit(obs_fit, ntraj)

    reported = obs

    reported_fit = obs_fit

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)

    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = reported_fit)

    total = as.data.frame(total)

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
                                        geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.3)+
                                        geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.5)+
                                        geom_point(aes(y=reported),color='black', alpha = 1., size = 0.5)+
                                        geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", linewidth = 1.5) +
                                        labs(y=ylab,x=xlab)+
                                        theme(plot.title = element_text(hjust = 0.5, vjust = 0.8)) +
                                        annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)



    # # Create the Plotly plot using plot_ly
    # custom_marker_size <- 4
    # pl[[disease]] <- plot_ly(data = total) %>%
    #   add_lines(x = ~date, y = ~`50%`, color = mycolor, showlegend = FALSE) %>%
    #   add_ribbons(x = ~date, ymin = ~`2.5%`, ymax = ~`97.5%`, fill = mycolor, alpha = 0.2, line = list(color = "transparent"), showlegend = FALSE) %>%
    #   add_ribbons(x = ~date, ymin = ~`25%`, ymax = ~`75%`, fill = mycolor, alpha = 0.4, line = list(color = "transparent"), showlegend = FALSE) %>%
    #   add_markers(x = ~date, y = ~reported_fit, color = I("black"), alpha = 1,size = custom_marker_size, sizemode = "diameter", sizeref = 0.02, showlegend = FALSE) %>%
    #   # add_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) %>%
    #   layout(yaxis = list(title = ylab), xaxis = list(title = xlab), title = mytitle, hovermode = "x unified")
    #
    # pl[[disease]] <- pl[[disease]] %>%
    #   layout(geom_vline(xintercept = dates[ntimes], line = list(color = "cornflowerblue", dash = "dash", width = 1.5)))


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
  return(arrange_plot)

  # cat("\nMaking Plots\n\n")
  # if (npath == 2) {
  #   suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
  # } else {
  #   suppressWarnings(pl[[1]])
  # }

}


shiny_plot_forecast <- function(prof_data, par_list, fit_list, ntraj =1000, nfrcst = 35) {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list = dates_frcst_list = list()

  forecast_traj = list()

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

    # Number of values for FOI
    nb = nb_vec[ip]

    # dates - fitted
    dates  = mydata$data_fit$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    ntimes_frcst= ndates + nfrcst

    # build also the arrays for the forecasts

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 week')
    }


    dates_frcst_list[[ip]] = dates_frcst

    # retrieve posterior for disease

    tab = tab_list[[disease]]

    nlines = dim(tab)[1]

    nparamtot = dim(tab)[2] -1 # it includes the LLK hence the -1

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

    obs_fit = mydata$data_fit$inc

    #print information to the User
    cat("\nCreating Forecast: ", nfrcst," ", print_lab, " Forward for ", reg_name, ' ', toupper(disease),'\n')

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

        yinit = c(state0$S0, state0$I0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)

        if (nb == 2) {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='rk4', func=td2_sirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='rk4', func=td3_sirh_dynamics, parms = parms)
          # calling H2 Ih here
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,4]
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

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

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)

        if (nb == 2) {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='rk4', func=td2_seirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='rk4', func=td3_seirh_dynamics, parms = parms)
          # calling H2 Ih here
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,5]
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        if (max(cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- cases

        }

        if (icount == ntraj) break

      }

    }

    simdat = simdat[1:icount,]

    simdat_list[[ip]] = simdat

    npad = nfrcst - length(obs)
    if (npad > 0) {
      reported = c(obs[1:length(dates_frcst)], rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit[1:length(dates_frcst)], rep(NA, npad_fit))
    } else {
      reported_fit = obs_fit[1:length(dates_frcst)]
    }

    forecast_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates_frcst, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = 1:ntimes_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    mytitle = paste0(reg_name,' - ', toupper(disease))

    # y-label only on left most plot
    if (disease == disease_list[[1]]) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    mycolor = mycolor_list[[disease]]
    mytitle = paste0(reg_name,' - ', toupper(disease), ' Mechanistic Model')


    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])

    xlab = ''
    # if (npath == 1) {
    #   xlab = paste0(start_year,' - ', end_year)
    # } else {
    #   xlab = ''
    # }

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, max(total[,"97.5%"]))  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[disease]] <- ggplot(data=total,
                            mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color=mycolor)+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.2)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.4)+
      geom_point(aes(y=reported),color='black', alpha = 0.5, size = 0.5)+
      geom_point(aes(y=reported_fit),color='black', alpha = 1., size = 0.5)+
      geom_line(data = vertical_line, aes(x = x, y = y), color = 'grey30', linetype = 'dashed') +
      # geom_vline(xintercept = total$date[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
      labs(y=ylab,x=xlab) + #,title=mytitle)+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7)) +
      annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)

  } #end of loop over diseases


  # Combine forecasts
  cat("Combining Forecasts \n")

  combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

  simdat_both = combined_frcst$simdat_both

  dates_both  = combined_frcst$dates_both

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  npad = nfrcst - length(obs_both)
  if (npad > 0) {
    reported_both = c(obs_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_frcst)]
  }

  npad_fit = nfrcst - length(obs_fit_both)
  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_frcst)]
  }

  combined_names <- c('random', 'sorted')

  # find maximum in simdat_both of random and sorted and use

  both_max = 0.0
  for (ip in 1:npath) {
    both_max = max(both_max, round(max(simdat_both[[ip]])))
  }

  for (ip in 1:npath) {

    apply(simdat_both[[ip]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)

    forecast_traj[[combined_names[[ip]]]] = list(traj = simdat_both[[ip]],
                                                 date = as.Date(dates_both, format = '%Y-%m-%d'),
                                                 reported_both = reported_both,
                                                 reported_fit_both = reported_fit_both)

    total_both=cbind(date = as.Date(dates_both, format = '%Y-%m-%d'),quantiles_both,
                     reported = c(obs_both, rep(NA, length(dates_both)-length(obs_both))),
                     reported_fit = c(obs_fit_both, rep(NA, length(dates_both)-length(obs_fit_both))))

    mytitle = paste0(reg_name,' - Combined Burden (', combined_names[ip],')')


    # y-label only on left most plot
    if (ip == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor = mycolor_list[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[combined_names[ip]]] <- ggplot(data=total_both,
                                       mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color=mycolor)+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.2)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.4)+
      geom_point(aes(y=reported),color='black', alpha = 0.5,size=0.5)+
      geom_point(aes(y=reported_fit),color='black', alpha = 1.,size=0.5)+
      geom_line(data = vertical_line, aes(x = x, y = y), color = 'grey30', linetype = 'dashed') +
      # geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
      coord_cartesian(ylim=c(0, both_max))+
      labs(y=ylab,x=xlab)+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7)) +
      annotate("text", x = median(total_both$date), y = both_max, label = mytitle, size = 4)

  }

  interactive_plot <- list()

  for (ip in 1:length(pl)) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")

  arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], interactive_plot[[3]], interactive_plot[[4]],
                          nrows = 2, titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = FALSE)
  return(arrange_plot)

}


shiny_plot_stat_forecast <- function(prof_data, diseases, nfrcst) {

  ntraj = 1000

  npath = length(diseases)

  disease_list = diseases

  pl = simdat_list = dates_frcst_list = list()

  forecast_traj = list()

  # loop on all diseases
  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    # hospitalization incidence - fitted
    inc = mydata$data_fit_stat$inc

    # dates - fitted
    dates  = mydata$data_fit_stat$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    ntimes_frcst= ndates + nfrcst

    # build also the arrays for the forecasts

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 week')
    }


    dates_frcst_list[[ip]] = dates_frcst

    # observations - all data stream

    obs = mydata$data$inc

    obs_fit = mydata$data_fit_stat$inc

    cat('\nCreating ',toupper(disease),' Statistical Forecast for ', reg_name,' ', nfrcst,' Days Forward\n\n')

    simdat = stat_forecast(mydata, ntraj, nfrcst)

    simdat_list[[ip]] = simdat

    npad = nfrcst - length(obs)
    if (npad > 0) {
      reported = c(obs[1:length(dates_frcst)], rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit[1:length(dates_frcst)], rep(NA, npad_fit))
    } else {
      reported_fit = obs_fit[1:length(dates_frcst)]
    }

    forecast_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates_frcst, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = 1:ntimes_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

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

    mycolor = mycolor_list[[disease]]

    mytitle = paste0(reg_name,' - ', toupper(disease), ' Statistical Baseline Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, max(total[,"97.5%"]))  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[disease]] <- ggplot(data=total,
                                             mapping=aes(x=date))+
                                        geom_line(aes(y=`50%`),color=mycolor)+
                                        geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.2)+
                                        geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.4)+
                                        geom_point(aes(y=reported),color='black', alpha = 0.5, size = 0.5)+
                                        geom_point(aes(y=reported_fit),color='black', alpha = 1., size = 0.5)+
                                        geom_line(data = vertical_line, aes(x = x, y = y), color = 'grey30', linetype = 'dashed') +
                                        # geom_vline(xintercept = total$date[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
                                        labs(y=ylab,x=xlab) + #,title=mytitle)+
                                        theme(plot.title = element_text(hjust = 0.5, vjust = 0.7)) +
                                        annotate("text", x = median(total$date), y = max(total[,"97.5%"]), label = mytitle, size = 4)

  } #end of loop over diseases


  if (npath == 1) {
    interactive_plot <- list()
    interactive_plot[[1]] <- ggplotly(pl[[1]])
    arrange_plot <- interactive_plot[[1]]
    return(arrange_plot)
  }


  # Combine forecasts
  cat("Combining Forecasts \n")

  combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

  simdat_both = combined_frcst$simdat_both

  dates_both  = combined_frcst$dates_both

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  npad = nfrcst - length(obs_both)
  if (npad > 0) {
    reported_both = c(obs_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_frcst)]
  }

  npad_fit = nfrcst - length(obs_fit_both)
  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_frcst)]
  }

  combined_names <- c('random', 'sorted')

  # find maximum in simdat_both of random and sorted and use

  both_max = 0.0
  for (ip in 1:length(combined_names)) {
    both_max = max(both_max, round(max(simdat_both[[ip]])))
  }

  for (ip in 1:length(combined_names)) {

    apply(simdat_both[[ip]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)

    forecast_traj[[combined_names[[ip]]]] = list(traj = simdat_both[[ip]],
                                                 date = as.Date(dates_both, format = '%Y-%m-%d'),
                                                 reported_both = reported_both,
                                                 reported_fit_both = reported_fit_both)

    total_both=cbind(date = as.Date(dates_both, format = '%Y-%m-%d'),quantiles_both,
                     reported = c(obs_both, rep(NA, length(dates_both)-length(obs_both))),
                     reported_fit = c(obs_fit_both, rep(NA, length(dates_both)-length(obs_fit_both))))

    mytitle = paste0(reg_name,' - Combined Burden (', combined_names[ip],')')

    # y-label only on left most plot
    if (ip == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor = mycolor_list[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[combined_names[ip]]] <- ggplot(data=total_both,
                                                        mapping=aes(x=date))+
                                                   geom_line(aes(y=`50%`),color=mycolor)+
                                                   geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill=mycolor,alpha=0.2)+
                                                   geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill=mycolor,alpha=0.4)+
                                                   geom_point(aes(y=reported),color='black', alpha = 0.5,size=0.5)+
                                                   geom_point(aes(y=reported_fit),color='black', alpha = 1.,size=0.5)+
                                                   geom_line(data = vertical_line, aes(x = x, y = y), color = 'grey30', linetype = 'dashed') +
                                                   # geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
                                                   coord_cartesian(ylim=c(0, both_max))+
                                                   labs(y=ylab,x=xlab)+
                                                    theme(plot.title = element_text(hjust = 0.5, vjust = 0.7)) +
                                                    annotate("text", x = median(total_both$date), y = both_max, label = mytitle, size = 4)

  }

  cat("\nMaking Plots\n\n")
  # if (npath == 2) {
  #   suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2)))
  # } else {
  #   suppressWarnings(pl[[1]])
  # }



  interactive_plot <- list()

  for (ip in 1:length(pl)) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")

  arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], interactive_plot[[3]], interactive_plot[[4]],
                            nrows = 2, titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = FALSE)
  return(arrange_plot)

}


