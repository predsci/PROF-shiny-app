# Define server

server <- function(input, output, session) {

  shinyjs::onclick("incidence_img",  updateTabsetPanel(session, inputId="navbar", selected= "1. Explore Incidence"))

  shared_data <- reactiveValues(data = NULL)

  shared_fit <- reactiveValues(data = NULL)

  shared_par <- reactiveValues(data = NULL)

  dates_data <- reactiveValues(data = NULL)

  shared_wis  <- reactiveValues(data = NULL)

  download_trigger <- reactiveValues(num=0)

  # Observe button click for Tab 1
  observeEvent(input$plotDataButton, {

    shinyjs::html("loading_message_1b","<strong> Loading Data..Please Wait</strong>")

    # download HHS hospitalizations file
    # result <- hhs_hosp_state_down(down_dir="~/Downloads")
    # loaded_data <- hhs_2_PROF(hhs_path=result$download_path, season = as.numeric(input$season), state=input$location)
    # data_path = "data/HHS_daily-hosp_state.csv"
    data_path = data_file
    loaded_data <- hhs_2_PROF(hhs_path=data_path, season=as.numeric(input$season),
                              state=input$location)

    # note that both loaded_data does not yet have  the data_fit list in it 'data_fit'
    shared_data$data = loaded_data

    if (!is.null(shared_data$data)) {
      obs_dates = list()
      for (ind in 1:length(loaded_data)) {
      obs_dates[[ind]] =loaded_data[[ind]]$data$date
      }
      dates_data$data$end_date   = min(max(obs_dates[[1]]), max(obs_dates[[2]]))
      dates_data$data$start_date = max(min(obs_dates[[1]]), min(obs_dates[[2]]))

    }


    if (!is.null(shared_data$data)){
      diseases = names(loaded_data)
      mydata = mytitle = list()
      for (ids in diseases) {
        mydata[[ids]] = loaded_data[[ids]]$data
        mytitle[[ids]] = paste0(loaded_data[[ids]]$loc_name,' - ',toupper(ids))
        state_abbv = loaded_data[[ids]]$loc_name # it is the same for all pathogens

      }
    }

      # Create time series plots
      output$plot1 <- renderPlotly({
        plot1 <- plot_ly(mydata[[1]], x = ~date, y = ~inc, type = "scatter", mode = "lines+markers", line=list(color=mycolor_list[['covid19']]),
                         marker = list(color=mycolor_list[['covid19']]))
        layout(plot1, title = mytitle[[1]], xaxis = list(title = ""), yaxis = list(title = "Daily New Hospitalization"),
               hovermode = "x unified")
      })

      output$plot2 <- renderPlotly({
        plot2 <- plot_ly(mydata[[2]], x = ~date, y = ~inc, type = "scatter", mode = "lines+markers", line=list(color=mycolor_list[['influenza']]),
                         marker = list(color=mycolor_list[['influenza']]))
        layout(plot2, title = mytitle[[2]], xaxis = list(title = ""), yaxis = list(title = "Daily New Hospitalization"),
               hovermode = "x unified")
      })

    shinyjs::html("loading_message_1b","")  # Disable loading message


    #Downloadable csv file with incidence data for chosen location
    output$dlInc <- downloadHandler(

      filename = function() { paste(state_abbv,"_incidence_",Sys.Date(),'.csv', sep='') },

      content = function(file) {

        # Title
        t <- c(paste("Daily Incidence data for", state_abbv, sep = " "),"","","","")
        #Subtitle
        tt <-  c(paste("Data Downloaded on",Sys.Date(), sep = " "),"","","","")
        #Column labels
        l <- c('loc_abbv', 'date','disease','metric','value')
        df = list()
        for (ids in diseases) {
          date = mydata[[ids]]$date
          inc  = mydata[[ids]]$inc
          loc  = rep(state_abbv, length(date))
          disease = rep(ids, length(date))
          metric  = rep('hosp', length(date))
          df[[ids]] = data.frame('loc_abbv' = loc, 'date' = date, 'disease' = disease, 'metric'= metric, 'value' = inc)
        }
        if (length(df) > 1) df_tot = rbind(df[[1]], df[[2]])

        df_tot[] <- lapply(df_tot, as.character)

        #Source
        # s <- c("Please see the Technical Notes tab of the application for data sources.","","","","")
        # p <- c("Prepared by Predictive Science Inc.","","","","")

        # dlm <- rbind(t, tt, l, df_tot, s, p)
        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })
  })


  # Observe button click for data download
  # observeEvent(input$downloadDataButton, {
  # 
  #   shinyjs::html("loading_message_1a","<strong> Downloading Data..Please Wait</strong>")
  # 
  #   # download HHS hospitalizations file
  #   # result <- hhs_hosp_state_down(down_dir="data")
  #   result <- fetch_hhs_data(down_dir="data")
  # 
  #   shinyjs::html("loading_message_1a","")  # Disable loading message
  # 
  #   # bump download_trigger so the data message updates
  #   download_trigger$num = download_trigger$num + 1
  # })

  # Get data file status when app is loaded
  # output$data_message <- reactive({
  #   # update anytime download_trigger changes
  #   req(download_trigger$num)
  #   # load data file
  #   hhs_data = read.csv(file="data/HHS_daily-hosp_state.csv")
  #   data_date = as.Date(max(hhs_data$date))
  #   cur_date = Sys.Date()
  # 
  #   if ((cur_date - data_date) < 12) {
  #     data_message = "The local PROF-Shiny data file appears to be up-to-date, but the data file can be updated using the Download Data button."
  #   } else {
  #     data_message = paste0("The local PROF-Shiny data file contains data from over 11 days ago (", data_date, "). Pressing the 'Download Data' button will likely result in more up-to-date data.")
  #   }
  #   # renderText(data_message)
  #   data_message
  # })

 observe({
   if (!is.null(shared_data$data)) {
     data = shared_data$data
     diseases = names(data)
     my_data = data[[diseases[1]]]
     my_year = year(my_data$data$date[1])
     ndate = length(my_data$data$date)
     ind = which(year_data == my_year)

     if (length(input$select_end_fit_stat) == 0) {
       end_fit_stat = my_data$data$date[ndate]
     } else {
       end_fit_stat = input$select_end_fit_stat
     }

     if (length(input$select_end_fit) == 0) {
       end_fit = my_data$data$date[ndate]
     } else {
       end_fit = input$select_end_fit
     }


     updateDateInput(session, "select_end_fit",min = end_fit_date_min[ind] , max = my_data$data$date[ndate], value = end_fit)
      updateDateInput(session, "select_end_fit_stat",min = end_fit_date_min[ind] , max = my_data$data$date[ndate], value = end_fit_stat)
     # updateDateInput(session, "cov_start_fit",min = cov_start_fit_date_min[ind] , max = cov_start_fit_date_max[ind], value = cov_start_fit_date_min[ind])
     # updateDateInput(session, "flu_start_fit",min = flu_start_fit_date_min[ind] , max = flu_start_fit_date_max[ind], value = flu_start_fit_date_min[ind])
     updateSelectInput(session, "location", choices = loc_abbv, selected = input$location)
     updateSelectInput(session, "season", choices = year_data, selected = input$season)
     updateCheckboxGroupInput(session, "disease", choices = c("COVID19"='covid19', "INFLUENZA"='influenza'), selected = input$disease)
   }
 })


  # Observe button click for Tab 2
  observeEvent(input$fitDataButton, {

    prof_data <- shared_data$data

    disease <- input$disease

    fit_start = NULL
    if (length(disease) > 1) {
      diseases=c("covid19", "influenza")
      fit_start = list('covid19'=input$cov_start_fit, 'influenza'=input$flu_start_fit)
      options_cov <- input$options_cov
      options_flu <- input$options_flu
      nb_cov <- as.numeric(input$nb_cov)
      nb_flu <- as.numeric(input$nb_flu)
    } else {
      if (disease == 'covid19') {
        diseases=c("covid19")
        fit_start = list('covid19'=input$cov_start_fit, 'influenza'=NULL)
        options_cov <- input$options_cov
        options_flu <- NULL
        nb_cov <- as.numeric(input$nb_cov)
        nb_flu <- NULL
      }
      if (disease == 'influenza') {
        diseases=c("influenza")
        fit_start = list('covid19' = NULL, 'influenza'=input$flu_start_fit)
        options_cov <- NULL
        options_flu <- input$options_flu
        nb_cov <- NULL
        nb_flu <- as.numeric(input$nb_flu)

      }

    }

    models = c(options_cov, options_flu)
    nb_vec = c(nb_cov, nb_flu)

    # create 'data_fit' since the user has selected the end_fit date

    prof_data <-hhs_set_fitdates(prof_data=prof_data, fit_start=fit_start,
                                 fit_end=as.Date(input$select_end_fit))

    shared_data$data <- prof_data # update shared_data$data to include 'data_fit'

    shinyjs::html("loading_message_2","<strong>    Fitting Incidence Data..Please Wait. This Will Take 10-15 Minutes</strong>")

    par_list = init_par_list(diseases=diseases,models=models)

    fit_list <- fit_data(prof_data = prof_data[diseases], par_list = par_list, nb_vec = nb_vec)

    shared_fit$data$fit_list = fit_list

    shared_par$data = par_list

    mech_fit <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_fit(prof_data = prof_data[diseases], par_list = par_list, fit_list = fit_list)
    })

    output$plot3 <- renderPlotly({mech_fit()$arrange_plot})

    shinyjs::html("loading_message_2","")  # Disable fitting message

    #Downloadable csv file with mechanistic fit to data

    state_abbv = input$location

    output$dlFitMech <- downloadHandler(

      filename = function() { paste(state_abbv,"_compartmental_fit_",Sys.Date(),'.csv', sep='') },

      content = function(file) {

        # Title
        t <- c(paste("Compartmenal Model Fit to", state_abbv,'Hospitalization data', sep = " "),"","","","","","","","","")
        #Subtitle
        tt <-  c(paste("Data Fitted on",Sys.Date(), sep = " "),"","","","","","","","","")
        #Column labels
        l <- c('loc_abbv', 'date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        df = list()
        for (ids in diseases) {
          total = mech_fit()$total_list[[ids]]
          loc = rep(state_abbv, length(nrow(total)))
          disease = rep(ids, length(nrow(total)))
          metric = rep('hosp', length(nrow(total)))
          date = total$date
          inc = total$reported
          sbst_total = subset(total, select = c("2.5%","25%","50%","75%","97.5%"))
          sbst_total = round(sbst_total, digits = 2)
          df[[ids]] = data.frame('loc_abbv' = loc, 'date' = date, 'disease'=disease,'metric'= metric, 'value' = inc, sbst_total)
          colnames(df[[ids]]) = c('loc_abbv','date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        }
        if (length(df) > 1) df_tot = rbind(df[[1]], df[[2]])

        df_tot[] <- lapply(df_tot, as.character)

        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })
  })


  # Observe button click for Tab 3
  observeEvent(input$fitStatButton, {

    prof_data <- shared_data$data

    diseaseStat <- input$diseaseStat

    fit_start = NULL
    if (length(diseaseStat) > 1) {
      diseases=c("covid19", "influenza")
      fit_start = list('covid19'=input$cov_start_fit_stat, 'influenza'=input$flu_start_fit_stat)
    } else {
      if (diseaseStat == 'covid19') {
        diseases=c("covid19")
        fit_start = list('covid19'=input$cov_start_fit_stat, 'influenza'=NULL)
      }
      if (diseaseStat == 'influenza') {
        diseases=c("influenza")
        fit_start = list('influenza'=input$flu_start_fit_stat)
      }

    }



    prof_data <-hhs_set_fitdates_stat(prof_data=prof_data, fit_start=fit_start,
                                      fit_end=as.Date(input$select_end_fit_stat))


    shared_data$data <- prof_data # update shared_data$data to include 'data_fit_stat'

    shinyjs::html("loading_message_3","<strong>Fitting Incidence Data..Please Wait.</strong>")

    #updateSelectInput(session, "disease", choices = )

    stat_fit <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_stat_fit(prof_data = prof_data, diseases = diseases)
    })


    output$plot4 <- renderPlotly({stat_fit()$arrange_plot})

    shinyjs::html("loading_message_3","")  # Disable fitting message


    #Downloadable csv file with statistical fit to data

    state_abbv = input$location

    output$dlFitStat <- downloadHandler(

      filename = function() { paste(state_abbv,"_stat_fit_",Sys.Date(),'.csv', sep='') },

      content = function(file) {

        # Title
        t <- c(paste("Baseline Statistical Fit to", state_abbv,'Hospitalization data', sep = " "),"","","","","","","","","")
        #Subtitle
        tt <-  c(paste("Data Fitted on",Sys.Date(), sep = " "),"","","","","","","","","")
        #Column labels
        l <- c('loc_abbv', 'date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        df = list()

        for (ids in diseaseStat) {
          total = stat_fit()$total_list[[ids]]
          loc = rep(state_abbv, length(nrow(total)))
          disease = rep(ids, length(nrow(total)))
          metric = rep('hosp', length(nrow(total)))
          date = total$date
          inc = total$reported
          sbst_total = subset(total, select = c("2.5%","25%","50%","75%","97.5%"))
          sbst_total = round(sbst_total, digits = 2)
          df[[ids]] = data.frame('loc_abbv' = loc, 'date' = date, 'disease'=disease,'metric'= metric, 'value' = inc, sbst_total)
          colnames(df[[ids]]) = c('loc_abbv','date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")

        }
        if (length(df) > 1) df_tot = rbind(df[[1]], df[[2]])

        df_tot[] <- lapply(df_tot, as.character)

        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })

  })




  # Observe button click for Tab 4
  observeEvent(input$forecastButton, {

    prof_data <- shared_data$data

    fit_list <- shared_fit$data$fit_list
    # fit_list will be NULL if a fit was not done before a forecast was requested
    if (is.null(fit_list)) {
      text <- 'For a Mechanistic Forecast you nmust\nfirst do a Mechanistic Fit.\nGo to Fit Incidence -> Mechanistic Tab.'
      output$plot5 <- renderPlotly({
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = text, size = 10, color = "red", hjust = 0.5, vjust = 0.5)
        })

    } else {
      
   
    par_list <- shared_par$data

    disease <- input$disease

    fit_start = NULL
    if (length(disease) > 1) {
      diseases=c("covid19", "influenza")
      options_cov <- input$options_cov
      options_flu <- input$options_flu
      nb_cov <- as.numeric(input$nb_cov)
      nb_flu <- as.numeric(input$nb_flu)
    } else {
      if (disease == 'covid19') {
        diseases=c("covid19")
        fit_start = list('covid19'=input$cov_start_fit, 'influenza'=NULL)
        options_cov <- input$options_cov
        options_flu <- NULL
        nb_cov <- as.numeric(input$nb_cov)
        nb_flu <- NULL
      }
      if (disease == 'influenza') {
        diseases=c("influenza")
        fit_start = list('covid19' = NULL, 'influenza'=input$flu_start_fit)
        options_cov <- NULL
        options_flu <- input$options_flu
        nb_cov <- NULL
        nb_flu <- as.numeric(input$nb_flu)

      }

    }

    shinyjs::html("loading_message_4","<strong>Calculating Forecast..Please Wait.</strong>")

    mech_forecast <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_forecast(prof_data = prof_data[diseases], par_list, fit_list, ntraj =1000, nfrcst = input$days_frcst)
    })



    if (length(mech_forecast()) > 0) {
      shared_wis$data$wis_mech <- mech_forecast()$wis_df
    }

    output$plot5 <- renderPlotly({mech_forecast()$arrange_plot})

    shinyjs::html("loading_message_4","")  # Disable fitting message

    #Downloadable csv file with Mechanistic forecast to data

    state_abbv = input$location

    output$dlFrcstMech <- downloadHandler(

      filename = function() { paste(state_abbv,"_compartmental_forecast_",Sys.Date(),'.csv', sep='') },

      content = function(file) {

        # Title
        t <- c(paste("Compartmental Forecast to", state_abbv,'Hospitalization data', sep = " "),"","","","","","","","","")
        #Subtitle
        tt <-  c(paste("Data Fitted on",Sys.Date(), sep = " "),"","","","","","","","","")
        #Column labels
        l <- c('loc_abbv', 'date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        df = list()
        for (ids in names(mech_forecast()$total_list)) {
          total = mech_forecast()$total_list[[ids]]
          loc = rep(state_abbv, length(nrow(total)))
          my_disease = ids
          if (my_disease == 'random') my_disease = 'combined-random'
          if (my_disease == 'sorted') my_disease = 'combined-sorted'
          disease = rep(my_disease, length(nrow(total)))
          metric = rep('hosp', length(nrow(total)))
          date = total$date
          inc = total$reported_fit
          sbst_total = subset(total, select = c("2.5%","25%","50%","75%","97.5%"))
          sbst_total = round(sbst_total, digits = 2)
          df[[ids]] = data.frame('loc_abbv' = loc, 'date' = date, 'disease'=disease,'metric'= metric, 'value' = inc, sbst_total)
          colnames(df[[ids]]) = c('loc_abbv','date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        }
        if (length(df) > 1) df_tot = rbind(df[[1]], df[[2]], df[[3]], df[[4]])

        df_tot[] <- lapply(df_tot, as.character)

        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })
    }
  })

  # Observe button click for Tab 5
  observeEvent(input$forecastStatButton, {

    prof_data <- shared_data$data

    shinyjs::html("loading_message_5","<strong>Calculating Statistical Forecast..Please Wait.</strong>")

    if (length(input$diseaseStat) > 1) {
      diseases=c("covid19", "influenza")
    } else {
      if (input$diseaseStat == 'covid19') diseases=c("covid19")
      if (input$diseaseStat == 'influenza') diseases = c("influenza")
    }

    stat_forecast <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_stat_forecast(prof_data = prof_data, diseases = diseases, nfrcst = input$days_frcst_stat)
    })


    if (length(stat_forecast()) > 0) {
      shared_wis$data$wis_stat <- stat_forecast()$wis_df
    }

    output$plot6 <- renderPlotly({stat_forecast()$arrange_plot})

    # output$plot6 <- renderPlotly({stat_forecast()$arrange_plot})

    shinyjs::html("loading_message_5","")  # Disable fitting message

    #Downloadable csv file with statistical forecast

    state_abbv = input$location

    output$dlFrcstStat <- downloadHandler(

      filename = function() { paste(state_abbv,"_stat_forecast_",Sys.Date(),'.csv', sep='') },

      content = function(file) {

        # Title
        t <- c(paste("Baseline Statistical Forecast to", state_abbv,'Hospitalization data', sep = " "),"","","","","","","","","")
        #Subtitle
        tt <-  c(paste("Data Fitted on",Sys.Date(), sep = " "),"","","","","","","","","")
        #Column labels
        l <- c('loc_abbv', 'date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        df = list()
        for (ids in names(stat_forecast()$total_list)) {
          total = stat_forecast()$total_list[[ids]]
          loc = rep(state_abbv, length(nrow(total)))
          my_disease = ids
          if (my_disease == 'random') my_disease = 'combined-random'
          if (my_disease == 'sorted') my_disease = 'combined-sorted'
          disease = rep(my_disease, length(nrow(total)))
          metric = rep('hosp', length(nrow(total)))
          date = total$date
          inc = total$reported_fit
          sbst_total = subset(total, select = c("2.5%","25%","50%","75%","97.5%"))
          sbst_total = round(sbst_total, digits = 2)
          df[[ids]] = data.frame('loc_abbv' = loc, 'date' = date, 'disease'=disease,'metric'= metric, 'value' = inc, sbst_total)
          colnames(df[[ids]]) = c('loc_abbv','date','disease','metric','value',"2.5%","25%","50%","75%","97.5%")
        }
        if (length(df) > 1) df_tot = rbind(df[[1]], df[[2]], df[[3]], df[[4]])

        df_tot[] <- lapply(df_tot, as.character)

        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })

  })




  observeEvent(input$wis_button, {

    if (!is.null(shared_wis$data)) {
      wis_names = names(shared_wis$data)
      all_null <- all(sapply(wis_names, is.null))
      if (all_null) {
        reactive_value = FALSE
      } else {
        reactive_value = TRUE
      }
    } else {
      reactive_value = FALSE
    }

    text <- 'Forecast WIS cannot be evaluated.\nThere is not yet observed data\nfor the forecasted time range.'

    # data <- data.frame(x = numeric(0), y = numeric(0))

    if (reactive_value) {
      wis_data = shared_wis$data

      state_abbv=input$location

      wis_output<- shiny_plot_wis(wis_data, state_abbv)
      
      wis_df <- wis_output$wis_df
      # Render the plot
      output$wis_plot <- renderPlotly({
        wis_output$arrange_plot
      })

      wis_df$metric = rep('wis', nrow(wis_df))
      wis_df$loc_abbv = rep(state_abbv, nrow(wis_df))
      
    } else {
      
      output$wis_plot <- renderPlotly({
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = text, size = 10, color = "red", hjust = 0.5, vjust = 0.5)
      })
      
      wis_df = NULL
    }

    output$dlWIS <- downloadHandler(
      
      filename = function() { paste(state_abbv,"_wis_",Sys.Date(),'.csv', sep='') },
      
      content = function(file) {
        
        # Title
        t <- c(paste("WIS for ", state_abbv,'Hospitalization data', sep = " "),"","","","","")
        #Subtitle
        tt <-  c(paste("Data Fitted and Forecasted on",Sys.Date(), sep = " "),"","","","","")
        #Column labels
        l <- c('date','value','disease','model','metric','loc_abbv')

        df_tot = wis_df
        df_tot[] <- lapply(df_tot, as.character)
        
        dlm <- rbind(t, tt, l, df_tot)
        write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      })



  })



}
