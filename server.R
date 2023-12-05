# Define server

source('helpers.R')

server <- function(input, output, session) {

  shared_data <- reactiveValues(data = NULL)

  shared_fit <- reactiveValues(data = NULL)

  shared_par <- reactiveValues(data = NULL)

  dates_data <- reactiveValues(data = NULL)

  # Observe button click for Tab 1
  observeEvent(input$loadDataButton, {

    shinyjs::html("loading_message_1","<strong> Loading Data..Please Wait</strong>")

    # download HHS hospitalizations file
    result <- hhs_hosp_state_down(down_dir="~/Downloads")
    loaded_data <- hhs_2_PROF(hhs_path=result$download_path, season = as.numeric(input$season), state=input$location)
    # loaded_data <- hhs_data_ex(season = as.numeric(input$season), state=input$location)

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
        plot1 <- plot_ly(mydata[[2]], x = ~date, y = ~inc, type = "scatter", mode = "lines+markers", line=list(color=mycolor_list[['influenza']]),
                         marker = list(color=mycolor_list[['influenza']]))
        layout(plot1, title = mytitle[[2]], xaxis = list(title = ""), yaxis = list(title = "Daily New Hospitalization"),
               hovermode = "x unified")
      })

    shinyjs::html("loading_message_1","")  # Disable loading message


  })


 observe({
   if (!is.null(shared_data$data)) {
     data = shared_data$data
     diseases = names(data)
     my_data = data[[diseases[1]]]
     my_year = year(my_data$data$date[1])
     ndate = length(my_data$data$date)
     ind = which(year_data == my_year)
     updateDateInput(session, "select_end_fit",min = end_fit_date_min[ind] , max = my_data$data$date[ndate], value = my_data$data$date[ndate])
     updateDateInput(session, "select_end_fit_stat",min = end_fit_date_min[ind] , max = my_data$data$date[ndate], value = my_data$data$date[ndate])
     updateSelectInput(session, "location", choices = loc_abbv, selected = input$location )
     updateSelectInput(session, "season", choices = year_data, selected = input$season)
   }
 })


  # Observe button click for Tab 2
  observeEvent(input$fitDataButton, {

    prof_data <- shared_data$data

    # create 'data_fit' since the user has selected the end_fit date

    prof_data <-hhs_set_fitdates(prof_data=prof_data, fit_start=NULL,
                                 fit_end=as.Date(input$select_end_fit))

    shared_data$data <- prof_data # update shared_data$data to include 'data_fit'

    shinyjs::html("loading_message_2","<strong>Fitting Incidence Data..Please Wait.<br>This Will Take 10-15 Minutes</strong>")

    if (length(input$selected_pathogens_models) > 1) {

    if ("A" %in% input$selected_pathogens_models & "B" %in% input$selected_pathogens_models) {
      diseases = c('covid19', 'influenza')
      models   = c('seirh', 'sirh')
    } else if ("C" %in% input$selected_pathogens_models & "D" %in% input$selected_pathogens_models) {
      diseases = c('covid19', 'influenza')
      models   = c('sirh', 'seirh')
    } else if ("A" %in% input$selected_pathogens_models & "C" %in% input$selected_pathogens_models) {
      diseases = c('covid19', 'influenza')
      models   = c('seirh', 'seirh')
    } else if ("B" %in% input$selected_pathogens_models & "D" %in% input$selected_pathogens_models) {
      diseases = c('covid19', 'influenza')
      models   = c('sirh', 'sirh')
    } else {
      diseases = c('covid19', 'influenza')
      models   = c('seirh', 'sirh')
      print("Unreanable Selection Fitting COVID19/INFLUENZA with SEIRH/SIRH Models")
    }

  } else {
    if (input$selected_pathogens_models == 'A') {
      diseases = c('covid19')
      models  = c('seirh')
    } else if (input$selected_pathogens_models == 'B') {
      diseases = c('influenza')
      models  = c('sirh')
    } else if (input$selected_pathogens_models == 'C') {
      diseases = c('covid19')
      models  = c('sirh')
    } else {
      diseases = c('influenza')
      models  = c('seirh')
    }
  }
    # if (input$disease == 'covid19') {
    #   diseases=c("covid19")
    # } else if (input$disease == 'influenza') {
    #   diseases=c("influenza")
    # } else {
    #   diseases=c("covid19", "influenza")
    # }
    #
    # if(input$model == 'sirh') {
    #   models = c('sirh')
    # } else if (input$model == 'seirh') {
    #   models = c('seirh')
    # } else if (input$model == 'seirh/sirh') {
    #   models = c('seirh', 'sirh')
    # } else if (input$model == 'seirh/seirh') {
    #   models = c('seirh', 'seirh')
    # } else {
    #   models = c('sirh', 'sirh')
    # }


    par_list = init_par_list(diseases=diseases,models=models)

    fit_list <- fit_data(prof_data = prof_data, par_list = par_list)

    shared_fit$data = fit_list

    shared_par$data = par_list

    pl_list <- reactive ({
      if (!is.null(fit_list))
        shiny_plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)
    })


    output$plot3 <- renderPlotly({pl_list()})

    shinyjs::html("loading_message_2","")  # Disable fitting message
  })


  # Observe button click for Tab 3
  observeEvent(input$fitStatButton, {

    prof_data <- shared_data$data

    prof_data <-hhs_set_fitdates_stat(prof_data=prof_data, fit_start=NULL,
                                      fit_end=as.Date(input$select_end_fit_stat))

    shared_data$data <- prof_data # update shared_data$data to include 'data_fit_stat'

    shinyjs::html("loading_message_3","<strong>Fitting Incidence Data..Please Wait.</strong>")

    print(length(input$diseaseStat))
    if (length(input$diseaseStat) > 1) {
      diseases=c("covid19", "influenza")
    } else {
      if (input$diseaseStat == 'covid19') diseases=c("covid19")
      if (input$diseaseStat == 'influenza') diseases=c("influenza")
    }
    # if (input$diseaseStat == 'covid19') {
    #   diseases=c("covid19")
    # } else if (input$diseaseStat == 'influenza') {
    #   diseases=c("influenza")
    # } else {
    #   diseases=c("covid19", "influenza")
    # }

    #updateSelectInput(session, "disease", choices = )

    pl_list <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_stat_fit(prof_data = prof_data, diseases = diseases)
    })


    output$plot4 <- renderPlotly({pl_list()})

    shinyjs::html("loading_message_3","")  # Disable fitting message
  })


  # Observe button click for Tab 4
  observeEvent(input$forecastButton, {

    prof_data <- shared_data$data

    fit_list <- shared_fit$data
    par_list <- shared_par$data

    shinyjs::html("loading_message_4","<strong>Calculating Forecast..Please Wait.</strong>")


    pl_list <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_forecast(prof_data = prof_data, par_list, fit_list, ntraj =1000, nfrcst = input$days_frcst)
    })

    output$plot5 <- renderPlotly({pl_list()})

    shinyjs::html("loading_message_4","")  # Disable fitting message
  })

  # Observe button click for Tab 5
  observeEvent(input$forecastStatButton, {

    prof_data <- shared_data$data

    shinyjs::html("loading_message_5","<strong>Calculating Statistical Forecast..Please Wait.</strong>")

    if (length(input$diseaseStatFrcst) > 1) {
      diseases=c("covid19", "influenza")
    } else {
      if (input$diseaseStatFrcst == 'covid19') diseases=c("covid19")
      if (input$diseaseStatFrcst == 'influenza') diseases = c("influenza")
    }
    # if (input$diseaseStatFrcst == 'covid19') {
    #   diseases=c("covid19")
    # } else if (input$diseaseStatFrcst == 'influenza') {
    #   diseases=c("influenza")
    # } else {
    #   diseases=c("covid19", "influenza")
    # }

    pl_list <- reactive ({
      if (!is.null(prof_data))
        shiny_plot_stat_forecast(prof_data = prof_data, diseases = diseases, nfrcst = input$days_frcst_stat)
    })

    output$plot6 <- renderPlotly({pl_list()})

    shinyjs::html("loading_message_5","")  # Disable fitting message
  })


}
