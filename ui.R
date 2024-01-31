# Load required libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(deSolve)
library(PROF)

# download data

# download HHS hospitalizations file
# result <<- hhs_hosp_state_down(down_dir="~/Downloads")

# locations
loc_abbv <<- loc_pops$abbreviation
loc_name <<- loc_pops$location_name

# for now we remove the US and VI
ind = which(loc_abbv %in% c('US', 'VI'))

loc_abbv <<- loc_abbv[-ind]
loc_name <<- loc_name[-ind]

year_data <<- c(2021, 2022, 2023)
nyear <<- length(year_data)

# Define default end dates for fitting
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
end_fit_date_min <<- c(as.Date(paste0(year_data,'-10-01')))
end_fit_date_max <<- c(as.Date(paste0(year_data+1,'-06-01')))
end_fit_date_max[nyear] <<- Sys.Date()

cov_start_fit_date_min <<- c(as.Date('2021-10-15'), as.Date('2022-10-15'),as.Date('2023-07-01'))
cov_start_fit_date_max <<- c(as.Date('2021-11-15'), as.Date('2022-11-15'),as.Date('2023-10-15'))

flu_start_fit_date_min <<- c(as.Date('2021-09-01'), as.Date('2022-09-01'),as.Date('2023-09-01'))
flu_start_fit_date_max <<- c(as.Date('2021-11-15'), as.Date('2022-11-15'),as.Date('2023-10-15'))

# Define colors for plots
mycolor_list <<- list('covid19' = "#0072B2", 'influenza'= "#FC4E07",
                      'combined' = "#CC79A7") #= "#D55E00",

# Define UI
ui <- navbarPage(
  useShinyjs(),  # Initialize shinyjs
  title = ("PROF A Package for Respiratory Disease Open-source Forecasting"),
  id = "navbar",
  inverse=TRUE,
  selected = "Introduction",
  position = "static-top",
  theme = shinytheme("cosmo"),
  windowTitle = "PROF - Open",

  footer = tagList(
    fluidRow(br(),
             hr()
    ), #End of fluidRow
    fluidRow(
      column(2,""),
      column(4,

             h4(paste0("Version 1.0.1 | Released ",Sys.Date())),
             #h4("EXPIRES: April 10, 2020"),
             HTML("<h5>CONTACT: mbennun@predsci.com jturtle@predsci.com</h5>"),
             HTML("<h5> <a href='https://github.com/predsci/PROF/'>PROF GitHub </a> | <a href='https://predsci.github.io/PROF/'>PROF Github.io</a>  |  <a href='https://www.predsci.com/usa-flu-hosp/'>PSI Influenza Forecasting Page</a> "),
             # HTML("<h6>Icons provided by the nounproject.com: <a href='https://thenounproject.com/browse/?i=2683859' target='_blank'>Magnify</a> | <a href='https://thenounproject.com/browse/?i=772325' target='_blank'>Binoculars</a>")
        ),
        column(4,
               img(src = 'psi.gif', align = "right"),
               br()

      ),
      column(2,"")
    ) #End of fluidRow
  ),

  #### Landing Page ####
  tabPanel("Introduction",
           fluidPage(
             tags$head(
               tags$style(
                 HTML(".shiny-notification {
                                      height: 50px;
                                      width: 300px;
                                      position: fixed;
                                      top: 50%;
                                      left: 50%;
                                      margin: -70px 0 0 -170px;
                                      }"
                 )
               )
             ),
             useShinyjs(),
             fluidRow(
               column(3,""),
               column(6,
                      br(),
                      HTML("<div style='text-align:center'><h1>Estimating the Combined Burden of COVID-19 and Influenza</h1></div><br>"),
                      hr()),
               column(3,""),
             ), #End of fluidRow
             br(),
             fluidRow(
               column(3, ""),
               column(6,
                      HTML("<div style='text-align:center'><h2>Get Started & Explore Incidence</h2> </div><br>"),
                      img(id = "incidence_img", src = 'incidence_icon.png', align = "center", width = '100%', style="cursor:pointer;", alt = "1. Explore Incidence"),
                      HTML("<hr><center>Explore COVID-19 and Influenza Incidence Data </center>")
               ),
               column(3, "")
             ) #End of fluidRow
           ) # End fluid page
  ), # End tab panel
  # tabsetPanel(

    #### Landing Page ####
    tabPanel("1. Explore Incidence",
             fluidPage(
               h2('Download Daily COVID-19 and Influenza Hospitalization Data'),
               h4("Use the Dropdown menus to select a location and a season. When you completed your selection click the Download button."),
               h4("You can proceed to the Fitting tab only after the data download is completed. Explore the data by hovering over it."),
               hr(),
             fluidRow(
               br(),
               column(6, selectInput("location", "Select Location:", choices = loc_abbv, selected = 'CA')),
               # column(2,""),
               column(6,selectInput("season", "Select Season:", choices = year_data, selected = 2023)),
               # column(4,""),
               column(6,actionButton("loadDataButton", "Download Incidence Data")),
               column(3,""),
               br(),
               br(),
               htmlOutput("loading_message_1"),
               br(),
               br(),
               br(),
               column(12,plotlyOutput("plot1")),
               tags$head(
                 tags$style(
                   HTML("
            #plot1 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot1.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot1').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot1').removeClass('fading');
          });
        ")
               ),
               column(12,plotlyOutput("plot2")),
               tags$head(
                 tags$style(
                   HTML("
            #plot2 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot2.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot2').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot2').removeClass('fading');
          });
        ")
               )
             )
             ) # End of fluidPage
    ), # End of Incidence Panel

tabPanel("2. Fit Incidence", # The Fitting Tab has the Mechanistic and Statistical tabs under it.
tabsetPanel(
    tabPanel("Mechanistic",
             fluidPage(
               h2("Fitting Compartmetal Models To Data"),
               h4("Setup a compratmental model for each pathogen.  When a pathogen is selected you will be promted to define
                  the specifics of the compartmental model. You can proceed to the Forecasting Tab only after the fitting is completed."),
               hr(),
               column(12,checkboxGroupInput("disease", "Select Pathogens:", choices = c("COVID19"='covid19', "INFLUENZA"='influenza'), inline = TRUE)),
               h4("You can select both pathogens or only one of them."),
               column(6,conditionalPanel(
                 condition = "input.disease.indexOf('covid19') !== -1",
                 checkboxGroupInput("options_cov", "Select Compartmetal Model for COVID19:",choices  = c("SEIRH"='seirh', "SIRH"='sirh'), inline = TRUE),
                 h5('For COVID-19 we recommend selecting the Sucscptible-Exposed-Infectious-Recovered-Hospitalized option'),
                 checkboxGroupInput("nb_cov", "Select Number of Values for the time-dependent Force of Infection:", choices = c(3,2), inline = TRUE),
                 h5('Based on the complexity of the COVID-19 time-series, select either three- or two-values for the time-dependent FOI.'),
                 dateInput("cov_start_fit", "Select Start Date For Fitting COVID19:", min = cov_start_fit_date_min[nyear], max= cov_start_fit_date_max[nyear],
                           value = cov_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('For COVID-19 examine the time-series and decide if to start the fit in the summer or later.')
               )),
                column(6,conditionalPanel(
                 condition = "input.disease.indexOf('influenza') !== -1",
                 checkboxGroupInput("options_flu", "Select Compartmetal Model for Influenza:", choices = c("SIRH"='sirh', "SEIRH"='seirh'), inline=TRUE),
                 h5('For Influenza we recommend selecting the Sucscptible-Infectious-Recovered-Hospitalized option'),
                 checkboxGroupInput("nb_flu", "Select Number of Values for the time-dependent Force of Infection:", choices = c(2,3), inline = TRUE),
                 h5('Typically a model with two-values for the time dependent FOI is suitable for fitting an entire Influenza season.'),
                 dateInput("flu_start_fit", "Select Start Date For Fitting Influenza:", min = flu_start_fit_date_min[nyear], max= flu_start_fit_date_max[nyear],
                           value = flu_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('For Influenza the default start date for each season is recommended.')
               )),
               br(),
               hr(style = "clear: both;"),
               column(12,dateInput("select_end_fit", "Select End Date For Fitting: ", min = end_fit_date_min[1], max= end_fit_date_max[nyear],
                                      value = end_fit_date_max[nyear],format = "yyyy-mm-dd")),
               h4("By default we fit all available data. If you select to fit only part of the data, you can compare the reported data to the forecast (See Forecast Tab)."),
               # Custom JavaScript to center the dateInput
               tags$script(HTML("
      $(document).ready(function() {
        $('#select_end_fit').parent().css('display', 'flex');
        $('#select_end_fit').parent().css('justify-content', 'center');
      });
    ")),
               # br(),
               # br(),
               # br(),
               br(),
               column(12,actionButton("fitDataButton", "Mechanistic Fit to Incidence")),
               br(),
               br(),
               h4("Once you have made all you selections, press the Fit button."),
               hr(),
               # Custom JavaScript to center the dateInput
               tags$script(HTML("
      $(document).ready(function() {
        $('#fitDataButton').parent().css('display', 'flex');
        $('#fitDataButton').parent().css('justify-content', 'center');
      });
    ")),
               # br(),
               # br(),
               htmlOutput("loading_message_2"),
               br(),
               br(),
               plotlyOutput("plot3"),
               tags$head(
                 tags$style(
                   HTML("
            #plot3 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot3.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),
               tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot3').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot3').removeClass('fading');
          });
        ")
               )
             )
    ),
    tabPanel("Statistical",
             fluidPage(
               h2("Fitting A Baseline Statistical Model To The Data"),
               h4('This model aims to replicate the entire time-series, capturing uncertainty from day-to-day variations in the data.'),
               hr(),
               column(12, checkboxGroupInput("diseaseStat", "Select Pathogens", choices = c("COVID-19"='covid19', "INFLUENZA" = 'influenza'), inline = TRUE)),
               h4("You can select both pathogens or only one of them. Once you select a pathogen you will be promted to select the start date for the fit."),
               column(6,conditionalPanel(
                 condition = "input.diseaseStat.indexOf('covid19') !== -1",
                 dateInput("cov_start_fit_stat", "Select Start Date For Fitting COVID19:", min = cov_start_fit_date_min[nyear], max= cov_start_fit_date_max[nyear],
                           value = cov_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('Modifying the start date will only affect the width of the fit, not its median value.')
               )),
               column(6,conditionalPanel(
                 condition = "input.diseaseStat.indexOf('influenza') !== -1",
                 dateInput("flu_start_fit_stat", "Select Start Date For Fitting Influenza:", min = flu_start_fit_date_min[nyear], max= flu_start_fit_date_max[nyear],
                           value = flu_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('Modifying the start date will only affect the width of the fit, not its median value.')
               )),
               br(),
               hr(style = "clear: both;"),
               column(12, dateInput("select_end_fit_stat", "Select End Date For Fitting: ", min = end_fit_date_min[1], max= end_fit_date_max[1],
                                    value = end_fit_date_max[1],format = "yyyy-mm-dd")),
               h4("By default we fit all available data. If you select to fit only part of the data, you can compare the reported data to the forecast (See Forecast Tab)."),
               tags$script(HTML("
                $(document).ready(function() {
                $('#select_end_fit').parent().css('display', 'flex');
                $('#select_end_fit').parent().css('justify-content', 'center');
                });
                ")),
               br(),
               column(12,actionButton("fitStatButton", "Statistical Fit to Incidence")),
               br(),
               br(),
               h4("Once you have made all you selections, press the Fit button."),
               hr(),
               htmlOutput("loading_message_3"),
               br(),
               br(),
               plotlyOutput("plot4"),
              tags$head(
                tags$style(
                  HTML("
            #plot4 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot4.fading {
              opacity: 0.2;
            }
          ")
                )
              ),
              tags$script(
                HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot4').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot4').removeClass('fading');
          });
        ")
              )
             )
    )
    )
),
tabPanel("3. Create Forecast", # The Forecasting Tab has the Mechanistic and Statistical tabs under it.
         tabsetPanel(
    tabPanel("Mechanistic",
             fluidPage(
               h2("Forecasting Using a Compartmental Model"),
               h4("For each fitted pathogen, the posterior distribution of the parameters will be
                  utilized to generate a forecast. Note that only pathogens that were fitted will be forecasted."),
               hr(),
               column(12,sliderInput("days_frcst", "Select Number of Days For Forecast:",
                                    min = 7, max = 42, value = c(35))),
               h4("Move the slider to select the number of days for the forecast."),
               br(),
               column(12,actionButton("forecastButton", "Mechanistic Forecast")),
               br(),
               br(),
               h4("Once you made your selection press the Forecast button."),
               hr(),
               htmlOutput("loading_message_4"),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               plotlyOutput("plot5"),
               tags$head(
                 tags$style(
                   HTML("
            #plot5 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot5.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),
               tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot5').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot5').removeClass('fading');
          });
        ")
               )
             )
    ),
    tabPanel("Statistical",
             fluidPage(
               h2("Forecasing With A Baseline Statistical Model"),
               h4("This model aims to replicate the entire time-series, capturing uncertainty from day-to-day variations in the data.
               Median forecast values reproduce the seven last observed values. Your choices of pathogens here is independent of choices
                  you made before and only requires the incidence data."),
               hr(),
               column(12, checkboxGroupInput("diseaseStatFrcst", "Select Pathogens", choices = c("COVID-19"='covid19', "INFLUENZA" = 'influenza'))),
               h4('You can select either both pathogens or only one of them.'),
               hr(),
               column(12,sliderInput("days_frcst_stat", "Select Number of Days For Forecast:",
                           min = 7, max = 42, value = c(35))),
               br(),
               h4("Move the slider to select the number of days for the forecast."),
               br(),
               column(12,actionButton("forecastStatButton", "Statistical Forecast")),
               br(),
               br(),
               h4("Once you made your selection press the Forecast button."),
               hr(),
               htmlOutput("loading_message_5"),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               plotlyOutput("plot6"),
               tags$head(
                 tags$style(
                   HTML("
            #plot6 {
              transition: opacity 0.5s ease-in-out;
            }
            #plot6.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),
               tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot6').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot6').removeClass('fading');
          });
        ")
               )
             )
    )
    )
    ),
tabPanel("About",
         fluidPage(
           h2("PROF"),
           p("PROF is an R package (with Fortran code) for fitting and forecasting infectious disease incidence.
                         The package ingests publicly-available confirmed hospital admission data, fits mechanistic and statistical
                         models to the data, and provides short-term probabilistic forecasts. Currently, the package supports fitting
                         and forecasting the individual and combined burden of influenza and COVID-19 at the state level.
                         S[I]2HR and SE[I]2HR models are used to fit the two pathogens and both models use a flexible,
                         time-dependent transmission term. A baseline statistical model is also offered for each pathogen.
                         Once the User selects a state, and either one or both viruses,
                         the PROF fitting procedure iteratively determines the joint posterior distribution of model parameters.
                         The joint posterior distribution is then used with the model to generate location-specific probabilistic
                         forecasts of the near-term number of hospital admissions. If both viruses are chosen, this procedure is done
                         twice and the total hospital burden forecast is estimated by combining the trajectory profiles of each disease
                         in multiple ways, including random, ordered, and semi-ordered. If the statistical model is also chosen, each
                         pathogen is independently fitted with the model and the combinded burden is estimated."),
           p("For more on PROF see",tags$a(href="https://predsci.github.io/PROF/",'our web documentation'))))
    # tabPanel("About",
    #          mainPanel(h2("PROF"),
    #                    p("PROF is an R package (with Fortran code) for fitting and forecasting infectious disease incidence.
    #                      The package ingests publicly-available confirmed hospital admission data, fits mechanistic and statistical
    #                      models to the data, and provides short-term probabilistic forecasts. Currently, the package supports fitting
    #                      and forecasting the individual and combined burden of influenza and COVID-19 at the state level.
    #                      S[I]2HR and SE[I]2HR models are used to fit the two pathogens and both models use a flexible,
    #                      time-dependent transmission term. A baseline statistical model is also offered for each pathogen.
    #                      Once the User selects a state, and either one or both viruses,
    #                      the PROF fitting procedure iteratively determines the joint posterior distribution of model parameters.
    #                      The joint posterior distribution is then used with the model to generate location-specific probabilistic
    #                      forecasts of the near-term number of hospital admissions. If both viruses are chosen, this procedure is done
    #                      twice and the total hospital burden forecast is estimated by combining the trajectory profiles of each disease
    #                      in multiple ways, including random, ordered, and semi-ordered. If the statistical model is also chosen, each
    #                      pathogen is independently fitted with the model and the combinded burden is estimated."),
    #                    p("For more on PROF see ",tags$a(href="https://predsci.github.io/PROF/",'our web documentation'))))

  # )
)


