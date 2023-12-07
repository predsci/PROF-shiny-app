# Load required libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
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
loc_name<<- loc_name[-ind]

year_data <<- c(2021, 2022, 2023)
nyear<<- length(year_data)

# Define default end dates for fitting
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
end_fit_date_min <<- c(as.Date(paste0(year_data,'-10-01')))
end_fit_date_max <<- c(as.Date(paste0(year_data+1,'-06-01')))
end_fit_date_max[nyear] <<- Sys.Date()

# Define colors for plots
mycolor_list <<- list('covid19' = "#0072B2", 'influenza'= "#FC4E07",
                      'combined' = "#CC79A7") #= "#D55E00",

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("PROF Shiny App"),
  tabsetPanel(
    tabPanel("Exploring Incidence Data",
             br(),
             fluidRow(
               br(),
               column(6, selectInput("location", "Select Location:", choices = loc_abbv, selected = 'CA')),
               column(6,selectInput("season", "Select Season:", choices = year_data, selected = 2023)),
               column(6,actionButton("loadDataButton", "Download Incidence Data")),
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
    ),
    tabPanel("Fitting Mechanistic",
             fluidRow(
               br(),
               # column(12,checkboxGroupInput("selected_pathogens_models", "Select Pathogens and Models", choices = c("COVID-19  SEIRH" = 'A', "INFLUENZA  SIRH" = 'B',
               #                                                                                                      "COVID-19  SIRH" = 'C', 'INFLUENZA  SEIRH' = 'D'))),
               # column(6, selectInput("disease", "Select Pathogens:", choices = c('COVID19' = 'covid19',
               #                                                                   'INFLUENZA'= 'influenza','COVID19 & INFLUENZA' = 'both'), selected = 'both')),
               # column(6, selectInput("model", "Select Model:", choices = c('SIRH' = 'sirh','SEIRH' = 'seirh','SEIRH/SIRH' = 'seirh/sirh',
               #                                                             'SEIRH/SEIRH' = 'seirh/seirh', 'SIRH/SIRH' = 'sirh/sirh'), selected = 'seirh/sirh' )),
               # column(12,checkboxGroupInput("disease","Select Pathogens", choices = c("COVID19"='covid19', 'INFLUENZA'='influenza'))),
               column(12,checkboxGroupInput("disease", "Select Pathogens", choices = c("COVID19"='covid19', "INFLUENZA"='influenza'))),
               column(6,conditionalPanel(
                 condition = "input.disease.indexOf('covid19') !== -1",
                 checkboxGroupInput("options_cov", "Select Compartmetal Model for COVID19", choices = c("SEIRH"='seirh', "SIRH"='sirh'))
               )),
               column(6,conditionalPanel(
                 condition = "input.disease.indexOf('influenza') !== -1",
                 checkboxGroupInput("options_flu", "Select Compartmetal Model for Influenza", choices = c("SIRH"='sirh', "SEIRH"='seirh'))
               )),
               br(),
               br(),
               br(),
               column(12, dateInput("select_end_fit", "Select End Date For Fitting: ", min = end_fit_date_min[1], max= end_fit_date_max[1],
                                      value = end_fit_date_max[1],format = "yyyy-mm-dd")),
               br(),
               br(),
               br(),
               column(6,actionButton("fitDataButton", "Mechanistic Fit to Incidence")),
               br(),
               br(),
               htmlOutput("loading_message_2"),
               br(),
               br(),
               br(),
               br(),
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
    tabPanel("Fitting Statistical",
             fluidRow(
               br(),
               column(6, checkboxGroupInput("diseaseStat", "Select Pathogens", choices = c("COVID-19"='covid19', "INFLUENZA" = 'influenza'))),
               # column(6, selectInput("diseaseStat", "Select Pathogens:", choices = c('COVID19' = 'covid19',
               #                                                                   'INFLUENZA'= 'influenza',
               #                                                                   'COVID19 & INFLUENZA' = 'both'),
                                     # selected = 'both')),
              # br(),
               column(6, dateInput("select_end_fit_stat", "Select End Date For Fitting: ", min = end_fit_date_min[1], max= end_fit_date_max[1],
                                    value = end_fit_date_max[1],format = "yyyy-mm-dd")),
               br(),
               br(),
               column(8,actionButton("fitStatButton", "Statistical Fit to Incidence")),
               br(),
               br(),
               htmlOutput("loading_message_3"),
               br(),
               br(),
               br(),
               br(),
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
    ),
    tabPanel("Forecasting Mechanistic",
             fluidRow(
               br(),
               column(6,sliderInput("days_frcst", "Select Number of Days For Forecast:",
                                    min = 7, max = 42, value = c(35))),
               HTML("<p>  The Posterior Distribution Will be Used for the Forecast"),
               br(),
               column(6,actionButton("forecastButton", "Mechanistic Forecast")),
               br(),
               br(),
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
    tabPanel("Forecasting Statistical",
             fluidRow(
               br(),
               column(6, checkboxGroupInput("diseaseStatFrcst", "Select Pathogens", choices = c("COVID-19"='covid19', "INFLUENZA" = 'influenza'))),
               # column(6, selectInput("diseaseStatFrcst", "Select Pathogens:", choices = c('COVID19' = 'covid19',
               #                                                                       'INFLUENZA'= 'influenza',
               #                                                                       'COVID19 & INFLUENZA' = 'both'),
               #                       selected = 'both')),
               br(),
               column(6,sliderInput("days_frcst_stat", "Select Number of Days For Forecast:",
                           min = 7, max = 42, value = c(35))),
               br(),
               column(12,HTML("<p>  A Baseline Statistical Model Will be Used for the Forecast")),
               br(),
               column(6,actionButton("forecastStatButton", "Statistical Forecast")),
               br(),
               br(),
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
    ),
    tabPanel("About",
             mainPanel(h2("PROF"),
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
                       p("For more on PROF see ",tags$a(href="https://predsci.github.io/PROF/",'our web documentation'))))

  )
)


