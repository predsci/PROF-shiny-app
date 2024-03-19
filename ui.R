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

             h4(paste0("Version 0.0.1000 | Released ",date_updated)),
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


    #### Landing Page ####

    tabPanel("1. Explore Incidence",
             h4('Model Daily Hospitlizatation data from HHS or upload your own hopitalization data'),
             tabsetPanel(
               tabPanel("HHS Data",
             fluidPage(
               h2('Explore Daily COVID-19 and Influenza Hospitalization Data'),
               h4("The data file provided with your PROF-shiny application may need an update, check the message below and consider our suggestion
               for an update if needed. Use the Dropdown menus to select a location and a season. When you complete your selection, click the Plot button.
                You can proceed to the Fitting tab only after plotting is completed. Explore the data by hovering over it.
                  You can save the incidence data and the plots to your computer."),
               hr(),
               fluidRow(
                 br(),
                 column(8, h4(htmlOutput(outputId="data_message"))),
                 column(4, actionButton(inputId="downloadDataButton",
                                        label="Download Data")),
                 br(),
                 htmlOutput("loading_message_1a")
               ),
               hr(),
             fluidRow(
               br(),
               column(6, selectInput("location", "Select Location:", choices = loc_abbv, selected = 'CA')),
               # column(2,""),
               column(6,selectInput("season", "Select Season:", choices = year_list, selected = 2023)),
               # column(4,""),
               column(6,actionButton("plotDataButton", "Plot Incidence Data")),
               column(3,""),
               br(),
               br(),
               htmlOutput("loading_message_1b"),
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
             ),
             downloadButton("dlInc", "Save Incidence Data"),
             h4("Incidence data can be saved after plots are made.")
             ) # End of fluidPage
    ),
    tabPanel("User Uploaded Data",
             fluidPage(
               h2("Exploring User Uploaded Data"),
               h4("Please provide the following information: (i) Population size (as an integer; scientific notation is accepted).
                  (ii) Location name (as a string).  After providing (i) and (ii) please press the Submit button. A message will
                  appear displaying the information received by PROF. You can then proceed to browse and select
                  the location your CSV data file. Once the data is uploaded, you can proceed to plot it."),
               p("For information on the CSV data file format specifications PROF see",tags$a(href="https://predsci.github.io/PROF/advanced/#uploading-your-own-data",'our web documentation')),
               hr(),
               column(6, numericInput("population", "Enter Population Size:", value = 3e5, min = 1e3, max = 40000000)),
               column(6,textInput("location_name", "Location Name:", value = "Enter Name")),
               column(12, div(style="text-align:center;", actionButton("submit", "Submit"))),
               h4("Press the submit button after providing location name and population."),
               hr(),
               div(textOutput("message"), style = "font-size: 20px;"),
               br(),
               hr(),
               fileInput("fileCSV", "Choose CSV File", accept = ".csv"),
               h4("Browse your computer to the location of your data file. Proceed to plotting after data upload is completed."),
               br(),
               column(6,actionButton("plotUserDataButton", "Plot Incidence Data")),
               column(12,plotlyOutput("plot1u")),
               tags$head(
                 tags$style(
                   HTML("
            #plot1u {
              transition: opacity 0.5s ease-in-out;
            }
            #plot1u.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot1u').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot1u').removeClass('fading');
          });
        ")
               ),
               column(12,plotlyOutput("plot2u")),
               tags$head(
                 tags$style(
                   HTML("
            #plot2u {
              transition: opacity 0.5s ease-in-out;
            }
            #plot2u.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#plot2u').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#plot2u').removeClass('fading');
          });
        ")
               ),
               br(),
               br(),
               column(12,downloadButton("dlIncUsr", "Save Incidence Data")),
               # h4('Enter population size for your data')
             )))), # End of Incidence Panel

tabPanel("2. Fit Incidence", # The Fitting Tab has the Mechanistic and Statistical tabs under it.
         h4("The Mechanistic Model takes about 10-15 minutes to run. During this time, you cannot make any changes or new selections. The Statistical model is nearly instantaneous. If you would like to evaluate a Forecast you must select an end-date that is prior to the end of the data stream and we strongly recommed using the same end-date for both Mechanistic and Statistical Fitting."),
tabsetPanel(
    tabPanel("Mechanistic",
             fluidPage(
               h2("Fitting Mechanistic Compartmental Models To Data"),
               h4("Setup a compartmental model for each pathogen. PROF supports two models: Susceptible-Exposed-Infectious-Recovered/Hospitalized (SEIRH)
               and Susceptible-Infectious-Recovered/Hospitalized (SIRH). You can proceed to the Forecasting Tab only after the fitting is completed.
                  You can save the plots/data to your computer."),
               hr(),
               column(12,checkboxGroupInput("disease", "Select Pathogens:", choices = c("COVID19"='covid19', "INFLUENZA"='influenza'), inline = TRUE)),
               h4("You can select both pathogens or only one of them. When a pathogen is selected you will be prompted to define the specifics of the compartmental model."),
               column(6,conditionalPanel(
                 condition = "input.disease.indexOf('covid19') !== -1",
                 checkboxGroupInput("options_cov", "Select Compartmental Model for COVID19:",choices  = c("SEIRH"='seirh', "SIRH"='sirh'), inline = TRUE),
                 h5('For COVID-19 we recommend selecting the Sucscptible-Exposed-Infectious-Recovered-Hospitalized option'),
                 checkboxGroupInput("nb_cov", "Select Number of Values for the time-dependent Force of Infection:", choices = c(2,3), inline = TRUE),
                 h5('We recommend starting with using a two-value FOI and changing to a three-value only if needed.'),
                 dateInput("cov_start_fit", "Select Start Date For Fitting COVID19:", min = cov_start_fit_date_min[nyear], max= cov_start_fit_date_max[nyear],
                           value = cov_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('For COVID-19 examine the time-series and decide if to start the fit in the summer or later.')
               )),
                column(6,conditionalPanel(
                 condition = "input.disease.indexOf('influenza') !== -1",
                 checkboxGroupInput("options_flu", "Select Compartmental Model for Influenza:", choices = c("SIRH"='sirh', "SEIRH"='seirh'), inline=TRUE),
                 h5('For Influenza we recommend selecting the Sucscptible-Infectious-Recovered-Hospitalized option'),
                 checkboxGroupInput("nb_flu", "Select Number of Values for the time-dependent Force of Infection:", choices = c(2,3), inline = TRUE),
                 h5('Typically a model with two-values for the time-dependent FOI is suitable for fitting an entire Influenza season.'),
                 dateInput("flu_start_fit", "Select Start Date For Fitting Influenza:", min = flu_start_fit_date_min[nyear], max= flu_start_fit_date_max[nyear],
                           value = flu_start_fit_date_min[nyear],format = "yyyy-mm-dd"),
                 h5('For Influenza the default start date for each season is recommended.')
               )),
               br(),
               hr(style = "clear: both;"),
               column(12,dateInput("select_end_fit", "Select End Date For Fitting: ", min = end_fit_date_min[1], max= end_fit_date_max[nyear],
                                      value = end_fit_date_max[nyear],format = "yyyy-mm-dd")),
               h4("By default, we fit all available data. If you would like to evaluate the forecast quality by comparing it to reported data, please select an earlier end-date for the fit. Please note that the choice you make here for the fit end-date determines the start date of the forecast, and if you change your mind about this date, you will have to re-run the calculation which takes about 10-15 minutes. We strongly recommed using the same end-date for both Mechanistic and Statistical Fitting."),
               # Custom JavaScript to center the dateInput
               tags$script(HTML("
      $(document).ready(function() {
        $('#select_end_fit').parent().css('display', 'flex');
        $('#select_end_fit').parent().css('justify-content', 'center');
      });
    ")),
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
               htmlOutput("loading_message_2"),
               br(),
               plotlyOutput("plot3"),
               br(),
               h4("Black circles are reported data, line and shaded areas are the median, 50% and 95% confidence intervals."),
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
             ),
             downloadButton("dlFitMech", "Save Plots Data"),
             h4("Compartmental model results can be saved after plots are made.")
    ),
    tabPanel("Statistical",
             fluidPage(
               h2("Fitting A Baseline Statistical Model To The Data"),
               h4('This model aims to replicate the entire time-series, capturing uncertainty from day-to-day variations in the data. You can save the plots data to your computer.'),
               hr(),
               column(12, checkboxGroupInput("diseaseStat", "Select Pathogens", choices = c("COVID-19"='covid19', "INFLUENZA" = 'influenza'), inline = TRUE)),
               h4("You can select both pathogens or only one of them. Once you select a pathogen you will be prompted to select the start date for the fit."),
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
               column(12, dateInput("select_end_fit_stat", "Select End Date For Fitting: ", min = end_fit_date_min[nyear], max= end_fit_date_max[nyear],
                                    value = end_fit_date_max[nyear],format = "yyyy-mm-dd")),
               h4("By default, we fit all available data. If you would like to evaluate the forecast quality by comparing it to reported data, please select an earlier fit end-date. Please note that the choice you make here for the fit end-date determines the start date of the forecast, and if you change your mind about this date, you will have to re-run the calculation. For the statistical model, this process is nearly instantaneous. We strongly recommed using the same end-date for both Mechanistic and Statistical Fitting."),
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
               plotlyOutput("plot4"),
               br(),
               h4("Black circles are reported data, line and shaded areas are the median, and quantiles corresponding to the 50% and 95% intervals."),
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
             ),
             downloadButton("dlFitStat", "Save Plots Data"),
             h4("Statistical model results can be saved after plots are made.")
    )
    )
),
tabPanel("3. Create Forecast", # The Forecasting Tab has the Mechanistic and Statistical tabs under it.
         tabsetPanel(
    tabPanel("Mechanistic",
             fluidPage(
               h2("Forecasting Using a Mechanistic Compartmental Model"),
               h4("For each fitted pathogen, the posterior distribution of the parameters will be
                  utilized to generate a forecast. Note that only pathogens that were fitted will be forecasted."),
               hr(),
               column(12,sliderInput("days_frcst", "Select Number of Days For Forecast:",
                                    min = 7, max = 42, value = c(28))),
               h4("Move the slider to select the number of days for the forecast."),
               br(),
               column(12,actionButton("forecastButton", "Mechanistic Forecast")),
               br(),
               br(),
               h4("Once you made your selection press the Forecast button."),
               hr(),
               htmlOutput("loading_message_4"),
               plotlyOutput("plot5"),
               br(),
               h4("Red/green bars are reported COVID-19/Influenza data, line and shaded areas are the median, and quantiles corresponding to the 50% and 95% intervals."),
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
        "),
               )
             ),
             downloadButton("dlFrcstMech", "Save Plots Data"),
             h4("Compartmental model results can be saved after plots are made.")
    ),
    tabPanel("Statistical",
             fluidPage(
               h2("Forecasting With A Baseline Statistical Model"),
               h4("This model aims to replicate the entire time-series, capturing uncertainty from day-to-day variations in the data.
               Median forecast values reproduce the seven last observed values."),
               hr(),
               column(12,sliderInput("days_frcst_stat", "Select Number of Days For Forecast:",
                           min = 7, max = 42, value = c(28))),
               br(),
               h4("Move the slider to select the number of days for the forecast."),
               br(),
               column(12,actionButton("forecastStatButton", "Statistical Forecast")),
               br(),
               h4("Once you made your selection press the Forecast button."),
               hr(),
               htmlOutput("loading_message_5"),
               br(),
               plotlyOutput("plot6"),
               br(),
               h4("Red/green bars are reported COVID-19/Influenza data, line and shaded areas are the median, and quantiles corresponding to the 50% and 95% intervals."),
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
             ),
             downloadButton("dlFrcstStat", "Save Plots Data"),
             h4("Statistical model results can be saved after plots are made.")
    ),
    tabPanel('Evaluate Forecast',
             fluidPage(
               h2("Evaluating Retrospective Forecasts"),
               h4("We assess the accuracy of PROF’s probabilistic forecasts by employing the weighted interval score [1] as implemented in the scoringutils R package [2]. Smaller values of the WIS indicate forecasts that are more aligned with observations. The evaluation of the WIS uses daily data (and forecasts) and the same quantiles as mandated by the CDC FluSight Challenge. The weighted interval score can be evaluted only for retrospective forecasts."),


               hr(),
               br(),
               # uiOutput("wis_button"),
               column(12,actionButton("wis_button", "Evaluate WIS")),
               br(),
               br(),
               br(),
               plotlyOutput("wis_plot"),
               tags$head(
                 tags$style(
                   HTML("
            #wis_plot {
              transition: opacity 0.5s ease-in-out;
            }
            #wis_plot.fading {
              opacity: 0.2;
            }
          ")
                 )
               ),tags$script(
                 HTML("
          $(document).on('shiny:busy', function(event) {
            $('#wis_plot').addClass('fading');
          });
          $(document).on('shiny:idle', function(event) {
            $('#wis_plot').removeClass('fading');
          });
        ")
               ),
               br(),
               h4("We evaluate the WIS score using all data and all forecasts. Each pathogen is displayed in a different panel and different colors
                  are used to indicate different models, see legend."),
               br(),
               p("1. Evaluating epidemics forecasts in interval format, Bracher J, Ray EL, Gneiting T and Reich NG, ", tags$a(href="https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618","PLoS Comput Biol 17(2): e1008618")),
               p("2. R package", tags$a(href="https://cran.r-project.org/web/packages/scoringutils/index.html", "scoringutils"))
             ))
    ),
    downloadButton("dlWIS", "Save WIS Data")
    ),
tabPanel("About",
         fluidPage(
           h2("PROF"),
           p("PROF is an R package (with Fortran code) for fitting and forecasting infectious disease incidence.
                         The package ingests publicly available confirmed hospital admission data, fits mechanistic and statistical
                         models to the data, and provides short-term probabilistic forecasts. Currently, the package supports fitting
                         and forecasting the individual and combined burden of influenza and COVID-19 at the state level.
                         S[I]2HR and SE[I]2HR models are used to fit the two pathogens, and both models use a flexible,
                         time-dependent transmission term. A baseline statistical model is also offered for each pathogen.
                         Once the User selects a state, and either one or both viruses,
                         the PROF fitting procedure iteratively determines the joint posterior distribution of model parameters.
                         The joint posterior distribution is then used with the model to generate location-specific probabilistic
                         forecasts of the near-term number of hospital admissions. If both viruses are chosen, this procedure is done
                         twice, and the total hospital burden forecast is estimated by combining the trajectory profiles of each disease
                         in multiple ways, including random, ordered, and semi-ordered. If the statistical model is also chosen, each
                         pathogen is independently fitted with the model, and the combined burden is estimated."),
           p("For more on PROF see",tags$a(href="https://predsci.github.io/PROF/",'our web documentation')),
           h2('Acknowledgement'),
           p("The development of PROF is supported by CSTE throught the CDC cooporative agreement number NU38OT000297.
             PSI would also like to thank the modeling team at the CA Department of Public Health for their invaluable comments and suggestions.
             The PROF GUI is modeled after the State of California Communicable diseases Assesment Tool",
             tags$a(href="https://github.com/StateOfCalifornia/CalCAT",'CalCAT'))))

)


