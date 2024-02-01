# Load required libraries
# library(shiny)
# library(shinyjs)
# library(shinythemes)
# library(dplyr)
# library(ggplot2)
# library(plotly)
# library(lubridate)
# library(deSolve)
# library(PROF)

# download data

# download HHS hospitalizations file
# result <- hhs_hosp_state_down(down_dir="~/Downloads")

if(!require(pacman)){
  install.packages("pacman")
}
required_packages <- c('shiny','shinyjs','shinythemes','dplyr','ggplot2','plotly',
                       'lubridate','deSolve','PROF')

pacman::p_load(required_packages, character.only = TRUE)

# locations
loc_abbv <- loc_pops$abbreviation
loc_name <- loc_pops$location_name

# for now we remove the US and VI
ind = which(loc_abbv %in% c('US', 'VI'))

loc_abbv <- loc_abbv[-ind]
loc_name <- loc_name[-ind]

year_data <- c(2021, 2022, 2023)
nyear <- length(year_data)

# Define default end dates for fitting
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
end_fit_date_min <- c(as.Date(paste0(year_data,'-10-01')))
end_fit_date_max <- c(as.Date(paste0(year_data+1,'-06-01')))
end_fit_date_max[nyear] <- Sys.Date()

cov_start_fit_date_min <- c(as.Date('2021-10-15'), as.Date('2022-10-15'),as.Date('2023-07-01'))
cov_start_fit_date_max <- c(as.Date('2021-11-15'), as.Date('2022-11-15'),as.Date('2023-10-15'))

flu_start_fit_date_min <- c(as.Date('2021-09-01'), as.Date('2022-09-01'),as.Date('2023-09-01'))
flu_start_fit_date_max <- c(as.Date('2021-11-15'), as.Date('2022-11-15'),as.Date('2023-10-15'))

# Define colors for plots
mycolor_list <- list('covid19' = "#0072B2", 'influenza'= "#FC4E07",
                      'combined' = "#CC79A7") #= "#D55E00",
