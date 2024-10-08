#
# Example use of PROF with an ensemble (i.e. mixed) mechanistic/statistical forecast
#

library(PROF)
library(plotly)
library(deSolve)

# download the most recent HHS Hospitalization file

result = fetch_hhs_data(down_dir="~/Downloads")

# set state and season, and extract data
state = "CA"
season = 2023
prof_data = hhs_2_PROF(hhs_path=result$download_path, season = season, state=state)

# The 'prof_data' data structure should now be available and the data can
# be plotted

# To plot the data to a screen use:

plot_prof_data(prof_data = prof_data)

# select a single disease for this example, 'covid19' or 'influenza'
disease = 'influenza'

# Use default values for start and end of fitting window

prof_data = hhs_set_fitdates(prof_data=prof_data,
                             fit_start=NULL, fit_end=NULL)

# load default parameter file for 'influenza' with 'sirh' compartmental model
par_list = init_par_list(diseases=disease,
                         models=c("sirh"))
#############
## Fit
#############

# Fit a mechanistic model to influenza time-series using a model with 3 values for
# force of infection
fit_list <- fit_data(prof_data = prof_data[disease], par_list = par_list, nb_vec=3)

# to plot the results of the fit to the screen use these two calls:

plot_fit_list <- plot_fit(prof_data = prof_data[disease], par_list = par_list, fit_list = fit_list)

plot_fit_list$arrange_plot

# repeat the procedure for using a baseline statistical model fit to time series

prof_data = hhs_set_fitdates_stat(prof_data=prof_data[disease], fit_start=NULL, fit_end=NULL)

stat_fit_list <- plot_stat_fit(prof_data = prof_data[disease], ntraj = 1e4, filename = NULL)

stat_fit_list$arrange_plot

#############
## Forecast
#############

# Create and plot (to screen) mechanistic forecast

forecast_list <- plot_forecast(prof_data = prof_data[disease], par_list = par_list, fit_list = fit_list, nfrcst = 28)

forecast_list$arrange_plot

# Create and plot (to screen) baseline statistical forecast

forecast_stat_list <- plot_stat_forecast(prof_data = prof_data[disease], nfrcst = 28)

forecast_stat_list$arrange_plot

##########################
## Combine to create an Ensemble
##########################

forecast_mix_list <- plot_mixed_forecast(prof_data = prof_data, forecast_list = forecast_list, forecast_stat_list = forecast_stat_list)

# Display the results to the screen
forecast_mix_list$arrange_plot


######





