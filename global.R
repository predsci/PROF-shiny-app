
if(!require(pacman)){
  install.packages("pacman")
}
required_packages <- c('shiny','shinyjs','shinythemes','dplyr','ggplot2','plotly',
                       'lubridate','deSolve','PROF','scoringutils')

pacman::p_load(required_packages, character.only = TRUE)

# date of update

date_updated = "February 16, 2024"

# locations
loc_abbv <- loc_pops$abbreviation
loc_name <- loc_pops$location_name

# for now we remove the US and VI
ind = which(loc_abbv %in% c('US', 'VI'))

loc_abbv <- loc_abbv[-ind]
loc_name <- loc_name[-ind]

year_data <- c(2021, 2022, 2023)
year_label <- c('2021-22', '2022-23', '2023-24')
year_list <- as.list(year_data)

names(year_list) <- year_label

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
mycolor_list <- list('covid19' = "#F8766D", 'influenza'= "#00BFC4",
                      'combined' = "#CC79A7") #= "#D55E00",

# Function to add transparency to colors
add_transparency <- function(color, alpha) {
  # Convert hexadecimal color code to RGB format
  rgb_vals <- col2rgb(color) / 255

  # Append alpha value
  rgb_vals <- c(rgb_vals, alpha)

  # Convert back to hexadecimal format
  rgba_color <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = rgb_vals[4])

  return(rgba_color)
}

# Transparency values (adjust as needed)
alpha_values <- c(0.5, 0.5, 0.8)

mycolor_list_with_transparency <- Map(add_transparency, mycolor_list, alpha_values)


# --- Data Prep --------------------------
# identify data filename and modified date
data_file = list.files(path="data", pattern="HHS_daily-hosp_state__[0-9]{12}\\.csv$")
data_modified_date = filename_timestamp_to_posix(data_file)
# query HHS dataset modified date
hhs_data_date = fetch_hhs_last_modified()

if (hhs_data_date > data_modified_date) {
  cat("Local data file is out-of-date. Attempting to update.\n")
  down_result = fetch_hhs_data(down_dir="data")
  if (down_result$out_flag==0) {
    cat("HHS data file successfully updated.")
    data_file = down_result$download_path
  } else {
    cat("There was a problem with the download. HHS data not updated.")
    data_file = file.path("data", data_file)
  }
} else {
  data_file = file.path("data", data_file)
}


