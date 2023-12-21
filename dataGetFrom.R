dataGetFrom <- function(URL = URL){
  library(googlesheets4) # google sheets
  library(gargle)
  
  # Retrieve data from Google sheet ----------------------------------------------
  options(gargle_oauth_cache = ".secrets")
  list.files(".secrets/")
  # gs4_auth(cache = ".secrets",
  #          email = "unbcmori@mail.com") # Authentification
  gs4_auth()
  # ss <- gs4_get(URL)
  
  
  # URL <- "https://docs.google.com/spreadsheets/d/1QDdgc_fR1_iALRschw7ymXOE7EJmEqAP_Xp_b6mwHP4/edit?usp=sharing"
  
  raw_sheet <- read_sheet(URL,
                          col_names = TRUE,
                          col_types = "Dcldddiiiidii",
                          range = "RAW!A1:M170")
  # import weather data ----------------------------------------------------------
  weather_sheet <- read_sheet(URL,
                              col_names = TRUE,
                              range = "pgweather230402retrieved!A1:BR1001")
  # Combine two sheet sorted by "date" -------------------------------------------
  sheet <- dplyr::left_join(raw_sheet, weather_sheet, by = "date")
  
  # Eliminate unnecessary columns
  sheet <- sheet[c(1:13,15,21,55)]
  # rename columns  # <- "old names"
  colnames(sheet) <- c("date",      # <- date
                       "day",       # <- day
                       "isClosed",  # <- isClosed
                       "soup_stock",      # <- soup_stock
                       "noodle_soup",    # <- noodle_soup
                       "soup",     # <- soup
                       "customers", # <- customers
                       "ramen_orders",      # <- ramen_orders
                       "mini_orders",      # <- mini_orders
                       "liquors",   # <- liquors
                       "salesD",    # <- sales
                       "takeouts",  # <- takeouts
                       "container", # <- togo
                       "avg_hourly_temperature",     # <- avg_hourly_temperature
                       "avg_hourly_relative_humidity",  # <- avg_hourly_relative_humidity
                       "precipitation"   # <- precipitation
  )
  
  
  # Create data frame ------------------------------------------------------------
  return(data.frame(sheet))
}