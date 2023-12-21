prepNetWeight <- function(sheet = sheet){
  # Calculation of net food loss and waste weight subtracting pail and strainer weight
  # Number of observations and closed day at the restaurant ----------------------
  openDays  <- sum(!sheet$isClosed)
  closeDays <- sum(sheet$isClosed)
  obsDays   <- openDays + closeDays
  
  if(!(nrow(sheet) == obsDays))
    print("Error! Number of observations.")
  
  # Gross -> Net food loss and waste -------------------------------------------
  # remove Pail(buckets) and strainer weights 
  BUCKET_KG = 1     # Bucket weight is 1 kg
  STRAINER_KG = 0.3 # Strainer weight is 0.3 kg
  
  # Create new vectors for food loss and waste observations
  netFoodLossKg <- netFoodWasteKg <- netLiquidWasteKg <- netSolidWasteKg <- numeric()
  
  # Bucket weight for food loss is 1 kg
  netFoodLossKg <- c(netFoodLossKg, 
                     ifelse(sheet$soup_stock == 0, 
                            sheet$soup_stock,
                            sheet$soup_stock - BUCKET_KG))
  
  # All food waste: Bucket and strainer weight is 1 kg and 0.3 kg
  netFoodWasteKg <- c(netFoodWasteKg, 
                      ifelse(sheet$noodle_soup == 0, 
                             sheet$noodle_soup, 
                             sheet$noodle_soup - (BUCKET_KG + STRAINER_KG)))
  
  # Liquid: Bucket and strainer weight is 1 kg + 0.3 kg
  netLiquidWasteKg <- c(netLiquidWasteKg, 
                        ifelse(sheet$soup == 0,
                               sheet$soup,
                               sheet$soup - (BUCKET_KG + STRAINER_KG)))
  
  # Solid := all - liquid food waste
  netSolidWasteKg <- netFoodWasteKg - netLiquidWasteKg
  
  # check negative values ------------------------------------------------------
  if(!(all((netFoodLossKg    >= 0)| 
           (netFoodWasteKg   >= 0)|
           (netLiquidWasteKg >= 0)| 
           (netSolidWasteKg  >= 0) == TRUE)))
    print("Error: the date has negative values.")
  
  ## Data Frame ------------------------------------------------------------------
  return(
    data.frame(
      # Obs. date and the restaurant close or not
      date      = sheet$date, #1
      is_closed = as.logical(sheet$isClosed),     #2
      day       = as.character(sheet$day),        #3 day of the week
      # food loss and waste at the restaurant
      food_loss_kg    = netFoodLossKg,    #4
      food_waste_kg   = netFoodWasteKg,   #5
      liquid_waste_kg = netLiquidWasteKg, #6
      solid_waste_kg  = netSolidWasteKg  #7
      # # types of orders
      # full_orders    = as.numeric(sheet$ramen_orders),    #8
      # half_orders    = as.numeric(sheet$mini_orders),    #9
      # takeout_orders = as.numeric(sheet$takeouts),#10
      # # Business variables
      # customers  = as.numeric(sheet$customers),   #11
      # liquors    = as.numeric(sheet$liquors),     #12
      # salesD     = as.numeric(sheet$salesD),      #13
      # container  = as.numeric(sheet$container),   #14
      # # weather conditions
      # tempC        = as.numeric(sheet$avg_hourly_temperature),   #15
      # humidityPerc = as.numeric(sheet$avg_hourly_relative_humidity),#16
      # precipMM     = as.numeric(sheet$precipitation) #17
  ))
}