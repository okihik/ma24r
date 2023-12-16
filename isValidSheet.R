isValidSheet <- function(sheet = sheet){
  isValid <- TRUE
  # Check for existence of sheet
  if(!exists("sheet")){
    isValid <- !isValid
    print("The object sheet does not exist.")
  }
  
  # check the sheet is data frame:
  # including the same number of columns and rows
  if(!inherits(sheet, "data.frame")){
    print("The sheet is not data frame format.")
  }
  
  # Check if isClose is 0 or 1
  if(!inherits(sheet$isClosed, "logical")){
    isValid <- !isValid
    print("The isClosed column is not logical.")
  }
  
  # Check for Date
  if(!inherits(sheet$date, "POSIXct")){
    isValid <- !isValid
    print("The date sheet is not Date format.")
  }
  
  # Check for negative values for measurements or observations
  if(!(all((sheet$fl|sheet$slfw|sheet$lfw|
            sheet$customers|sheet$full|sheet$half|
            sheet$liquors|sheet$sales|sheet$takeouts|
            sheet$container >= 0) == TRUE))){
    isValid <- !isValid
    print("The date retrieved sheet has negative values.")
  }
  
  # Check if weather conditions (temperature) is between -50 to 50
  if(!(all(dplyr::between(sheet$temp,-50,50)))){
    isValid <- !isValid
    print("The date retrieved sheet has negative values.")
  }
  # # Check if weather conditions (temperature) is between -50 to 50
  # if(!(all(between(sheet$tMax,-50,50)|
  #          between(sheet$tMin,-50,50)|
  #          between(sheet$hrTM,-50,50)|
  #          between(sheet$mmTM,-50,50)))){
  #   isValid <- !isValid
  #   print("The date retrieved sheet has negative values.")
  # }
  
  # Check if weather conditions (humiditiy) is between 0 to 100
  if(!(all(dplyr::between(sheet$humi,0,100)))){
    isValid <- !isValid
    print("The humidity date is not percentage.")
  }
  
  # Check if precipitation has negative value
  if(!(all((sheet$precip >= 0) == TRUE))){
    isValid <- !isValid
    print("The precipitation date has negative values.")
  }
  return(isValid)
}