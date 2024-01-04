prepWeek <- function(sheet = sheet, baseDay = baseDay){
  
  # Number of observations and closed day at the restaurant ----------------------
  openDays  <- sum(!sheet$is_closed)
  closeDays <- sum(sheet$is_closed)
  obsDays   <- openDays + closeDays
  
  # Retrieve the set of day in the sheet
  set_week <- sets::as.set(sheet$day)
  
  ## Day of the week coding ----------------------------------------------------
  # create new vector for day of the week based on the date in the sheet
  week_day <- wday(sheet$date, label = TRUE)
  week_day <- factor(week_day, 
                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                     ordered = TRUE)
  week_day <- as.factor(week_day)
  
  tue <- wed <- thur <- fri <- sat <- sun <- numeric(length = obsDays)
  # wkend <- numeric(length = obsDays)
  
  # Convert baseDay
  # Dummy Coding: Assign 1; otherwise 0 ----------------------------------------
  switch(baseDay,
         "Sun" = {# Sunday is the based day -> 0
                  tue <- ifelse(week_day == "Tue", 1, 0)
                  wed <- ifelse(week_day == "Wed", 1, 0)
                  thu <- ifelse(week_day == "Thu", 1, 0)
                  fri <- ifelse(week_day == "Fri", 1, 0)
                  sat <- ifelse(week_day == "Sat", 1, 0)
                  #sun <- ifelse(week_day == "Sun", 1, 0)
                  # Combine each week of day vector
                  wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                           tueD = tue, wedD = wed, thuD = thu, 
                                           friD = fri, satD = sat)
                  
                  # Sunday is the effect day -> -1
                  tue <- ifelse(week_day == "Tue", 1, 
                                ifelse(week_day == "Sun", -1, 0))
                  wed <- ifelse(week_day == "Wed", 1, 
                                ifelse(week_day == "Sun", -1, 0))
                  thu <- ifelse(week_day == "Thu", 1, 
                                ifelse(week_day == "Sun", -1, 0))
                  fri <- ifelse(week_day == "Fri", 1, 
                                ifelse(week_day == "Sun", -1, 0))
                  sat <- ifelse(week_day == "Sat", 1, 
                                ifelse(week_day == "Sun", -1, 0))
                  # sun <- ifelse(week_day == "Sun", -1, 0)

                  # Combine each week of day vector
                  wkEffectMat <- data.frame(
                    date = sheet$date[1:obsDays],
                    tueE = tue, wedE = wed, thuE = thu, friE = fri, satE = sat)
         },
         "Sat" = {# Saturday is the based day -> 0
                  tue <- ifelse(week_day == "Tue", 1, 0)
                  wed <- ifelse(week_day == "Wed", 1, 0)
                  thu <- ifelse(week_day == "Thu", 1, 0)
                  fri <- ifelse(week_day == "Fri", 1, 0)
                  #sat <- ifelse(week_day == "Sat", 1, 0)
                  sun <- ifelse(week_day == "Sun", 1, 0)
                  
                  # Combine each week of day vector
                  wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                           tueD = tue, wedD = wed, thuD = thu, 
                                           friD = fri, sunD = sun)
                  
                  # Saturday is the effect day -> -1
                  tue <- ifelse(week_day == "Tue", 1, 
                                ifelse(week_day == "Sat", -1, 0))
                  wed <- ifelse(week_day == "Wed", 1, 
                                ifelse(week_day == "Sat", -1, 0))
                  thu <- ifelse(week_day == "Thu", 1, 
                                ifelse(week_day == "Sat", -1, 0))
                  fri <- ifelse(week_day == "Fri", 1, 
                                ifelse(week_day == "Sat", -1, 0))
                  # sat <- ifelse(week_day == "Sat", 1, 
                  #               ifelse(week_day == "Sun", -1, 0))
                  sun <- ifelse(week_day == "Sun", 1,
                                ifelse(week_day == "Sat",-1,0))
                  
                  # Combine each week of day vector
                  wkEffectMat <- data.frame(
                    date = sheet$date[1:obsDays],
                    tueE = tue, wedE = wed, thuE = thu, friE = fri, sunE = sun)
         },
         "Fri" = {# Friday is the based day -> 0
                  tue <- ifelse(week_day == "Tue", 1, 0)
                  wed <- ifelse(week_day == "Wed", 1, 0)
                  thu <- ifelse(week_day == "Thu", 1, 0)
                  #fri <- ifelse(week_day == "Fri", 1, 0)
                  sat <- ifelse(week_day == "Sat", 1, 0)
                  sun <- ifelse(week_day == "Sun", 1, 0)
                  # Combine each week of day vector
                  wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                           tueD = tue, wedD = wed, thuD = thu, 
                                           satD = sat, sunD = sun)
                  
                  # Friday is the effect day -> -1
                  tue <- ifelse(week_day == "Tue", 1, 
                                ifelse(week_day == "Fri", -1, 0))
                  wed <- ifelse(week_day == "Wed", 1, 
                                ifelse(week_day == "Fri", -1, 0))
                  thu <- ifelse(week_day == "Thu", 1, 
                                ifelse(week_day == "Fri", -1, 0))
                  # fri <- ifelse(week_day == "Fri", 1, 
                  #               ifelse(week_day == "Sun", -1, 0))
                  sat <- ifelse(week_day == "Sat", 1, 
                                ifelse(week_day == "Fri", -1, 0))
                  sun <- ifelse(week_day == "Sun", 1,
                                ifelse(week_day == "Fri", -1, 0))
                  
                  # Combine each week of day vector
                  wkEffectMat <- data.frame(
                    date = sheet$date[1:obsDays],
                    tueE = tue, wedE = wed, thuE = thu, satE = sat, sunE=sun)
         },
         "Thu" = {# Thursday is the based day -> 0
                  tue <- ifelse(week_day == "Tue", 1, 0)
                  wed <- ifelse(week_day == "Wed", 1, 0)
                  #thu <- ifelse(week_day == "Thu", 1, 0)
                  fri <- ifelse(week_day == "Fri", 1, 0)
                  sat <- ifelse(week_day == "Sat", 1, 0)
                  sun <- ifelse(week_day == "Sun", 1, 0)
                  # Combine each week of day vector
                  wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                           tueD = tue, wedD = wed, friD = fri, 
                                           satD = sat, sunD = sun)
                  
                  # Thurday is the effect day -> -1
                  tue <- ifelse(week_day == "Tue", 1, 
                                ifelse(week_day == "Thu", -1, 0))
                  wed <- ifelse(week_day == "Wed", 1, 
                                ifelse(week_day == "Thu", -1, 0))
                  # thu <- ifelse(week_day == "Thu", 1, 
                  #               ifelse(week_day == "Sun", -1, 0))
                  fri <- ifelse(week_day == "Fri", 1, 
                                ifelse(week_day == "Thu", -1, 0))
                  sat <- ifelse(week_day == "Sat", 1, 
                                ifelse(week_day == "Thu", -1, 0))
                  sun <- ifelse(week_day == "Sun", 1,
                                ifelse(week_day == "Thu", -1, 0))
                  
                  # Combine each week of day vector
                  wkEffectMat <- data.frame(
                    date = sheet$date[1:obsDays],
                    tueE = tue, wedE = wed, friE = fri, satE = sat, sunE = sun)
         },
         "Wed"= {# Wednesday is the based day -> 0
                 tue <- ifelse(week_day == "Tue", 1, 0)
                 #wed <- ifelse(week_day == "Wed", 1, 0)
                 thu <- ifelse(week_day == "Thu", 1, 0)
                 fri <- ifelse(week_day == "Fri", 1, 0)
                 sat <- ifelse(week_day == "Sat", 1, 0)
                 sun <- ifelse(week_day == "Sun", 1, 0)
                 # Combine each week of day vector
                 wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                          tueD = tue, thuD = thu, firD = fri,
                                          satD = sat, sunD = sun)
                 # Wednesday is the effect day -> -1
                 tue <- ifelse(week_day == "Tue", 1, 
                               ifelse(week_day == "Wed", -1, 0))
                 # wed <- ifelse(week_day == "Wed", 1, 
                 #               ifelse(week_day == "Sun", -1, 0))
                 thu <- ifelse(week_day == "Thu", 1, 
                               ifelse(week_day == "Wed", -1, 0))
                 fri <- ifelse(week_day == "Fri", 1, 
                               ifelse(week_day == "Wed", -1, 0))
                 sat <- ifelse(week_day == "Sat", 1, 
                               ifelse(week_day == "Wed", -1, 0))
                 sun <- ifelse(week_day == "Sun", 1,
                               ifelse(week_day == "Wed", -1, 0))
                 
                 # Combine each week of day vector
                 wkEffectMat <- data.frame(
                   date = sheet$date[1:obsDays],
                   tueE = tue, thuE = thu, friE = fri, satE = sat, sunE=sun)
          },
         "Tue" = {# Tuesday is the based day -> 0
                  #tue <- ifelse(week_day == "Tue", 1, 0)
                  wed <- ifelse(week_day == "Wed", 1, 0)
                  thu <- ifelse(week_day == "Thu", 1, 0)
                  fri <- ifelse(week_day == "Fri", 1, 0)
                  sat <- ifelse(week_day == "Sat", 1, 0)
                  sun <- ifelse(week_day == "Sun", 1, 0)
                  # Combine each week of day vector
                  wkDummyMat <- data.frame(date = sheet$date[1:obsDays],
                                           wedD = wed, thuD = thu, firD = fri,
                                           satD = sat, sunD = sun)
                  # Tuesday is the effect day -> -1
                  # tue <- ifelse(week_day == "Tue", 1, 
                  #               ifelse(week_day == "Sun", -1, 0))
                  wed <- ifelse(week_day == "Wed", 1, 
                                ifelse(week_day == "Tue", -1, 0))
                  thu <- ifelse(week_day == "Thu", 1, 
                                ifelse(week_day == "Tue", -1, 0))
                  fri <- ifelse(week_day == "Fri", 1, 
                                ifelse(week_day == "Tue", -1, 0))
                  sat <- ifelse(week_day == "Sat", 1, 
                                ifelse(week_day == "Tue", -1, 0))
                  sun <- ifelse(week_day == "Sun", 1,
                                ifelse(week_day == "Tue", -1, 0))
                  
                  # Combine each week of day vector
                  wkEffectMat <- data.frame(
                    date = sheet$date[1:obsDays],
                    wedE = wed, thuE = thu, friE = fri, satE = sat, sunE=sun)
         }
  )
  
  wk_end <- data.frame(
    date  = sheet$date[1:obsDays],
    wkend = if_else(wday(sheet$date) %in% c(1,6,7), 1, -1)
  )
  
  sheet.merged <- merge(sheet[,c(1,3)], wkDummyMat,  by = "date")
  sheet.merged <- merge(sheet.merged,   wkEffectMat, by = "date")
  sheet.merged <- merge(sheet.merged,   wk_end,      by = "date")
  return(sheet.merged)
}