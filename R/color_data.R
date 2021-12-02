color_data <- function(b){
  switch(b,
         "None" = NULL,
         "Severity" = us_accidents$severity,
         "Temperature (F)" = us_accidents$temp,
         "Precipitation" = us_accidents$precip,
         "Day/Night" = us_accidents$day.night,
         "Visibility" = us_accidents$vis,
  )
}
