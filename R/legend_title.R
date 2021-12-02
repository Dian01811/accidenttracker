legend_title <- function(c){
  switch(c,
         "None" = "None",
         "Severity" = "Accident Severity",
         "Temperature (F)" = "Temperature (F)",
         "Precipitation" = "Precipitation",
         "Day/Night" = "Day/Night",
         "Visibility" = "Visibility",
  )}