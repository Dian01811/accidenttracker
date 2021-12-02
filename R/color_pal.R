color_pal <- function(a){
  switch(a,
         "None" =  colorBin("black", domain = NULL),
         "Severity" = colorFactor("YlOrRd", domain = us_accidents$severity),
         "Temperature (F)" = colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp, 
                                      bins = c(-50, 0, 32, 50, 80, 100, 175)),
         "Precipitation" = colorBin("BrBG", domain = us_accidents$precip, 
                                    bins = c(0, 0.001, 0.05, 0.1, 0.3, 1, 25)),
         "Day/Night" = colorFactor("Dark2", domain = us_accidents$day.night),
         "Visibility" = colorBin("YlGnBu", domain = us_accidents$vis, 
                                 bins = c(0, 0.5, 2, 150))
  )
}