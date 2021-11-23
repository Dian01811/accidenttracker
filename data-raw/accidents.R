# Data sourced from https://www.kaggle.com/sobhanmoosavi/us-accidents then
# filtered for only data from 2019 to make the file size manageable
# and removed unwanted columns, then saved as US_Accidents2019.csv

raw_accident_data <- readr::read_csv("data-raw/US_Accidents2019.csv")

accidents <- raw_accident_data %>%
  dplyr::select(-(...1)) %>% # removes extra row # column added when reading file
  # renaming columns to match class style guide
  dplyr::rename(id = ID, severity = Severity, time = Start_Time, lat = Start_Lat,
         lng = Start_Lng, street = Street, side = Side, city = City,
         county = County, state = State, zip = Zipcode, timezone = Timezone,
         wthr.time = Weather_Timestamp, temp = `Temperature(F)`,
         wind.chill = `Wind_Chill(F)`, humid = `Humidity(%)`,
         pressure = `Pressure(in)`, vis = `Visibility(mi)`,
         wind.dir = Wind_Direction, wind.spd = `Wind_Speed(mph)`,
         precip = `Precipitation(in)`, wthr.cond = Weather_Condition,
         amenity = Amenity, bump = Bump, cross = Crossing, give.way = Give_Way,
         junction = Junction, no.exit = No_Exit, rail = Railway,
         roundabt = Roundabout, station = Station, stop = Stop,
         traffic.calm = Traffic_Calming, traffic.sgnl = Traffic_Signal,
         turn.loop = Turning_Loop, day.night = Sunrise_Sunset) %>%
  dplyr::filter(!is.na(lat), !is.na(lng), !is.na(id)) %>% # remove missing values
  # use time column to add separate year, month, day, and hour columns
  dplyr::mutate(year = lubridate::year(time),
                month = lubridate::month(time, label = TRUE),
                day = lubridate::day(time),
                hour = lubridate::hour(time)) %>%
  # categorize weather condition into different broad weather categories in a new column
  dplyr::mutate(wthr.cat = wthr.cond) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Fair",
                                                             "Cloudy",
                                                             "Mostly Cloudy",
                                                             "Partly Cloudy",
                                                             "N/A Precipitation",
                                                             "Overcast",
                                                             "Clear",
                                                             "Scattered Clouds"),
                                   "Fair / Cloudy")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Light Rain",
                                                             "Rain / Windy",
                                                             "Heavy Rain",
                                                             "Light Drizzle",
                                                             "Light Rain / Windy",
                                                             "Drizzle", "Rain",
                                                             "Light Rain Shower",
                                                             "Heavy Rain / Windy",
                                                             "Showers in the Vicinity",
                                                             "Heavy Drizzle",
                                                             "Light Drizzle / Windy"),
                                   "Rain")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Light Snow",
                                                             "Heavy Snow",
                                                             "Wintry Mix",
                                                             "Light Snow / Windy",
                                                             "Snow / Windy",
                                                             "Light Snow and Sleet",
                                                             "Snow and Sleet",
                                                             "Heavy Snow / Windy",
                                                             "Blowing Snow / Windy",
                                                             "Blowing Snow",
                                                             "Wintry Mix / Windy",
                                                             "Light Snow with Thunder",
                                                             "Snow and Sleet / Windy",
                                                             "Heavy Snow with Thunder"),
                                   "Snow")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Haze",
                                                             "Mist",
                                                             "Shallow Fog",
                                                             "Patches of Fog",
                                                             "Fog / Windy",
                                                             "Haze / Windy",
                                                             "Drizzle and Fog",
                                                             "Partial Fog",
                                                             "Smoke",
                                                             "Smoke / Windy",
                                                             "Light Freezing Fog"),
                                   "Fog")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Fair / Wind",
                                                             "Cloudy / Windy",
                                                             "Partly Cloudy / Windy",
                                                             "Mostly Cloudy / Windy",
                                                             "Blowing Dust / Windy",
                                                             "Blowing Dust",
                                                             "Sand / Dust Whirlwinds",
                                                             "Thunder / Windy",
                                                             "Squalls / Windy",
                                                             "Thunder and Hail / Windy",
                                                             "Tornado",
                                                             "Fair / Windy"),
                                   "Windy")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Heavy T-Storm",
                                                             "T-Storm",
                                                             "Thunder in the Vicinity",
                                                             "T-Storm / Windy",
                                                             "Thunder",
                                                             "Light Rain with Thunder",
                                                             "Heavy T-Storm / Windy",
                                                             "Heavy Thunderstorms and Rain",
                                                             "Light Thunderstorms and Rain",
                                                             "Thunderstorms and Rain"),
                                   "Thunder Storm")) %>%
  dplyr::mutate(wthr.cat = replace(wthr.cat, wthr.cat %in% c("Light Freezing Drizzle",
                                                             "Light Freezing Rain",
                                                             "Freezing Rain",
                                                             "Small Hail",
                                                             "Ice Pellets",
                                                             "Light Ice Pellets",
                                                             "Sleet"),
                                   "Ice / Freezing Rain"))


usethis::use_data(accidents, overwrite = TRUE)
