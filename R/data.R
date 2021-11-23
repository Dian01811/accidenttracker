#' Locations of US car accidents in 2019
#'
#' A dataset containing the locations, severity, dates, and other attributes
#' of car accidents in the United States in 2019.
#'
#' @format A data frame with 261772 rows and 36 variables:
#'  \describe{
#'  \item{id}{unique identifier of accident record}
#'  \item{severity}{severity of the accident, as a number between 1 and 4, with a 1 having the least impact on traffic}
#'  \item{time}{time of when accident occurred in local time zone, date-time format}
#'  \item{lat}{latitude in GPS coordinate}
#'  \item{lng}{longitude in GPS coordinate}
#'  \item{street}{name of street in address record}
#'  \item{side}{relative side of the road in address record}
#'  \item{city}{city in address record}
#'  \item{county}{county in address record}
#'  \item{state}{state in address record}
#'  \item{zip}{zipcode in address record}
#'  \item{timezone}{timezone based on accident location}
#'  \item{wthr.time}{time stamp of weather observation record in local timezone}
#'  \item{temp}{temperature, in Farenheit}
#'  \item{wind.chill}{wind chill, in Farenheit}
#'  \item{humid}{% humidity}
#'  \item{pressure}{air pressure, in inches}
#'  \item{vis}{visibility, in miles}
#'  \item{wind.dir}{wind direction}
#'  \item{wind.spd}{wind speed, in mph}
#'  \item{precip}{amount of precipitation, in inches}
#'  \item{wthr.cond}{weather condition, (fair, rain, snow, etc.)}
#'  \item{amenity}{TRUE or FALSE, TRUE indicates presence of amenity nearby}
#'  \item{bump}{TRUE or FALSE, TRUE indicates presence of a bump nearby}
#'  \item{cross}{TRUE or FALSE, TRUE indicates presence of a crossing nearby}
#'  \item{give.way}{TRUE or FALSE, TRUE indicates presence of a give way nearby}
#'  \item{junction}{TRUE or FALSE, TRUE indicates presence of a junction nearby}
#'  \item{no.exit}{TRUE or FALSE, TRUE indicates presence of no exit nearby}
#'  \item{rail}{TRUE or FALSE, TRUE indicates presence of a railway nearby}
#'  \item{roundabt}{TRUE or FALSE, TRUE indicates presence of a roundabout nearby}
#'  \item{station}{TRUE or FALSE, TRUE indicates presence of a station nearby}
#'  \item{stop}{TRUE or FALSE, TRUE indicates presence of a stop nearby}
#'  \item{traffic.calm}{TRUE or FALSE, TRUE indicates presence of traffic calming (such as a speed bump) nearby}
#'  \item{traffic.sgnl}{TRUE or FALSE, TRUE indicates presence of a traffic signal nearby}
#'  \item{turn.loop}{TRUE or FALSE, TRUE indicates presence of a turning loop nearby}
#'  \item{day.night}{indicates the time of day (i.e. day or night) based on sunrise/sunset}
#'  \item{year}{year when accident occurred}
#'  \item{month}{month when accident occurred as 3-letter abbreviation}
#'  \item{day}{day of the month when accident occurred}
#'  \item{hour}{hour when accident occurred, 24 hour format}
#'  \item{wthr.cat}{the weather condition categorized into one of seven broad categories: Fair/Cloudy, Rain, Fog, Windy, Thunder Storms, Ice/Freezing Rain, or Snow}
#'  }
#'  @source \url{https://www.kaggle.com/sobhanmoosavi/us-accidents}
"accidents"
