#' @title pavedRoadDustEmissionFactor
#' @description Function to calculate dust emissions from paved roads
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @param massUnit Character. One of c("g", "lb"). Default: "g"
#' @param distanceUnit Character. One of c("km", "ml")
#' @param size Character. One of c("PM2.5", "PM10", "PM15", "PM30"). Default:  "PM10"
#' @param sL Numeric. road surface silt loading (grams per square meter) (g/m2)
#' @param W Numeric. average weight (tons) of the vehicles traveling the road
#' @param precipitationPeriod Character. One of c("day", "hour")
#' @param P Numeric. Number of “wet” days or hours (see precipitationPeriod) with at least 0.254 mm (0.01 in) of precipitation during the averaging period.
#' @param N Numeric. Number of days or hours (see precipitationPeriod) in the averaging period
#' e.g., 365 for annual, 91 for seasonal, 30 for monthly days or
#' 8760 for annual, 2124 for seasonal, 720 for monthly hours
#' @return  Numeric. Particulate emission factor (having units matching the units of k)
#' @import units
#' @import dplyr
#' @import magrittr
#' @export


pavedRoadDustEmissionFactor <- function(massUnit = c("g", "lb")[1],
                                   distanceUnit = c("km", "ml")[1],
                                   size = c("PM2.5", "PM10", "PM15", "PM30")[2],
                                   sL = NULL,
                                   W = NULL,
                                   precipitationPeriod = NULL,
                                   P = NULL,
                                   N = 365){
  k <- makeVehicleDistanceLookup() %>% filter(massUnit == {{massUnit}} &
                             distanceUnit == {{distanceUnit}} &
                             size == {{size}}) %>% dplyr::select(k) %>%
    as.data.frame() %>% `[`(1)
  k <- as.numeric(k)

  if (!class(W) %in% "units") stop("W must be in a unit of mass")

  units(W) <- make_units(t)

  E <- k * (sL/2)^0.65 * (as.numeric(W)/3)^1.5
  u <- sprintf("%s/%s", massUnit, distanceUnit)
  units(E) <- as_units(u)

  if (!is.null(precipitationPeriod)){
    #message("precipitationPeriod is nie null nie dis ", precipitationPeriod)
    if (is.null(N) | is.null(P)) stop("P and N cannot be NULL")
    if (precipitationPeriod %in% c("day", "days", "Day","Days")) {
      #message("Binne-in dag")
      E <- E * (1-(P/(4*N)))
      een = 1
      u2 <- switch(as.character(N), "365" = "year", "91" = "season", "30" = "month")
      units(een) <- as_units(u2)
      E <- E / een
    }
    if (precipitationPeriod %in% c("hour", "hours", "Hour", "Hours")) {
      message("Binne-in uur")
      E <- E * (1-( (1.2*P)/N ) )
      een = 1
      u2 <- switch(as.character(N), "8760" = "year", "720" = "month", "24" = "day")
      units(een) <- as_units(u2)
      E <- E / een
    }
  }
  E

}


