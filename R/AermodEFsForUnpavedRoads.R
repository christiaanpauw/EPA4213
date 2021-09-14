#'@name EFpublicRoadUnpaved
#'@description Calculates size-specific particulate emissions (in grams) from an
#'  unpaved public road, per vehicle per kilometer travelled.
#'@param s Numeric. Surface silt content (%).
#'@param S Numeric. Mean vehicle speed (km/h).
#'@param M Numeric. Surface moisture content (%).
#'@param pm Character. Maximum particulate matter diameter.
#'@return Numeric. Emission factor in gram per kilometer.
#'
EFpublicRoadUnpaved <- function(s = NULL, S = NULL, M = NULL,
                                pm = c("2.5", "10", "30")) {

  if (is.null(s)) {
    stop("Please provide a value for 's' (surface silt content as %).")
  }
  if (s < 1.8 | s > 35) {
    stop("Value for 's' must be in the range of [1.8,35].")
  }

  if (is.null(S)) {
    stop("Please provide a value for 'S' (mean vehicle speed in km/h).")
  }
  # convert S from km/h to mph
  S <- S/1.60934
  if (S < 10 | S > 55) {
    stop("Value for 'S' must be in the range of [10,55].")
  }

  if (is.null(M)) {
    stop("Please provide a value for 'M' (surface moisture content as %).")
  }
  if (M < 0.03 | M > 13) {
    stop("Value for 'M' must be in the range of [0.03,13].")
  }

  k <- switch(pm,
              "2.5" = 0.18,
              "10" = 1.8,
              "30" = 6.0)
  a <- 1 # always = 1 for public roads
  c <- ifelse(pm == "30", 0.3, 0.2)
  d <- ifelse(pm == "30", 0.3, 0.5)
  C <- ifelse(pm == "2.5", 0.00036, 0.00047)

  # calculate according to EPA specs
  x <- (k*((s/12)^a)*((S/30)^d))/((M/0.5)^c) -C

  # convert from pound/mile to g/km
  x <- x * 281.9

  return(x)
}

#'@name calcEFforAermod
#'@description Uses function 'EFpublicRoadUnpaved' to calculate an emission
#'  factor suitable for input to Aermod.
#'@param EFx Numeric. Output from EFpublicRoadUnpaved().
#'@param nVph Numeric. Average number of vehicles per hour.
#'@param roadLength Numeric. Length of road in meter.
#'@param roadWidth Numeric. Width of road in meter.
#'
calcEFforAermod <- function(EFx, nVph, roadLength, roadWidth) {

  # calculate grams per hour for the defined piece of road
  x <- EFx * nVph * (roadLength/1000)

  # convert to grams per second (g/s)
  x <- x/(60*60)

  # convert to g/(s*(m^2)) (Aermod peculiarity)
  x <- x / (roadLength * roadWidth)

  return(x)
}

#' @examples

# (1) Calculate the PM2.5 emission factor for a 210 m stretch of road that is 2 m wide,
# has a silt content of 4.1 %, a surface moisture content of 1.8 %, and with a
# traffic load of 50 vehicles per hour travelling at 47 km/h on average.
EFx <- EFpublicRoadUnpaved(s = 4.1, S = 47, M = 1.8, pm = "2.5")
xAermod <- calcEFforAermod(EFx, nVph = 50, roadLength = 210, roadWidth = 2)

# (1) Calculate the daytime PM10 emission factor for a 100 m stretch of road that
# is 2 m wide, has a silt content of 6.0 %, a surface moisture content of 0.7 %,
# and with a daytime (16 hours) traffic load of 110 vehicles in total, travelling
# at 63 km/h on average.
EFx <- EFpublicRoadUnpaved(s = 6.0, S = 63, M = 0.7, pm = "10")
xAermod <- calcEFforAermod(EFx, nVph = 110/16, roadLength = 100, roadWidth = 2)
