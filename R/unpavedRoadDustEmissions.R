#' @title pavedRoadDustEmissionFactor
#' @description Function to calculate dust emissions from unpaved roads
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @param massUnit Character. One of c("g", "lb"). Default: "g"
#' @param distanceUnit Character. One of c("km", "ml")
#' @param size Character. One of c("PM2.5", "PM10", "PM15", "PM30"). Default:  "PM10"
#' @param s Numeric. surface material silt content (percent)
#' @param W Numeric. mean vehicle weight (tons)
#' @param M Numeric. surface material moisture content (precent)
#' @param S Numeric.mean vehicle speed
#' @param M Numeric. Surface material moisture content (percent)
#' @param type Character. What type of unpaved road is it? Choose between  "unpaved-industrial" and "unpaved-public"
#' @param outunit Character. Either "g/VKT" or "lb/VMT"
#' @param verbose Logical. Messages or not
#' @param unsafespeed Logical. Ignore speed limits of
#' @return Numeric. E. size-specific emission factor in units of lb/VMT or g/VKT depending on value of outunit argument
#' @import units
#' @import dplyr
#' @import magrittr
#' @export

unpavedRoadDustEmissionFactor <- function(size = c("PM2.5", "PM10", "PM30")[2],
                                   s = NULL,
                                   W = NULL,
                                   S = NULL,
                                   M = NULL,
                                   type = c("unpaved-industrial", "unpaved-public")[2],
                                   outunit =c("g/VKT", "lb/VMT")[1],
                                   verbose = FALSE,
                                   unsafespeed = FALSE){
  if (!size %in% c("PM2.5", "PM10", "PM30")) stop("size must be one of c('PM2.5', 'PM10', 'PM30')")

  if (any(purrr::map_lgl(as.list(sys.call()), ~is.null(.)))) stop("No argument may be NULL\n", paste(as.list(sys.call()), " "))

  if (type == "unpaved-industrial") {
    if (!inherits(W, "units")) stop("W must have a unit of mass")
    message("W received in units of ", units::deparse_unit(W))
    W <- units::set_units(W, "ton")
    message("W converted in units of ", units::deparse_unit(W))
  }

  if (!inherits(S, "units")) stop("S must have a unit of speed")
  if (s < 0 | s > 100) stop("s must be a percentage")
  if (M < 0 | M > 100) stop("M must be a percentage")

  refdf <- makeVehicleDistanceLookup(type =type) %>%
    filter(massUnit == "lb" & distanceUnit == "ml" & size == {{size}})

  if (type == "unpaved-industrial") {
    k <- pull(refdf, k)
    a <- pull(refdf, a)
    b <- pull(refdf, b)
    s <- units::drop_units(s)
    W <- units::drop_units(W)

    E <-  k * ( (s/12)^a ) * ((W/3)^b)
  }

  if (type == "unpaved-public") {

    if (verbose) message("S received in units of ", units::deparse_unit(S))
    S <- units::set_units(S, miles/h)
    if (verbose) message("S converted in units of ", units::deparse_unit(S), "S")
    C <- makeFleetCLookup() %>% filter(size == {{size}}) %>% pull(C)
    if (verbose) message("C is ", C)

    if (s < 1.8 | s > 35) {
      stop("Value for 's' must be in the range of [1.8,35].")
    }

    if (S < units::set_units(10, miles/h) | S > units::set_units(55, miles/h)) {
      if (!unsafespeed){stop("Value for 'S' must be in the range of [10,55] miles per hour.")}

    }

    k <- pull(refdf, k)
    a <- pull(refdf, a)
    c <- pull(refdf, c)
    d <- pull(refdf, d)
    S <- units::drop_units(S)

    if (verbose) message("k: ", k, "\nc: ", c, "\nd: ", d , "\nS: ", round(S, 3),  "\ns: ", s,  "\nM: ", M)

    E <- (k*((s/12)^a)*((S/30)^d))/((M/0.5)^c) -C

    if (verbose) message("E is ", round(E, 6), " lb/VMT")

    if (outunit == "g/VKT") {
      E <- units::set_units(E *  281.9, g/km)
    } else {
      E <- units::set_units(E , lb/mile)
      }
  }

  E

}


