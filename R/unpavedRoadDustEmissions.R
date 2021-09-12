#' @title pavedRoadDustEmissionFactor
#' @description Function to calculate dust emissions from unpaved roads
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @param massUnit Character. One of c("g", "lb"). Default: "g"
#' @param distanceUnit Character. One of c("km", "ml")
#' @param size Character. One of c("PM2.5", "PM10", "PM15", "PM30"). Default:  "PM10"
#' @param s Numeric. surface material silt content (%)
#' @param W Numeric. mean vehicle weight (tons)
#' @param M Numeric. surface material moisture content (%)
#' @param S Numeric.mean vehicle speed
#' @param M Numeric. Surface material moisture content (%)
#' @param type Character. What type of unpaved road is it? Choose between  "unpaved-industrial" and "unpaved-public"
#' @return Numeric. E. size-specific emission factor (lb/VMT)
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
                                   outunit =c("g/VKT", "lb/VMT")[1]){
  if (any(purrr::map_lgl(as.list(sys.call()), ~is.null(.)))) stop("No argument may be NULL\n", paste(as.list(sys.call()), " "))

  if (!inherits(W, "units")) stop("W must have a unit of mass")
  if (!inherits(S, "units")) stop("S must have a unit of speed")
  if (s < 0 | s > 100) stop("s must be a percentage")
  if (M < 0 | M > 100) stop("M must be a percentage")

  message("W received in units of ", units(W))
  W <- set_units(W, "ton")
  message("W converted in units of ", units(W))

  message("S received in units of ", units(S))
  S <- set_units(S, m/h)
  message("S converted in units of ", units(S))

  C <- makeFleetCLookup() %>% filter(size == {{size}}) %>% pull(C)

  message("C is ", C)

  refdf <- makeVehicleDistanceLookup(type =type) %>%
    filter(massUnit == "lb" & distanceUnit == "ml" & size == {{size}})

  if (type == "unpaved-industrial") {
    k <- pull(refdf, k)
    a <- pull(refdf, a)
    b <- pull(refdf, b)
    s <- drop_units(s)
    W <- drop_units(W)

    E <-  k * ( (s/12)^a ) * ((W/3)^b)
  }

  if (type == "unpaved-public") {
    k <- pull(refdf, k)
    a <- pull(refdf, a)
    c <- pull(refdf, c)
    d <- pull(refdf, d)
    s <- drop_units(s)
    S <- drop_units(S)
    M <- drop_units(M)
    bo  <- k * ( (s/12)^a ) * ( (S/30) ^d)
    onder <- (M/0.5)^c

    E <- bo / onder - C

    if (outunit == "g/VKT") E <- E *  281.9

    E
  }







  E



}


