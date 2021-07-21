#' @name pavedRoadDustEmissions
#' @description Function to calculate dust emissions from paved roads
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @param massUnit Character. One of c("g", "lb"). Default: "g"
#' @param distanceUnit Character. One of c("km", "ml")
#' @param size Character. One of c("PM2.5", "PM10", "PM15", "PM30"). Default:  "PM10"
#' @param sL Numeric. road surface silt loading (grams per square meter) (g/m2)
#' @param W Numeric. average weight (tons) of the vehicles traveling the road
#' @return  Numeric. Particulate emission factor (having units matching the units of k)
#' @import units
#' @import dplyr
#' @export


pavedRoadDustEmissions <- function(massUnit = c("g", "lb")[1],
                                   distanceUnit = c("km", "ml")[1],
                                   size = c("PM2.5", "PM10", "PM15", "PM30")[2],
                                   sL = NULL,
                                   W = NULL){
  k <- dfLookup %>% filter(massUnit == {{massUnit}} & distanceUnit == {{distanceUnit}} & size == {{size}}) %>% dplyr::select(k) %>% as.data.frame() %>% `[`(1)
  k <- as.numeric(k)

  E <- k * (sL/2)^0.65 * (W/3)^1.5
  u <- sprintf("%s/s/%s", massUnit, distanceUnit)
  units(E) <- as_units(u)
  E

}


dfLookup <- expand.grid(massUnit = c("g","lb"), distanceUnit = c("km", "ml"), size = c("PM2.5", "PM10", "PM15", "PM30")) %>%
  filter(!(distanceUnit == "km" & massUnit == "lb")) %>%
  mutate(k = c(1.1, 1.8, 0.0040,
               4.6, 7.3, 0.016,
               5.5, 9.0, 0.020,
               24, 38, 0.082))

