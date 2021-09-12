#' @title makeVehicleDistanceLookup
#' @description Create a lookup table for the values of K in EPA 42 Ch 13 Section 13.2.1.3
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @export
#' @import dplyr
#' @import magrittr


makeVehicleDistanceLookup <- function(){
 expand.grid(massUnit = c("g","lb"),
                          distanceUnit = c("km", "ml"),
                          size = c("PM2.5", "PM10", "PM15", "PM30")) %>%
    filter(!(distanceUnit == "km" & massUnit == "lb")) %>%
    mutate(k = c(1.1, 1.8, 0.0040,
                 4.6, 7.3, 0.016,
                 5.5, 9.0, 0.020,
                 24, 38, 0.082))
}
