#' @title makeVehicleDistanceLookup
#' @description Create a lookup table for the values of K in EPA 42 Ch 13 Section 13.2.1.3 and
#' Contants in EPA 42 Ch 13 Section 13.2.2 Table 13.2.2-2
#' @references https://www3.epa.gov/ttn/chief/old/ap42/ch13/s021/final/c13s02-1_2002.pdf
#' @param type Character. What type of road is it c("paved", "unpaved-industrial", "unpaved-public")
#' @export
#' @import dplyr
#' @import magrittr


makeVehicleDistanceLookup <- function(type = c("paved", "unpaved-industrial", "unpaved-public")[1]){
 if (type == "paved")
   return(
     expand.grid(massUnit = c("g","lb"),
                          distanceUnit = c("km", "ml"),
                          size = c("PM2.5", "PM10", "PM15", "PM30")) %>%
    filter(!(distanceUnit == "km" & massUnit == "lb")) %>%
    mutate(k = c(1.1, 1.8, 0.0040,
                 4.6, 7.3, 0.016,
                 5.5, 9.0, 0.020,
                 24, 38, 0.082))
    )
  if (type == "unpaved-industrial")
    return(
      expand.grid(massUnit = c("g","lb"),
                  distanceUnit = c("km", "ml"),
                  size = c("PM2.5", "PM10", "PM30")) %>%
        filter(!(distanceUnit == "km" & massUnit == "lb")) %>%
        filter(!(distanceUnit == "ml" & massUnit == "g")) %>%
        arrange(massUnit) %>%
        mutate(k = c(0.15 * 281.9, 0.15 * 281.9, 4.9 * 281.9,
                     0.15, 0.15, 4.9),
               a = c(0.9, 0.9, 0.7,
                     0.9, 0.9, 0.7),
               b = c(0.45, 0.45, 0.45,
                     0.45, 0.45, 0.45))
    )

  if (type == "unpaved-public")
    return(
      expand.grid(massUnit = c("g","lb"),
                  distanceUnit = c("km", "ml"),
                  size = c("PM2.5", "PM10", "PM30")) %>%
        filter(!(distanceUnit == "km" & massUnit == "lb")) %>%
        filter(!(distanceUnit == "ml" & massUnit == "g")) %>%
        arrange(massUnit) %>%
        mutate(k = c(0.18 * 281.9, 1.8 * 281.9, 6 * 281.9,
                     0.18, 1.8, 6),
               a = c(1, 1, 1,
                     1, 1, 1),
               c = c(0.2, 0.2, 0.3,
                     0.2, 0.2, 0.3),
               d = c(0.5, 0.5, 0.3,
                     0.5, 0.5, 0.3)
               )
      )
}
