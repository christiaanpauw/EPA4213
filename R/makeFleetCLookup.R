#' @title makeFleetCLookup
#' @description Create a lookup table for the values of C in EPA 42 Ch 13 Section 13.2.2 Table 13.2.2-4
#' @references https://www3.epa.gov/ttnchie1/ap42/ch13/final/c13s0202.pdf
#' @import dplyr
#' @import magrittr
#' @export


makeFleetCLookup <- function(){
 df <- expand.grid(massUnit = c("lb"),
                          distanceUnit = c("ml"),
                          size = c("PM2.5", "PM10", "PM30"))
 df$C = c(0.00036, 0.00047, 0.00047)
 df
}
