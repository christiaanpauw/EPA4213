#' @title normalisedVehicleCount
#' @description Take vehicle count per timeunit and length and width of road and return vehicle count per m2 per second
#' @param n Integer. Count of vehicles per unit t
#' @param l Numeric or units. Length in meters OR units with distance unit
#' @param w Numeric or units. Width in meters OR units with distance unit
#' @param t Numeric or units. 
#' @return Vehicles per m2 per second [1/m^2/s]
#' @import units
#' @export

normalisedVehicleCount <- function(n, l, w, t){
  
  # Check is arguments have units, else give them some
  if (class(l) != "units") l <- set_units(l, m) 
  if (class(w) != "units") w <- set_units(w, m) 
  if (class(t) != "units") t <- set_units(t, h) 
  
  units(t) <- make_units(s)
  units(w) <- make_units(m)
  units(l) <- make_units(m)
  
  area = set_units(l * w, m^2)
  
  (n / area / t)
}

# pavedRoadDustEmissions(sL = 200, W = 10) *  set_units(400, m) * 10 / (set_units(400, m) * set_units(3, m)) / set_units(60*60, s) 
