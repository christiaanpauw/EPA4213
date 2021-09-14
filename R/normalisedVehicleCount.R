#' @title normalisedVehicleCount
#' @description Take vehicle count per timeunit and length and width of road and return vehicle count per m2 per second
#' @param n Integer. Count of vehicles per unit t
#' @param l Numeric or units. Length in meters OR units with distance unit
#' @param w Numeric or units. Width in meters OR units with distance unit
#' @param t Numeric or units. Assumed per hour
#' @param verbose Logical. Messages or not
#' @return Vehicles per m2 per second [1/m^2/s]
#' @import units
#' @export

normalisedVehicleCount <- function(n, l, w, t, verbose = FALSE){

  # Check is arguments have units, else give them some
  if (class(l) != "units") l <- units::set_units(l, m)
  if (class(w) != "units") w <- units::set_units(w, m)
  if (class(t) != "units") t <- units::set_units(t, h)

  units(t) <- units::make_units(s)
  units(w) <- units::make_units(m)
  units(l) <- units::make_units(km)

  area = units::set_units(l * w, m^2)
  if (verbose) message("area is ", area, units::deparse_unit(area))
  if (verbose) message("n ", n)
  if (verbose) message("l ", l, units::deparse_unit(l))
  if (verbose) message("t ", t, units::deparse_unit(t))

  ( (n / area ) / t)
}

# pavedRoadDustEmissions(sL = 200, W = 10) *  set_units(400, m) * 10 / (set_units(400, m) * set_units(3, m)) / set_units(60*60, s)

normalisedVKT <- function(n, l, w, t, verbose = FALSE){

  # Check is arguments have units, else give them some
  if (class(l) != "units") l <- units::set_units(l, m)
  if (class(t) != "units") t <- units::set_units(t, h)
  if (class(w) != "units") w <- units::set_units(w, m)

  units(t) <- units::make_units(s)
  units(w) <- units::make_units(m)
  units(l) <- units::make_units(km)

  if (verbose) message("n ", n)
  if (verbose) message("l ", l, units::deparse_unit(l))
  if (verbose) message("t ", t, units::deparse_unit(t))

   ((n * l ) / t )
}


