#' @title makeVKT
#' @description Calculate the vehicle kilometer travelled
#' @param n Integer. Count per hour.
#' @param d Numeric. Distance in kilometer
#' @export

makeVKT <- function(n = NULL, d = NULL){
  if (is.null(n) | is.null(d)) stop("n and d cannot be NULL")
  res <- n * d
  units(res) <- as_units("km")
  res
}
