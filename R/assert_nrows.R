#' Assert that a data.frame contains a specified number of rows
#'
#' Given a data.frame or data.table object and a target number of rows, check that a dataset has that many rows

#' @param data A data.frame or data.table
#' @param target_nrows Numeric -- number of expected rows

#' @return Throws error if test is violated
#' @export
#' @examples
#' assert_nrows(CO2,84)

assert_nrows <- function(data,target_nrows) {
  if(nrow(data) != target_nrows) stop(paste0("Have ", nrow(data), " rows, expecting ",target_nrows))
  print("All rows present")
}