#' Assert that a data.frame's columns are non-NA/infinite, or are greater, less than, equal/not-equal, or contain specified values.
#'
#' Given a data.frame or data.table object, make assertions about values of the columns within the object.
#' Assert that a column contains no missing/infinite values, or that it is greater/less than, equal to, or contains 
#' either a single value, vector with nrow(data) values, or a vector of any length(for \emph{in} option).


#' @param data A data.frame or data.table
#' @param colnames Character vector with column names corresponding to columns in \emph{data}
#' @param test The type of evaluation you want to assert in your data
#' \itemize{
#'   \item \emph{not_na}:      All values must not be Na
#'   \item \emph{not_nan}:     All values must not be NaN
#'   \item \emph{not_inf}:     All values must not be infinite
#'   \item \emph{lt}:          All values must be less than test_val
#'   \item \emph{lte}:         All values must be less than or equal to test_val
#'   \item \emph{gt}:          All values must be greater than test_val
#'   \item \emph{gte}:         All values must be greater than or equal to test_val
#'   \item \emph{equal}:       All values must be equal to test_val
#'   \item \emph{not_equal}:   All values must not equal test_val
#'   \item \emph{in}:          All values must be one of the values in test_val
#' }
#' @param test_val A single value, a vector with length = nrow(data), or a vector of any length (if using the \emph{in} option for test. 
#'                  Must match the character type of colnames.
#' @param display_rows Do you want to show the actual rows that violate the assertion? Default=T
#' @param na.rm Do you want to remove NA and NaN values from assertions? Default=F
#' @param warn_only Do you want to warn, rather than error? Will return all offending rows from the first violation of the assertion Default=F
#' @param quiet Do you want to suppress the printed messages when a test is passed? Default = F.

#' @return Throws error if test is violated. If warn_only=T, will return all offending rows from the first violation of the assertion.
#' @export

#' @examples
#' assert_values(CO2, colnames="uptake", test="gt", 0) # Are all values greater than 0?
#' assert_values(CO2, colnames="conc", test="lte", 1000) # Are all values less than/equal to 1000?
#' \dontrun{
#'  assert_values(CO2, colnames="uptake", test="lt", 40) # Are all values less than 40?
#'  # Fails: not all values < 40.
#' }
#' assert_values(CO2, colnames="Treatment", test="in", test_val = c("nonchilled","chilled"))
#' CO2_mult <- CO2
#' CO2_mult$new_uptake <- CO2_mult$uptake * 2
#' assert_values(CO2, colnames="uptake", test="equal", CO2_mult$new_uptake/2)
#' \dontrun{
#'  assert_values(CO2, colnames="uptake", test="gt", CO2_mult$new_uptake/2, display_rows=F)
#'  # Fails: uptake !> new_uptake/2
#' }


assert_values <- function(data, colnames, test="not_na", test_val=NA, display_rows = TRUE, na.rm=FALSE, warn_only=FALSE, quiet=FALSE) {
  if(!(test %in% c("not_na","not_nan","not_inf")) & is.na(test_val[1])) 
    stop("Must specify test_val argument for comparison tests")
  
  if(!test %in% c("in") & length(test_val) != 1 & length(test_val) != nrow(data)) 
    stop("test_val must be either a single value or the same length as nrow(data), unless using the option test='in'")

  if(test == "not_na") {
    error_message <- "are NA"
  } else if(test == "not_nan") {
    error_message <- "are NaN"
  } else if(test == "not_inf") {
    error_message <- "are infinite"
  } else if(test == "lt") {
    symbol <-         "<"
    error_message <- paste0("not less than the test value(s)")
  } else if(test == "lte") {
    symbol <-         "<="
    error_message <- paste0("not less than or equal to the test value(s)")
  } else if(test == "gt") {
    symbol <-         ">"
    error_message <- paste0("not more than the test value(s)")
  } else if(test == "gte") {
    symbol <-         ">="
    error_message <- paste0("not more than or equal to the test value(s)")
  } else if(test == "equal") {
    symbol <-         "=="
    error_message <- paste0("not equal to the test value(s)")
  } else if(test == "not_equal") {
    symbol <-         "!="
    error_message <- paste0("are equal to the test value(s)")
  } else if(test == "in") {
    symbol <-         "%in%"
    error_message <- paste0("do not contain the test value(s)")
  }
  
  for(col in colnames) {
    if(test == "not_na") {
      rows <- eval(parse(text=paste0("is.na(data$",col,")")))
    } else if (test == "not_nan") {  
      rows <- eval(parse(text=paste0("is.nan(data$",col,")")))
    } else if (test == "not_inf") {  
      rows <- eval(parse(text=paste0("is.infinite(data$",col,")"))) # Automatically evaluates NAs to false
    } else if(na.rm==F) {
      rows <- eval(parse(text=paste0("!data$",col,symbol,"test_val")))
    } else if(na.rm==T) {
      rows <- eval(parse(text=paste0("!(data$",col,symbol,"test_val | is.na(data$",col,") | is.nan(data$",col,"))")))
    }
    rows[is.na(rows) | is.nan(rows)] <- TRUE # Treat NAs as failures of the test
    nrows <- length(rows[rows==TRUE]) 
    if(nrows > 0) {
      if(display_rows == T & warn_only == F) {
        print(data[rows,])
        stop(paste(nrows,"Rows for variable",col,error_message,"in the dataset above"))
      } else if(warn_only == TRUE) {
        warning(paste(nrows,"Rows for variable",col,error_message,"in the dataset"))
        return(data[rows,])
      } else {
        stop(paste(nrows,"Rows for variable",col,error_message,":",paste(which(rows==TRUE),collapse = " ")))
      }
    }
    if(quiet != TRUE) print(paste("Variable",col,"passed",test,"test"))
  }
}
