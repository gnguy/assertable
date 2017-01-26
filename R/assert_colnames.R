#' Assert that a data.frame contains specified column names
#'
#' Given a data.frame or data.table object, assert that all columns in the colnames argument exist as columns.

#' @param data A data.frame or data.table
#' @param colnames Character vector with column names corresponding to columns in \emph{data}
#' @param only_colnames Assert that the only columns in the data object should be those in \emph{colnames}. Default = T.

#' @return Throws error if test is violated.
#' @export

#' @examples
#' assert_colnames(CO2, c("Plant","Type","Treatment","conc","uptake"))
#' assert_colnames(CO2, c("Plant","Type"), only_colnames=FALSE)

assert_colnames <- function(data, colnames, only_colnames=TRUE) {
  # Do all specified colnames exist in dataframe?
    non_df_cols <- colnames[!colnames %in% colnames(data)]
  
  # Do all columns of the data exist in the specified colnames?
    non_colname_cols <- colnames(data)[!colnames(data) %in% colnames]
  
  if(length(non_df_cols) > 0 & length(non_colname_cols) > 0) {
    stop(paste0("These columns exist in colnames but not in your dataframe: ",
                paste(non_df_cols, collapse=" "),
                " and these exist in your dataframe but not in colnames: ",
                paste(non_colname_cols, collapse=" ")))
  } else if(length(non_df_cols) > 0) {
    stop(paste0("These columns exist in colnames but not in your dataframe: ",
                paste(non_df_cols, collapse=" ")))
  } else if(length(non_colname_cols) > 0 & only_colnames == TRUE) {
    stop(paste0("These columns exist in your dataframe but not in colnames: ",
                paste(non_colname_cols, collapse=" ")))
  }
  print("All column names present")
}
