#' Assert that a data.frame's columns are certain types
#'
#' Given a data.frame or data.table object, assert that all columns in the names of the coltypes 
#' argument match the types of the elements of the coltypes argument. 

#' @param data A data.frame or data.table
#' @param coltypes List with names corresponding to columns in \emph{data}. The types of the columns in \emph{data} 
#'   will be tested against types of the elements in coltypes.
#' @param quiet Do you want to suppress the printed message when a test is passed? Default = F.

#' @return Throws error if test is violated.
#' @export

#' @examples
#' # Should pass
#' assert_coltypes(CO2, list(Plant = integer(), conc = double()))
#' # Should fail
#' \dontrun{
#'   assert_coltypes(CO2, list(Plant = character(), conc = character()))
#' }

assert_coltypes <- function(data, coltypes, quiet=FALSE) {
  # Do all specified colnames exist in data frame?
  non_df_cols <- colnames(coltypes)[!(names(coltypes) %in% colnames(data))]
  if(length(non_df_cols) > 0) {
    stop(paste0("These columns exist in colnames but not in your dataframe: ",
                paste(non_df_cols, collapse=" ")))
  }
  
  # Do all column types match?
  typematch <- sapply(data[, names(coltypes)], typeof) == sapply(coltypes, typeof)
  if(any(!typematch)) {
    msg_list <- sprintf("%s should be '%s' but is '%s'", 
                        names(coltypes),
                        sapply(coltypes, typeof),
                        sapply(data[, names(coltypes)], typeof))[!typematch]
    
    stop(paste0("These columns in your data frame have different types than coltypes: \n",
                paste(msg_list, collapse = "\n")))
  }
  
  if(!quiet) {
    print("All column types match")
  }
}
