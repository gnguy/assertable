#' Given a vector of filenames, append all files and return as one data.table using a user-defined function
#'
#' Given a character vector of filenames, check how many of them currently exist. 
#' Optionally, can keep checking for a specified amount of time, at a given frequency
#' 

#' @param filenames A character vector of filenames (specify full paths if you are checking files that are not in present working directory)
#' @param folder An optional character containing the folder name that contains the files you want to check (if used, do not include folderpath in the filenames characters). If not specified, will look in present working directory. 
#' @param FUN function: The function that you want to use to import your data, e.g. read.csv, fread, read_dta, etc.
#' @param warn_only Boolean (T/F), whether to send a warning message as opposed to an error message if files are missing prior to import. Will only import the files that do exist.
#' @param multicore boolean, use lapply or mclapply (multicore = T) to loop over files in \emph{filenames} for import. Default=F.
#' @param use.names boolean, pass to the use.names option for \emph{rbindlist}
#' @param fill boolean, pass to the fill option for \emph{rbindlist}
#' @param mc.preschedule boolean, pass to the mc.preschedule option for \emph{mclapply} if multicore = T. Default = F.
#' @param mc.cores, pass to the mc.preschedule option for \emph{mclapply} if multicore = T. Default = mclapply default.
#' @param ... named arguments of \emph{FUN} to pass to \emph{FUN}

#' @return One data.table that contains all files in \emph{filenames}, combined together using rbindlist. 
#'         Returns an error if any file in \emph{filenames} does not exist
#' @export

#' @examples
#' \dontrun{
#'  for(i in 1:3) {
#'    data <- CO2
#'    data$id_var <- i
#'    write.csv(data,file=paste0("file_",i,".csv"),row.names=FALSE)
#'  }
#'  filenames <- paste0("file_",c(1:3),".csv")
#'  import_files(filenames, FUN=fread)
#'  import_files(filenames, FUN=read.csv, stringsAsFactors=FALSE)
#'  import_files(filenames, FUN=fread, multicore=T, mc.cores=1) # Only if you have a multi-core system
#' }

#' @import data.table

import_files <- function(filenames, folder="", FUN=fread, warn_only=FALSE, multicore=FALSE, use.names=TRUE, fill=TRUE, 
                        mc.preschedule=FALSE, mc.cores = getOption("mc.cores", 2L), ...) {

  file_list <- tryCatch({
    test <- check_files(filenames, folder, warn_only=T)
  }, warning = function(w) {
    return(test)
  })

  if(length(file_list) > 0) {
    message <- paste0("These files do not exist: ",paste(file_list, collapse=" "))

    if(warn_only == FALSE) {
      stop(message)
    } else {
      warning(paste0("Some files don't exist, importing those that do exist. ",message))
      filenames <- filenames[!filenames %in% file_list]
      if(length(filenames)==0) {
        stop("No files exist to import")
      }
    }
  }

  if(folder != "") filenames <- file.path(folder,filenames)

  if(multicore == FALSE) {
    results <- rbindlist(lapply(filenames, FUN,...), use.names=use.names, fill=fill)
  } else {
    results <- rbindlist(parallel::mclapply(filenames, FUN,..., mc.preschedule=mc.preschedule, mc.cores=mc.cores), 
                        use.names=use.names, fill=fill)
  }

  return(results)
}
