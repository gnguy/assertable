#' Check for the existence of a vector of files, optionally repeated for a set amount of time.
#'
#' Given a character vector of filenames, check how many of them currently exist. 
#' Optionally, can keep checking for a specified amount of time, at a given frequency
#' 

#' @param filenames A character vector of filenames (specify full paths if you are checking files that are not in present working directory)
#' @param folder An optional character containing the folder name that contains the files you want to check (if used, do not include folderpath in the filenames characters). If not specified, will search in present working directory. 
#' @param warn_only Boolean (T/F), whether to end with a warning message as opposed to an error message if files are still missing at the end of the checks.
#' @param continual Boolean (T/F), whether to only run once or to continually keep checking for files for \emph{sleep_end} minutes. Default = F.
#' @param sleep_time numeric (seconds); if \emph{continual} = T, specify the number of seconds to wait in-between file checks. Default = 30 seconds.
#' @param sleep_end numeric (minutes); if \emph{continual} = T, specify number of minutes to check at \emph{sleep_time} intervals before terminating. Default = 180 minutes.
#' @param display_pct numeric (0-100); at what percentage of files found do you want to print the full list of still-missing files? Default = 75 percent of files.

#' @return Prints the number of files that match. If \emph{warn_only} = T, returns a character vector of missing files
#' @export

#' @examples
#' \dontrun{
#'  for(i in 1:3) {
#'    data <- CO2
#'    data$id_var <- i
#'    write.csv(data,file=paste0("file_",i,".csv"),row.names=FALSE)
#'  }
#'  filenames <- paste0("file_",c(1:3),".csv")
#'  check_files(filenames)
#' }


check_files <- function(filenames,folder="",warn_only=FALSE,continual=FALSE,sleep_time=30,sleep_end=(60*3),display_pct=75) {
  counter <- 0
  time_counter <- 0

  if(folder != "") {
    pwd <- getwd()
    setwd(folder)
  }

  while(counter == 0) {
    inner_counter <- 0
    missing_list <- c()
    for(file in filenames) {
      if(file.exists(file)) {
        inner_counter <- inner_counter + 1
      } else {
        missing_list <- c(missing_list, file)
      }
    }
    
    ## Success condition
    if(length(filenames) == inner_counter) {
      print("All results are present")
      if(folder != "") setwd(pwd)
      counter <- 1
    } else {

      ## Print the number of files left, and the actual files left if display_pct is met
      print(paste0("Have ",inner_counter," files: expecting ",length(filenames)," at ",Sys.time()))
      if(inner_counter >= (length(filenames) * (display_pct/100))) {
        print(paste0("Still Missing: ",paste(missing_list,collapse=" "))) 
      }

      ## Stop if continual = T, otherwise sleep for sleep_time and then move back to the top of the while{} loop
      if(continual == FALSE) {
        counter <- 1
        if(folder != "") setwd(pwd)
        message <- "Files not complete; stopping execution -- set continual=T for continual file checks"
        if(warn_only == FALSE) {
          stop(message)
        } else {
          warning(message)
          return(missing_list)
        }
      } else {
        time_counter <- time_counter + 1
        Sys.sleep(sleep_time)
        if(time_counter * (sleep_time/60) > sleep_end) {
          if(folder != "") setwd(pwd)
          message <- paste0("Files not complete; stopping execution after ",sleep_end," minutes")
          if(warn_only == FALSE) {
            stop(message)
          } else {
            warning(message)
            return(missing_list)
          }
        } # if(time_counter * (sleep_time/60) > sleep_end)
      } # else{} for continual == FALSE
    } # else{} for if(length(filenames) == inner_counter)
  } # while(counter==0)
}
