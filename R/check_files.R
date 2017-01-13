#' Check for the existence of a vector of files, optionally repeated for a set amount of time.
#'
#' Given a character vector of filenames, check how many of them currently exist. 
#' Optionally, can keep checking for a specified amount of time, at a given frequency
#' 

#' @param filenames A character vector of filenames (specify full paths if you are checking files that are not in present working directory)
#' @param continual Boolean (T/F), whether to only run once or to continually keep checking for files for \emph{sleep_end} minutes. Default = F.
#' @param sleep_time numeric (seconds); if \emph{continual} = T, specify the number of seconds to wait in-between file checks. Default = 30 seconds.
#' @param sleep_end numeric (minutes); if \emph{continual} = T, specify number of minutes to check at \emph{sleep_time} intervals before terminating. Default = 180 minutes.
#' @param display_pct numeric (0-100); at what percentage of files found do you want to print the full list of still-missing files? Default = 75 percent of files.

#' @return Prints the number of files that match
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


check_files <- function(filenames,continual=FALSE,sleep_time=30,sleep_end=(60*3),display_pct=75) {
  counter <- 0
  time_counter <- 0
  while(counter == 0) {
    inner_counter <- 0
    missing_list <- ""
    for(file in filenames) {
      if(file.exists(file)) {
        inner_counter <- inner_counter + 1
      } else {
        missing_list <- paste0(missing_list," ",file)
      }
    }
    
    if(length(filenames) == inner_counter) {
      print("All results are present")
      counter <- 1
    } else {
      print(paste0("Have ",inner_counter," files: expecting ",length(filenames)," at ",Sys.time()))
      if(inner_counter >= (length(filenames) * (display_pct/100))) {
        print(paste0("Still Missing: ",missing_list)) 
      }

      if(continual == FALSE) {
        counter <- 1
        stop("Files not complete; stopping execution -- set continual=T for continual file checks")
      } else {
        time_counter <- time_counter + 1
        Sys.sleep(sleep_time)
        if(time_counter * (sleep_time/60) > sleep_end) {
          stop(paste0("Files not complete; stopping execution after ",sleep_end," minutes")) 
        }
      }
    }
  }
}
