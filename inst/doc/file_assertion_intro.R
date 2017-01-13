## ---- echo=FALSE, results='asis'-----------------------------------------
library(assertable)

## ---- results='asis', eval=FALSE-----------------------------------------
#  for(i in 1:3) {
#    data <- CO2
#    data$id_var <- i
#    write.csv(data,file=paste0("../data/file_",i,".csv"),row.names=F)
#  }

## ---- results='markup'---------------------------------------------------
filenames <- paste0("../data/file_",c(1:3),".csv")
filenames

## ---- results='markup', error=TRUE---------------------------------------
check_files(filenames)

## ---- results='markup', error=TRUE---------------------------------------
filenames <- paste0("../data/file_",c(1:4),".csv")
check_files(filenames)

## ---- results='markup', error=TRUE---------------------------------------
filenames <- paste0("../data/file_",c(1:4),".csv")
check_files(filenames, continual=T, sleep_time = 1, sleep_end = .10)

## ---- results='markup', error=TRUE---------------------------------------
filenames <- paste0("../data/file_",c(1:5),".csv")
check_files(filenames, display_pct=50)

## ---- results='markup'---------------------------------------------------
library(data.table)
filenames <- paste0("../data/file_",c(1:3),".csv")
data <- import_files(filenames, FUN=fread)
data

## ---- results='markup'---------------------------------------------------
data <- import_files(filenames, FUN=read.csv, stringsAsFactors=F)
data

## ---- results='markup', error=TRUE---------------------------------------
filenames <- paste0("../data/file_",c(1:20),".csv")
import_files(filenames)

