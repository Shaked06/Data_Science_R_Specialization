
pollutantmean <- function(directory, pollutant, id=1:322){
  csv_files <- list.files(directory)  
  vect <- c()
  vect.no.na <- c()
  for(file in csv_files[id]){
    tmp_file <- read.csv(paste(getwd(),"/specdata/",file, sep=""))
    vect <- tmp_file[, pollutant]
    vect.no.na <- c(vect.no.na, vect[!is.na(vect)])
  }  
  return (sum(vect.no.na)/length(vect.no.na))
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

