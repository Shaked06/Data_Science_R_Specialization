
pollutantmean <- function(directory, pollutant, id=1:332){
  csv_files <- list.files(paste(getwd(),"/week2/",directory, sep=""))
  
  for(i in id){
    tmp_file <- read.csv(paste(getwd(),"/week2/specdata/",csv_files[i], sep=""))
    df <- rbind(df, tmp_file)    
  }
  return(mean(df[, pollutant], na.rm =TRUE))
}
