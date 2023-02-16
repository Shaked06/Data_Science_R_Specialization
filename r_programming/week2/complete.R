
complete <- function(directory, id=1:332){
  csv_files <- list.files(paste(getwd(),"/week2/",directory, sep=""))
  df <- data.frame()
  for(i in id){
    tmp_file <- read.csv(paste(getwd(),"/week2/specdata/",csv_files[i], sep=""))
    df <- rbind(df, tmp_file)    
  }
  df <- df[!is.na(df$sulfate & df$nitrate),]
  ids <- numeric(length(id))
  nobs <- numeric(length(id))
  pos <- 0
  for(i in id){
    pos <- pos+1
    ids[pos] <- i
    nobs[pos] <- sum(df$ID == i)
    
  }
  ids <- ids[1:pos]
  nobs <- nobs[1:pos]
  return(data.frame(ids, nobs))
}
