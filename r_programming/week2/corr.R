
source("week2/complete.R")

corr <- function(directory, thershold = 0){
    res <- c()
    df <- complete(directory)
    df <- subset(df, nobs >= thershold)
    ind_files <- as.integer(rownames(df))
    csv_files <- list.files(paste(getwd(),"/week2/",directory, sep=""))
    for(file in csv_files[ind_files]){
      tmp_file <- read.csv(paste(getwd(),"/week2/specdata/",file, sep=""))
      tmp_file <- na.omit(tmp_file)
      res <- c(res, cor(tmp_file$sulfate, tmp_file$nitrate))
    }
    return(res)
  
}
