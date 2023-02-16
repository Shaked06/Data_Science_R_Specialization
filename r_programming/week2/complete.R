
complete <- function(directory, id=1:322){
  csv_files <- list.files(paste(getwd(),"/week2/",directory, sep=""))
  curr_csv_files <- csv_files[id]
  mat <- matrix(, nrow=length(id), ncol=2)
  for(i in seq_along(curr_csv_files)){
    tmp_file <- read.csv(paste(getwd(),"/week2/specdata/",curr_csv_files[i], sep=""))
    mat[i, 1] <- id[i]
    mat[i, 2] <- nrow(na.omit(tmp_file))
  }
  colnames(mat) <- c("id", "nobs")
  
  return(data.frame(mat))
}

complete("specdata", id=c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


