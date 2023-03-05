
# QUESTION 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11], col="blue", 
     main="Histogram of the 30-day death rates from heart attack",
     )

# QUESTION 2
best <- function(state, outcome){
  df <- read.csv("outcome-of-care-measures.csv", 
                 colClasses = "character")
  cols <- c("Hospital.Name",
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
            "Hospital.30.Day.Readmission.Rates.from.Heart.Failure",
            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  df_state <- subset(df, State == state)[, cols]
  colnames(df_state) <- c("Hospital.Name", "heart attack", "heart failure", "pneumonia")
  df_state <- df_state[(df_state$`heart attack` != "Not Available") & 
                         (df_state$`heart failure` != "Not Available") & (df_state$pneumonia != "Not Available"),]
  df_state <- na.omit(df_state)
  df_state[,outcome] <- as.numeric(df_state[, outcome])
  df_min_state <- subset(df_state, outcome = min(df_state[, outcome]))
  df_min_state <- df_min_state[order(df_min_state[,outcome] ,decreasing=FALSE),]
  return(df_min_state$Hospital.Name[1])
}
  
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

# QUESTION 3
rankhospital <- function(state, outcome, num){
  
  df <- read.csv("outcome-of-care-measures.csv", 
                 colClasses = "character")
  
  cols <- c("Hospital.Name",
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
            "Hospital.30.Day.Readmission.Rates.from.Heart.Failure",
            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  df_state <- subset(df, State == state)[, cols]
  
  if(nrow(df_state) == 0){return(NaN)}
  try(if(any(df_state$Hospital.Name == state)) stop("invalid state"))
  try(if(any(df_state$outcome == outcome)) stop("invalid outcome"))
  
  colnames(df_state) <- c("Hospital.Name", "heart attack", "heart failure", "pneumonia")
  
  df_state <- df_state[(df_state[, outcome] != "Not Available"), ]
  df_state[,outcome] <- as.numeric(df_state[, outcome])
  df_state_1 <- df_state[(!is.na(df_state[, outcome])), ]
  df_state_2 <- df_state[order(df_state_1[ ,outcome],df_state_1[,"Hospital.Name"]), ]
  
  row = NaN
  if(is.numeric(num)){
    if(nrow(df_state_2) >= num){
      row = num
    }
    else{
      row = num
    }
  }
  if(num == "best") {
    row =  1 
  }
  if(num == "worst"){
    row = nrow(df_state_2)
  }
  return(df_state_2[row, 1])
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

# QUESTION 4
rankall <- function(outcome, num){
  df <- read.csv("outcome-of-care-measures.csv", 
                 colClasses = "character")
  names <- c()
  states <- unique(df[, "State"])
  for(state in states){
    names <- c(names, rankhospital(state, outcome, num))
  }
  res <- data.frame(states, names)
  colnames(res) <- c("state", "hospital")
  rownames(res) <- states
  return(res)
}
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)


r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


