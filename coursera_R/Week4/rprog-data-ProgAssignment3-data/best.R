best <- function(state, outcome) {
    ## Read outcome data
    mydata <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
    output = ""
    ## Check that state and outcome are valid
    if (!any(mydata$State %in% state)){
        stop("invalid state")
    }
    orig_reasons = c(
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")

    lc_reasons = c(tolower(
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"),
    tolower("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"),
    tolower("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))

    if (!any(
    lc_reasons %in%
    paste("hospital.30.day.death..mortality..rates.from",
          gsub(" ","\\.", outcome),sep="."))){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    index = match(paste("hospital.30.day.death..mortality..rates.from",
            gsub(" ","\\.", outcome),sep="."), lc_reasons)
    row_to_keep = which(mydata[,orig_reasons[index]] != 'Not Available')
    mydata<-mydata[row_to_keep,]
    mydata[,orig_reasons[index]] <- as.numeric(
        mydata[,orig_reasons[index]])
    x<-mydata[order(
    mydata[,orig_reasons[index]], mydata[,"Hospital.Name"]), ]
    y<-(x[,c(orig_reasons[index],"Hospital.Name", "State")])
    z<-(y[y$State %in% state,])[1,]
    z$Hospital.Name

}
