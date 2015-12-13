rankall <- function(outcome, num = "best") {

    mydata <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
    output = ""
    allstate <- sort(unique(mydata$State))
    hospital_names = c()
    len = length(allstate)
    ct = 1
    while(ct <= len){
        hospital_names[ct] <- rankhospital(allstate[ct], outcome, num)
        ct = ct + 1
    }
    result <- data.frame(hospital=hospital_names, state=allstate)
    result
}
rankhospital <- function(state, outcome, num='best') {
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
    mydata[,orig_reasons[index]]
    mydata[,orig_reasons[index]] <- as.numeric(
        mydata[,orig_reasons[index]])
    x<-mydata[order(
        mydata[,orig_reasons[index]], mydata[,"Hospital.Name"]), ]
    y<-(x[,c(orig_reasons[index],"Hospital.Name", "State")])
    if ((num != 'best') && (num!='worst')){
        numeric_num <-as.numeric(num)
    }
    z<-(y[y$State %in% state,])
    len = length(z$State)
    if (num == 'best'){
        z<-z[1,]
    }
    else if (num == 'worst'){
        z<-z[len,]
    }
    else if ((numeric_num > 0) && (numeric_num < len)){
        z<-z[num,]
    }
    else{
        return(NA)
    }
    return(z$Hospital.Name)
}

