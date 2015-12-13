corr <- function(directory, threshold = 0) {
    myfiles = list()
    for (filename in list.files(directory)){
           myfiles <- append(myfiles,
                                 paste(directory,filename,sep="/"))
    }
    correlation_vec <- c()
    for (filename in myfiles){
        df<-read.csv(filename)
        length_cc <- nrow(na.omit(df))
        if (length_cc > threshold){
            correlation_vec <- append(correlation_vec,
                                cor(as.numeric((na.omit(df))$sulfate),
                                as.numeric((na.omit(df))$nitrate)))

        }
    }
    return(correlation_vec)
}
