complete <- function(directory, id = 1:332) {
    file_index = 1
    myfiles = list()
    for (item in id){
        if(item<10){
            myfiles[[file_index]] <- paste(directory,'/00',
                                           item,'.csv', sep="")
        }
        if(item>=10 && item<100){
            myfiles[[file_index]] <- paste(directory,'/0',
                                           item,'.csv', sep="")
        }
        if(item>=100){
            myfiles[[file_index]] <- paste(directory,'/',
                                           item,'.csv', sep="")
        }
        file_index = file_index + 1
    }
    mydata = lapply(myfiles,read.csv)
    nobs=c()
    data_range = 1:length(mydata)
    for (item in data_range){
        nobs[item] = nrow(na.omit(mydata[[item]]))
    }
    df<-data.frame(id=id, nobs=nobs)
    df
}
