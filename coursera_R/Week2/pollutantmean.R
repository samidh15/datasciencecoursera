pollutantmean <- function(directory, pollutant, id=1:332){
    mydata = list()
    myfiles = list()
    file_index = 1
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
    write(as.character(myfiles),"filename",sep="\n")
    mydata = lapply(myfiles, read.csv)
    sum_total = list()
    sum_total = lapply(1:length(mydata),
                       function(i)
                       sum(mydata[[i]][[pollutant]],na.rm=TRUE))
    sum_obs = list()
    sum_obs = lapply(1:length(mydata),
             function(i) length(
            mydata[[i]][[pollutant]][!is.na(mydata[[i]][[pollutant]])]))
    mean = sum(as.numeric(sum_total))/sum(as.numeric(sum_obs))
    return(mean)
}

