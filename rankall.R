rankall<-function(outcome,rank){
    outcome<-outcome
    read<-read.csv("outcome-of-care-measures.csv")
    if(!is.numeric(rank)&!(rank%in%c("best","worst"))){
        stop(withCallingHandlers("invalid rank"))
    }
    states<-unique(read$State)
    table<-data.frame("hospital"=1:length(states),"state"=1:length(states))
    if(rank=="best"){
        rank<-1
    }
    else if(is.numeric(rank)){
        rank<-as.numeric(rank)
    }
    if (outcome=="heart attack"){
        selectread<-select(read,c(7,2,11))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else if(outcome=="heart failure"){
        selectread<-select(read,c(7,2,17))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else if(outcome=="pneumonia"){
        selectread<-select(read,c(7,2,23))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else{
        stop(withCallingHandlers("invalid outcome"))
    }
    for (n in 1:length(states)){
        statefilter<-filter(selectread,State==as.character(states[n]),!is.na(condition))
        if (rank=='worst'){
            rank<-nrow(statefilter)
        }
        if (rank>nrow(statefilter)){
            table[n,1]<-as.character(NA)
            table[n,2]<-as.character(states[n])
            next
        }
        staterank<-arrange(statefilter,condition,Hospital.Name)
        hospital<-staterank[rank,2]
        table[n,1]<-as.character(hospital)
        table[n,2]<-as.character(states[n])
    }
    return(table)
}
