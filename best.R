best<-function(state,outcome){
    state<-state
    outcome<-outcome
    read<-read.csv("outcome-of-care-measures.csv")
    readstate<-filter(read,State==state)
    if(nrow(readstate)==0){
        stop(withCallingHandlers("invalid state"))
    }
    if (outcome=="heart attack"){
        selectread<-select(readstate,c(7,2,11))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else if(outcome=="heart failure"){
        selectread<-select(readstate,c(7,2,17))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else if(outcome=="pneumonia"){
        selectread<-select(readstate,c(7,2,23))
        names(selectread)<-c("State","Hospital.Name","condition")
    }
    else{
        stop(withCallingHandlers("invalid outcome"))
    }
    statebest<-arrange(selectread,condition,Hospital.Name)[1,]
    return(statebest[,2])
}