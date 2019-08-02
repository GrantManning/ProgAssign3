best<-function(state,outcome){
    state<-state
    outcome<-outcome
    read<-read.csv("outcome-of-care-measures.csv")
    readstate<-filter(read,read$State==state)
    if(nrow(readstate)==0){
        stop(withCallingHandlers("invalid state"))
    }
    if (outcome=="heart attack"){
        condition<-readstate[,11]
    }
    else if(outcome=="heart failure"){
        condition<-readstate[,17]
    }
    else if(outcome=="pneumonia"){
        condition<-readstate[,23]
    }
    else{
        stop(withCallingHandlers("invalid outcome"))
    }
    best<-filter(readstate,as.numeric(condition)==min(as.numeric(condition)))
    if(nrow(best)>1){
        best<-arrange(best,Hospital.Name)[1,]
    }
    return(best[,2])
}