rankhospital<-function(state,outcome,rank){
    state<-state
    outcome<-outcome
    read<-read.csv("outcome-of-care-measures.csv")
    readstate<-filter(read,read$State==state)
    if(nrow(readstate)==0){
        stop(withCallingHandlers("invalid state"))
    }
    if(!is.numeric(rank)&!(rank%in%c("best","worst"))){
        stop(withCallingHandlers("invalid rank"))
    }
    if(rank=="worst"){
        rank<-nrow(readstate)
    }
    else if(rank=="best"){
        rank<-1
    }
    else{
        rank<-as.numeric(rank)
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
    orderedlist<-readstate[order(as.numeric(condition),readstate$Hospital.Name),]
    ranked<-orderedlist[rank,2]
}