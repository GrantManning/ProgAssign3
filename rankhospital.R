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
    if (outcome=="heart attack"){
        selectread<-select(readstate,c(7,2,11))
        names(selectread)<-c("State","Hospital.Name","condition")
        selectread<-filter(selectread,condition!="Not Available")
        selectread<-mutate(selectread,condition=type.convert(condition))
    }
    else if(outcome=="heart failure"){
        selectread<-select(readstate,c(7,2,17))
        names(selectread)<-c("State","Hospital.Name","condition")
        selectread<-filter(selectread,condition!="Not Available")
        selectread<-mutate(selectread,condition=type.convert(condition))
    }
    else if(outcome=="pneumonia"){
        selectread<-select(readstate,c(7,2,23))
        names(selectread)<-c("State","Hospital.Name","condition")
        selectread<-filter(selectread,condition!="Not Available")
        selectread<-mutate(selectread,condition=type.convert(condition))
    }
    else{
        stop(withCallingHandlers("invalid outcome"))
    }
    if(rank=="worst"){
        rank<-nrow(selectread)
    }
    else if(rank=="best"){
        rank<-1
    }
    else if(as.numeric(rank)>nrow(selectread)){
        return(NA)
    }
    else{
        rank<-as.numeric(rank)
    }
    orderedlist<-arrange(selectread,condition,Hospital.Name)
    ranked<-orderedlist[rank,2]
    return(ranked)
}
