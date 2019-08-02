best<-function(state,outcome){
    state<-state
    outcome<-outcome
    read<-read.csv("outcome-of-care-measures.csv")
    readstate<-filter(read,read$State==state)
    #if(nrow(readstate)==0){
    #   print(paste("Error in best(",state,outcome,") : invalid state",split=" "))
    #    stop()
    #}
    if (outcome=="heart attack"){
        condition<-readstate["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
    }
    else if(outcome=="heart failure"){
        condition<-readstate["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
    }
    else if(outcome=="pneumonia"){
        condition<-readstate["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
    }
    #else{
    #    print(paste("Error in best(",state,outcome,") : invalid outcome",split=" "))
    #    stop()
    #}
    best<-filter(readstate,condition==min(condition))
    return(best[,2])
}