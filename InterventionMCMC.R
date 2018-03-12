library(data.table)
library(markovchain)
library(readxl)
library(expm)
library(ggplot2)
library(stringr)
library(foreach)

## read in tran mat
Forced_Tran_Mat <- data.table(read_excel("JohnClassTest_MC_Int.xlsx", sheet = "ForcedInterventionMC"))
Forced_Tran_Mat[,1] <-NULL

NonForced_Tran_Mat <- data.table(read_excel("JohnClassTest_MC_Int.xlsx", sheet = "NonForcedIntMC"))
NonForced_Tran_Mat[,1] <- NULL

mc_forcedInt <- new("markovchain",states=colnames(Forced_Tran_Mat),transitionMatrix=as.matrix(Forced_Tran_Mat))
mc_nonForcedInt <- new("markovchain",states=colnames(NonForced_Tran_Mat), transitionMatrix=as.matrix(NonForced_Tran_Mat))


## bring in init states of kids ##
ActTestGrades <- read_excel("JohnClassTest_MC_Int.xlsx", sheet = "Class1")

#get rid of columns and rows you don't need from the actual grades
ActTestGrades <- ActTestGrades[1:51,1:9]
ActTestGrades <- data.table(ActTestGrades)
ActTestGrades <- ActTestGrades[,ActSuccess:=ifelse(FinalGrade>=70.0,1,0)]

initClass1<-data.table(ActTestGrades$`Test 1`)
colnames(initClass1) <- c("Test 1")
initClass1<-initClass1[,Letter:=ifelse(`Test 1`>=90,"T1A",ifelse(`Test 1`<90 & `Test 1`>=80,"T1B",ifelse(`Test 1`<80 & `Test 1`>=70,"T1C",ifelse(`Test 1`<70 & `Test 1`>=60,"T1D","T1F"))))]


StudentData <- data.table(ActTestGrades$StudentID)
StudentData$ActSuccess <- ActTestGrades$ActSuccess
colnames(StudentData)<-c("StudentID","ActSuccess")
StudentData$ForcedInt <- NA
StudentData$ForcedIntStepCnt <- NA
StudentData$ForcedIntSuccess <- NA
StudentData$ForcedIntPoints <- NA
StudentData$NonForcedInt <- NA
StudentData$NonForcedIntStepCnt <- NA
StudentData$NonForcedIntSuccess <- NA
StudentData$NonForcedIntPoints <- NA

runMonteCarlo <- function(initState,mcObj,nSims){
  runs <- c()
  for(i in 1:nSims){
    runs <- c(runs,rmarkovchain(1,mcObj,t0=initState))
  }
  
  d <- names(which.max(table(runs)))
  rtn <- d
}

getInterventionPoints <- function(grades){
  z <- str_split(grades,",")[[1]]
  z2 <- c()
  for(i in 1:length(z)){
    x <- str_sub(z[i],1,1)
    if(x=='I'){
      z2<-c(z2,z[i])
    }
  }
  if(length(z2 == 0)){
    rtn <- z2
  }
  else{
    rtn <- NA
  }
  
}

getSuccess <- function(grades){
  g <- str_split(grades,",")[[1]]
  g2 <- c()
  scores <- 0
  #get just the test and final grades
  for(i in 1:length(g)){
    x <- str_sub(g[i],1,1)
    if(x=='T' | x=='F'){
      g2<-c(g2,str_sub(g[i],-1,-1))
    }
  }
  
  #figure out the overall grade
  for(i in 1:length(g2)){
    scores <- scores + ifelse(g2[i] == 'A',4,ifelse(g2[i]=='B',3,ifelse(g2[i]=='C',2,ifelse(g2[i]=='D',1,0))))
  }
  gpa <- scores / length(g2)
  rtn <- ifelse(gpa>=2.0,1,0)
}

## loop through the students
for(i in 1:nrow(initClass1)){
  state <- initClass1$Letter[i]
  nfstate <- state
  states <- c(state)
  nfstates <- c(state)
  
  cnt <-0
  while(!state %in% absorbingStates(mc_forcedInt)){
    cnt<-cnt+1
    state <- runMonteCarlo(state,mc_forcedInt,1)
    states <- c(states,state)
  }
  StudentData$ForcedIntStepCnt[i] <- cnt
  StudentData$ForcedInt[i] <- paste(states,collapse = ",")
  StudentData$ForcedIntSuccess[i] <- getSuccess(paste(states,collapse = ","))
  StudentData$ForcedIntPoints[i] <- getInterventionPoints(paste(states,collapse = ","))
  
  cnt<-0
  while(!nfstate %in% absorbingStates(mc_nonForcedInt)){
    cnt<-cnt+1
    nfstate <- runMonteCarlo(nfstate,mc_nonForcedInt,1)
    nfstates <- c(nfstates,nfstate)
  }
  StudentData$NonForcedIntStepCnt[i] <- cnt
  StudentData$NonForcedInt[i] <- paste(nfstates,collapse = ",")
  StudentData$NonForcedIntSuccess[i] <- getSuccess(paste(nfstates,collapse = ","))
  StudentData$NonForcedIntPoints[i] <- getInterventionPoints(paste(nfstates,collapse = ","))
}

studentsWhoForcedHelp <- which(StudentData[,ActSuccess==0 & ForcedIntSuccess==1])
studentsWhoNonForcedHelp <- which(StudentData[,ActSuccess==0 & NonForcedIntSuccess==1])

### % stuff
successRate <- sum(StudentData$ActSuccess)/nrow(StudentData)
forcedIntSuccessRate <- sum(StudentData$ForcedIntSuccess)/nrow(StudentData)
nonForcedIntSuccessRate <- sum(StudentData$NonForcedIntSuccess)/nrow(StudentData)


### TODO: check the number of interventions people went to and whether it helped
