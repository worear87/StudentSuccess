library(markovchain)
library(readxl)
library(ggplot2)
library(plyr)
library(reshape)
library(readxl)
library(data.table)
library(expm)

#### pull in the transition matrices ####
T1toT2Tran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_1")
T2toT3Tran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_2")
T3toT4Tran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_3")
T4toT5Tran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_4")
T5toT6Tran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_5")
T6toFinTran <- read_excel("JohnClassTest_MC.xlsx", sheet = "Exam_6")

#get rid of first column, don't need it
T1toT2Tran$X__1 <- NULL
T2toT3Tran$X__1 <- NULL
T3toT4Tran$X__1 <- NULL
T4toT5Tran$X__1 <- NULL
T5toT6Tran$X__1 <- NULL
T6toFinTran$X__1 <- NULL

#### pull in the student Grades ####
ActTestGrades <- data.table(read_excel("JohnClassTest_MC.xlsx", sheet = "Class1"))

#get rid of columns and rows you don't need from the actual grades
ActTestGrades <- ActTestGrades[1:51,1:9]

ActTestGrades <- ActTestGrades[,ActSuccess:=ifelse(FinalGrade>=70.0,1,0)]

#### Make the markov chain objects
mc1 <- new("markovchain",states=colnames(T1toT2Tran), transitionMatrix=as.matrix(T1toT2Tran))
mc2 <- new("markovchain",states=colnames(T2toT3Tran), transitionMatrix=as.matrix(T2toT3Tran))
mc3 <- new("markovchain",states=colnames(T3toT4Tran), transitionMatrix=as.matrix(T3toT4Tran))
mc4 <- new("markovchain",states=colnames(T4toT5Tran), transitionMatrix=as.matrix(T4toT5Tran))
mc5 <- new("markovchain",states=colnames(T5toT6Tran), transitionMatrix=as.matrix(T5toT6Tran))
mc6 <- new("markovchain",states=colnames(T6toFinTran), transitionMatrix=as.matrix(T6toFinTran))

chains <- list(mc1,mc2,mc3,mc4,mc5,mc6)

initClass1<-data.table(ActTestGrades[,2])
initClass1<-initClass1[,Letter:=ifelse(`Test 1`>=90,"A",ifelse(`Test 1`<90 & `Test 1`>=80,"B",ifelse(`Test 1`<80 & `Test 1`>=70,"C",ifelse(`Test 1`<70 & `Test 1`>=60,"D","F"))))]
initClass1<-initClass1[,Test1PredGrade:=ifelse(`Test 1`>=90,4,ifelse(`Test 1`<90 & `Test 1`>=80,3,ifelse(`Test 1`<80 & `Test 1`>=70,2,ifelse(`Test 1`<70 & `Test 1`>=60,1,0))))]

getRow <- function(state){
  rtn <- ifelse(state == 'A',1,ifelse(state=='B',2,ifelse(state=='C',3,ifelse(state=='D',4,5))))
}

getLetterGrade <- function(grade){
  rtn <- ifelse(grade>=4.0,"A",ifelse(grade<4.0 & grade>=3.0,"B",ifelse(grade<3.0 & grade>=2.0,"C",ifelse(grade<2.0 & grade>=1.0,"D","F"))))
}

getGpaGrade <- function(letter){
  rtn <- ifelse(letter == 'A',4,ifelse(letter=='B',3,ifelse(letter=='C',2,ifelse(letter=='D',1,0))))
}

getGpaGradeFromNum <- function(grade){
  rtn <- ifelse(grade>=90,4,ifelse(grade<90 & grade>=80,3,ifelse(grade<80 & grade>=70,2,ifelse(grade<70 & grade>=60,1,0))))
}

PredictedGrades <- data.table(cbind(ActTestGrades$StudentID,initClass1$Test1PredGrade))
colnames(PredictedGrades) <- c("StudentID","Test1PredGrade")
PredictedGrades$Test2PredGrade <- NA
PredictedGrades$Test3PredGrade <- NA
PredictedGrades$Test4PredGrade <- NA
PredictedGrades$Test5PredGrade <- NA
PredictedGrades$Test6PredGrade <- NA
PredictedGrades$FinalExamPredGrade <- NA

g <- c(4,3,2,1,0) ## used for the dot product


runMonteCarlo <- function(initState,mcObj,nSims){
  runs <- c()
  for(i in 1:nSims){
    runs <- c(runs,rmarkovchain(1,mcObj,t0=initState))
  }
  
  d <- names(which.max(table(runs)))
  rtn <- getGpaGrade(d)
}


for(n in 1:51)
{
  #test 2 prediction
  #PredictedGrades$Test2PredGrade[n]<- mc1[getRow(getLetterGrade(PredictedGrades$Test1PredGrade[n])),] %*% g
  #PredictedGrades$Test2PredGrade[n]<- getGpaGrade(rmarkovchain(1,mc1,t0=getLetterGrade(PredictedGrades$Test1PredGrade[n])))
  PredictedGrades$Test2PredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test1PredGrade[n]),mc1,1000)
  
  #test 3 prediction
  #PredictedGrades$Test3PredGrade[n]<- mc2[getRow(getLetterGrade(PredictedGrades$Test2PredGrade[n])),] %*% g
  #PredictedGrades$Test3PredGrade[n]<- getGpaGrade(rmarkovchain(1,mc2,t0=getLetterGrade(PredictedGrades$Test2PredGrade[n])))
  PredictedGrades$Test3PredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test2PredGrade[n]),mc2,1000)
  
  #test 4 prediction
  #PredictedGrades$Test4PredGrade[n]<- mc3[getRow(getLetterGrade(PredictedGrades$Test3PredGrade[n])),] %*% g
  #PredictedGrades$Test4PredGrade[n]<- getGpaGrade(rmarkovchain(1,mc3,t0=getLetterGrade(PredictedGrades$Test3PredGrade[n])))
  PredictedGrades$Test4PredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test3PredGrade[n]),mc3,1000)
  
  #test 5 prediction
  #PredictedGrades$Test5PredGrade[n]<- mc4[getRow(getLetterGrade(PredictedGrades$Test4PredGrade[n])),] %*% g
  #PredictedGrades$Test5PredGrade[n]<- getGpaGrade(rmarkovchain(1,mc4,t0=getLetterGrade(PredictedGrades$Test4PredGrade[n])))
  PredictedGrades$Test5PredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test4PredGrade[n]),mc4,1000)
  
  #test 6 prediction
  #PredictedGrades$Test6PredGrade[n]<- mc5[getRow(getLetterGrade(PredictedGrades$Test5PredGrade[n])),] %*% g
  #PredictedGrades$Test6PredGrade[n]<- getGpaGrade(rmarkovchain(1,mc5,t0=getLetterGrade(PredictedGrades$Test5PredGrade[n])))
  PredictedGrades$Test6PredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test5PredGrade[n]),mc5,1000)
  
  #final exam prediction
  #PredictedGrades$Test6PredGrade[n]<- mc5[getRow(getLetterGrade(PredictedGrades$Test5PredGrade[n])),] %*% g
  #PredictedGrades$FinalExamPredGrade[n]<- getGpaGrade(rmarkovchain(1,mc6,t0=getLetterGrade(PredictedGrades$Test6PredGrade[n])))
  PredictedGrades$FinalExamPredGrade[n]<- runMonteCarlo(getLetterGrade(PredictedGrades$Test6PredGrade[n]),mc6,1000)
}

PredictedGrades$PredFinalGrade <- PredictedGrades[,((Test1PredGrade+Test2PredGrade+Test3PredGrade+Test4PredGrade+Test5PredGrade+Test6PredGrade)/6)*.8
                                                + FinalExamPredGrade*.2]

PredictedGrades <- PredictedGrades[,PredSuccess:=ifelse(PredFinalGrade>=2.0,1,0)]

Ovr <- data.table(cbind(ActTestGrades$StudentID,getGpaGradeFromNum(ActTestGrades$FinalGrade),ActTestGrades$ActSuccess,
                        PredictedGrades$PredFinalGrade,PredictedGrades$PredSuccess))
colnames(Ovr) <- c("StudentID","ActFinalGrade","ActSuccess","PredFinalGrade","PredSuccess")

Ovr <- Ovr[,Accuracy:=ifelse(PredSuccess==ActSuccess,0,ifelse(PredSuccess>ActSuccess,1,-1))]
#write.csv(Ovr,file="MultMCsCalc3.csv",row.names = F)

## Compare the Tests
StudentData <- merge(ActTestGrades,PredictedGrades, on="StudentID")
StudentData$`Test 1`<-getGpaGradeFromNum(StudentData$`Test 1`)
StudentData$`Test 2`<-getGpaGradeFromNum(StudentData$`Test 2`)
StudentData$`Test 3`<-getGpaGradeFromNum(StudentData$`Test 3`)
StudentData$`Test 4`<-getGpaGradeFromNum(StudentData$`Test 4`)
StudentData$`Test 5`<-getGpaGradeFromNum(StudentData$`Test 5`)
StudentData$`Test 6`<-getGpaGradeFromNum(StudentData$`Test 6`)
StudentData$Final<-getGpaGradeFromNum(StudentData$Final)
StudentData$FinalGrade<-getGpaGradeFromNum(StudentData$FinalGrade)


StudentData <- StudentData[,Test1Acc:=ifelse(`Test 1`==Test1PredGrade,0,ifelse(Test1PredGrade>`Test 1`,1,-1))] ##dumb, don't need this
StudentData <- StudentData[,Test2Acc:=ifelse(`Test 2`==Test2PredGrade,0,ifelse(Test2PredGrade>`Test 2`,1,-1))]
StudentData <- StudentData[,Test3Acc:=ifelse(`Test 3`==Test3PredGrade,0,ifelse(Test3PredGrade>`Test 3`,1,-1))]
StudentData <- StudentData[,Test4Acc:=ifelse(`Test 4`==Test4PredGrade,0,ifelse(Test4PredGrade>`Test 4`,1,-1))]
StudentData <- StudentData[,Test5Acc:=ifelse(`Test 5`==Test5PredGrade,0,ifelse(Test5PredGrade>`Test 5`,1,-1))]
StudentData <- StudentData[,Test6Acc:=ifelse(`Test 6`==Test6PredGrade,0,ifelse(Test6PredGrade>`Test 6`,1,-1))]
StudentData <- StudentData[,FinalExamAcc:=ifelse(Final==FinalExamPredGrade,0,ifelse(FinalExamPredGrade>Final,1,-1))]
