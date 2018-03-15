library(data.table)


#bring in data
#math class
student.mat <- data.table(read.csv("student-mat.csv", sep=";"))
#portugese class
student.por <- data.table(read.csv("student-por.csv", sep=";"))

## G1 = first period grade, G2 = second period grade, G3 = final grade

## set success variable
student.mat <- student.mat[,Success:=ifelse(G3 >= 14, 1,0)]
student.por <- student.por[,Success:=ifelse(G3 >= 14, 1,0)]

student.mat <- data.frame(sapply(student.mat,as.factor))

## set train and test set for math class
student.mat.train <- student.mat[1:350,]
student.mat.test  <- student.mat[301:395,]

## set train and test set for portugese class
student.por.train <- student.por[1:325,]
student.por.test  <- student.por[326:649,]

### first model is without any grades

matModelNoGrades <- glm(Success ~ . , family = binomial(link = "logit"), data =  subset(student.mat.train, select = -c(G1,G2,G3)))

fitted.results <- predict(matModelNoGrades,newdata=student.mat.test)
fitted.results <- ifelse(fitted.results>0.5,1,0)

#student.mat.test <- cbind(student.mat.test,fitted.results)
noGradeAcc <- 1-mean(fitted.results != student.mat.test$Success)

matModelNoGrades2 <- glm(Success ~ Medu + paid + absences + reason + failures + Dalc, family=binomial, data = subset(student.mat.train, select = -c(G1,G2,G3)))


### second model is with G1 and qualitative data
matModelWG1 <- glm(Success ~ . , family = binomial(link = "logit"), data =  subset(student.mat.train, select = -c(G2,G3)))


fitted.results2 <- predict(matModelWG1,newdata=student.mat.test)
fitted.results2 <- ifelse(fitted.results2>0.5,1,0)

G1GradeAcc <- 1-mean(fitted.results2 != student.mat.test$Success)

### third model is with G1 and G2 and qualitative data

matModelWG1G2 <- glm(Success ~ . -G3, family = binomial(link = "logit"), data =  student.mat.train)
