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

matModelNoGrades2 <- glm(Success ~ Medu + paid + absences + reason + failures + Dalc, family=binomial, data = student.mat.train)

### second model is with G1 and qualitative data
matModelWG1 <- glm(Success ~ . , family = binomial(link = "logit"), data =  subset(student.mat.train, select = -c(G2,G3)))

### third model is with G1 and G2 and qualitative data

matModelWG1G2 <- glm(Success ~ . -G3, family = binomial(link = "logit"), data =  student.mat.train)
