train<-M2022train

mosaicplot(train$Major~train$Grade)#psychology had more As, Economics had more Fs
mosaicplot(train$Seniority~train$Grade)# freshman earn

v<-sample(1:nrow(train))
v[1:5]
trainScrambled<-train[v, ]
#one step crossvalidation
trainSample<-trainScrambled[nrow(trainScrambled)-10:nrow(trainScrambled), ]
myprediction<-trainSample

#CS MAJOR
CSMajors<-subset(train,train$Major=="CS")
CSMajorsSenior<-subset(CSMajors, CSMajors$Seniority=="Senior" & CSMajors$Grade=="C")
hist(CSMajorsSenior$Score)
CSMajorsA<-subset(CSMajors,CSMajors$Grade=="A")
CSMajorsB<-subset(CSMajors,CSMajors$Grade=="B")
CSMajorsC<-subset(CSMajors,CSMajors$Grade=="C")
CSMajorsD<-subset(CSMajors,CSMajors$Grade=="D")
CSMajorsF<-subset(CSMajors,CSMajors$Grade=="F")

hist(CSMajorsA$Score, main="Histogram of CS Majors with A", xlab="Score") #90-100
hist(CSMajorsB$Score, main="Histogram of CS Majors with B", xlab="Score") #80-90
hist(CSMajorsC$Score, main="Histogram of CS Majors with C", xlab="Score")#60-80
hist(CSMajorsD$Score, main="Histogram of CS Majors with D", xlab="Score") #50-60
hist(CSMajorsF$Score, main="Histogram of CS Majors with F", xlab="Score") #<50

#Psych MAJOR
PsychMajors<-subset(train, train$Major=="Psychology")
PsychMajorsA<-subset(PsychMajors, PsychMajors$Grade=="A")
PsychMajorsB<-subset(PsychMajors, PsychMajors$Grade=="B")
PsychMajorsC<-subset(PsychMajors, PsychMajors$Grade=="C")
PsychMajorsD<-subset(PsychMajors, PsychMajors$Grade=="D")
PsychMajorsF<-subset(PsychMajors, PsychMajors$Grade=="F")
PsychMajorsSenior<-subset(PsychMajors, PsychMajors$Seniority=="Senior" & PsychMajors$Grade=="A")
PsychMajorsFreshman<-subset(PsychMajors, PsychMajors$Freshman=="Freshman" & PsychMajors$Grade=="A")


hist(PsychMajorsA$Score, main="Histogram of Psychology Majors with A", xlab="Score") #90-100 & 70-80
hist(PsychMajorsB$Score,main="Histogram of Psychology Majors with B", xlab="Score") #45-50, 55-60, 65-70
hist(PsychMajorsC$Score, main="Histogram of Psychology Majors with C", xlab="Score")#35-60
hist(PsychMajorsD$Score, main="Histogram of Psychology Majors with D", xlab="Score") #20-35, 30-40
hist(PsychMajorsF$Score, main="Histogram of Psychology Majors with F", xlab="Score") #<30

#Econ MAJOR
EconMajors<-subset(train, train$Major=="Economics")
EconMajorsA<-subset(EconMajors, EconMajors$Grade=="A")
EconMajorsB<-subset(EconMajors, EconMajors$Grade=="B")
EconMajorsC<-subset(EconMajors, EconMajors$Grade=="C")
EconMajorsD<-subset(EconMajors, EconMajors$Grade=="D")
EconMajorsF<-subset(EconMajors, EconMajors$Grade=="F")

hist(EconMajorsA$Score, main="Histogram of Economics Majors with A", xlab="Score") #85-100 
hist(EconMajorsB$Score, main="Histogram of Economics Majors with B", xlab="Score") #50-80
hist(EconMajorsC$Score, main="Histogram of Economics Majors with C", xlab="Score")#50-60
hist(EconMajorsD$Score, main="Histogram of Economics Majors with D", xlab="Score") #10-20, 40-50
hist(EconMajorsF$Score, main="Histogram of Economics Majors with F", xlab="Score") #<30




#Stat MAJOR
StatMajors<-subset(train, train$Major=="Statistics")
StatMajorsA<-subset(StatMajors, StatMajors$Grade=="A")
StatMajorsB<-subset(StatMajors, StatMajors$Grade=="B")
StatMajorsC<-subset(StatMajors, StatMajors$Grade=="C")
StatMajorsD<-subset(StatMajors, StatMajors$Grade=="D")
StatMajorsF<-subset(StatMajors, StatMajors$Grade=="F")
StratMajorsBSoph<-subset(StatMajorsB, StatMajorsB$Seniority=="Sophomore")
StratMajorsBSenior<-subset(StatMajorsB, StatMajorsB$Seniority=="Senior")
StratMajorsASoph<-subset(StatMajorsA, StatMajorsA$Seniority=="Sophomore")

hist(StatMajorsA$Score, main="Histogram of Statistics Majors with A", xlab="Score") #90-100 
hist(StatMajorsB$Score, main="Histogram of Statistics Majors with B", xlab="Score") #70-90
hist(StatMajorsC$Score, main="Histogram of Statistics Majors with C", xlab="Score")#50-70
hist(StatMajorsD$Score, main="Histogram of Statistics Majors with D", xlab="Score") #30-40
hist(StatMajorsF$Score, main="Histogram of Statistics Majors with F", xlab="Score") #<30
hist(StratMajorsAF$Score)
hist(StratMajorsBSenior$Score)
hist(StratMajorsBSoph$Score)




myprediction<-M2022testSNoGrade
decision <- rep('F',nrow(myprediction))
decision[myprediction$Major=="CS" & myprediction$Score>=90] <- 'A'
decision[myprediction$Major=="CS" & (myprediction$Score>80 & myprediction$Score<90)] <- 'B'
decision[myprediction$Major=="CS" & (myprediction$Score>60 & myprediction$Score<80)] <- 'C'
decision[myprediction$Major=="CS"& (myprediction$Score>50 & myprediction$Score<60)] <- 'D'

decision[myprediction$Major=="Psychology" & (myprediction$Score>=90 | (myprediction$Score>70 & myprediction$Score<=80))] <- 'A'
decision[myprediction$Major=="Psychology" & ((myprediction$Score>45 & myprediction$Score<=50) | (myprediction$Score>55 & myprediction$Score<=60)| (myprediction$Score>65 & myprediction$Score<=70))] <- 'B'
decision[myprediction$Major=="Psychology" & myprediction$Score<80] <- 'C'
decision[myprediction$Major=="Psychology"& myprediction$Score<60] <- 'D'

decision[myprediction$Major=="Statistics" & myprediction$Score>=90] <- 'A'
decision[myprediction$Major=="Statistics" & (myprediction$Score>70 & myprediction$Score<90)] <- 'B'
decision[myprediction$Major=="Statistics" & (myprediction$Score>50 & myprediction$Score<70)] <- 'C'
decision[myprediction$Major=="Statistics"& (myprediction$Score>30 & myprediction$Score<=40)] <- 'D'
decision[myprediction$Major=="Statistics" & myprediction$Seniority=="Sophomore" & ((myprediction$Score>=94 & myprediction$Score<=96) | (myprediction$Score>98))] <-'A'

decision[myprediction$Major=="Economics" & myprediction$Score>=85] <- 'A'
decision[myprediction$Major=="Economics" & (myprediction$Score>60 & myprediction$Score<85)] <- 'B'
decision[myprediction$Major=="Economics" & (myprediction$Score>50 & myprediction$Score<=60)] <- 'C'
decision[myprediction$Major=="Economics"& (myprediction$Score>10 & myprediction$Score<=20) | (myprediction$Score>40 & myprediction$Score<50)] <- 'D'

myprediction$Grade <-decision
error <- mean(train$Grade!= myprediction$Grade)
error

M2022submissionS$Grade<-decision
M2022submissionS
write.csv(M2022submissionS, 'submission.csv', row.names=FALSE) 



