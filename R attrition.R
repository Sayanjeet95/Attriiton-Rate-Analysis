
data_set <- read.csv("C:/Users/user/OneDrive/Desktop/HCA/Attrition_details.csv")
View(data_set)
summary(data_set)
dim(data_set)
str(data_set)
dataset <- transform(data_set,
                     Attrition=as.factor(Attrition),
                     BusinessTravel=as.factor(BusinessTravel),
                     Department=as.factor(Department),
                     EducationField=as.factor(EducationField),
                     Gender=as.factor(Gender),
                     JobRole=as.factor(JobRole),
                     MaritalStatus=as.factor(MaritalStatus),
                     OverTime=as.factor(OverTime))

str(dataset)
table(dataset$BusinessTrave)/nrow(dataset)
library(sqldf)
sqldf("select BusinessTravel, COUNT (*) as obs from dataset GROUP BY 1")
dataset_1 <- dataset
dataset_1$BusinessTravel1 <- ifelse(dataset_1$BusinessTravel=='Travel_Rarely',1,0)
dataset_1$BusinessTravel2 <- ifelse(dataset_1$BusinessTravel=='Travel_Frequently',1,0)
View(dataset_1)
sqldf("select Department , COUNT (*) as obs from dataset GROUP BY 1")
dataset_1$Department1 <- ifelse(dataset_1$Department==' Sales',1,0)
dataset_1$Department2 <- ifelse(dataset_1$Department=='Research & Development',1,0)
sqldf("select EducationField, COUNT (*) as obs from dataset GROUP BY 1")
dataset_1$EducationField1 <- ifelse(dataset_1$EducationField=='Life Sciences',1,0)
dataset_1$EducationField2<- ifelse(dataset_1$EducationField=='Medical',1,0)
sqldf("select JobRole, COUNT (*) as obs from dataset GROUP BY 1")
dataset_1$JobRole1 <- ifelse(dataset_1$JobRole=='Sales Executive',1,0)
dataset_1$JobRole2<- ifelse(dataset_1$JobRole=='Research Scientist',1,0)
sqldf("select MaritalStatus, COUNT (*) as obs from dataset GROUP BY 1")
dataset_1$MaritalStatus1 <- ifelse(dataset_1$MaritalStatus=='Married',1,0)
dataset_1$MaritalStatus2<- ifelse(dataset_1$MaritalStatus=='Single',1,0)


View(dataset_1)


dataset_1 <- dataset_1[,-c(3,4,7,11,13)]

trainDataIndex <- sample(1:nrow(dataset_1),0.7*nrow(dataset_1), replace = F)
trainData <-dataset_1[trainDataIndex, ]
testData <- dataset_1[-trainDataIndex, ]
View(trainData)
View(testData)


logit <- glm(Attrition~., data = trainData, family = 'binomial')
summary(logit)

logit_2 <- glm(Attrition ~ DistanceFromHome + EnvironmentSatisfaction  +JobInvolvement +
                 JobSatisfaction    +NumCompaniesWorked    +
                 TotalWorkingYears +  WorkLifeBalance  +YearsInCurrentRole + YearsSinceLastPromotion + BusinessTravel1 
               +BusinessTravel2 + EducationField1 + EducationField2  + MaritalStatus2,
               data = trainData, family = 'binomial')

summary(logit_2)