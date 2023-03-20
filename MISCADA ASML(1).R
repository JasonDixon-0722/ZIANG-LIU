bank_personal_loan <- read.csv("C:/Users/SHAH/Downloads/Classification-20230313T095131Z-001/Classification/data/bank_personal_loan.csv")
View(bank_personal_loan)

str(bank_personal_loan)
summary(bank_personal_loan)

install.packages("lattice")
library(lattice)

#Visualizations of the data
bwplot(~Age,bank_personal_loan)
bwplot(~Income,bank_personal_loan)
histogram(Age~Personal.Loan,bank_personal_loan)
bwplot(~Income |Family,bank_personal_loan)
histogram(Income~CreditCard,bank_personal_loan)
densityplot(~Mortgage,bank_personal_loan)

bargraph
library(tidyverse)
personal_loan_plot <-bank_personal_loan %>%
  gather(factors, length,1:4)%>%
  group_by(Personal.Loan,factors)%>%
  summarise(mn=mean(length),sdev=sd(length))

head(personal_loan_plot)
summary(personal_loan_plot)


#Convert all categorical variables to factors
bank_personal_loan$Education <- as.factor(bank_personal_loan$Education)
bank_personal_loan$Personal.Loan <- as.factor(bank_personal_loan$Personal.Loan)
bank_personal_loan$Securities.Account <- as.factor(bank_personal_loan$Securities.Account)
bank_personal_loan$CD.Account <- as.factor(bank_personal_loan$CD.Account)
bank_personal_loan$Online <- as.factor(bank_personal_loan$Online)
bank_personal_loan$CreditCard <- as.factor(bank_personal_loan$CreditCard)

#Run logistic regression model with all parameters
mod1 <- glm(Personal.Loan ~ .
            , data = bank_personal_loan, family= 'binomial')
summary(mod1)

#run AIC to see the best model to fit
install.packages("MASS")
library(MASS)

mod_step <-stepAIC(mod1, direction = 'backward', trace = FALSE )
mod_step


install.packages("caTools")
library(caTools)

split<- sample.split(bank_personal_loan, SplitRatio = 0.8)
split

train<- subset(bank_personal_loan, split="TRUE")
test<- subset(bank_personal_loan, split="FALSE")

#Apply logistic regression analysis on the train subset
mymodel<- glm(Personal.Loan ~ Age + Experience + Income + ZIP.Code + Family + 
                CCAvg + Education + Mortgage + Securities.Account + CD.Account + 
                Online + CreditCard, data = train, family= 'binomial')
summary(mymodel)

#Run the test data through the model
tes <- predict(mymodel,test,type="response")
tes

tes <- predict(mymodel,train,type="response")
tes

#validate the model (confusion matrix)
confmatrix <- table(actual_value= train$Personal.Loan, predicted_value= tes >0.5)
confmatrix

#Accuracy
(confmatrix[[1,1]]+ confmatrix[[2,2]]) / sum(confmatrix)

mymodel2 <- stepAIC(mymodel)
summary(mymodel2)
