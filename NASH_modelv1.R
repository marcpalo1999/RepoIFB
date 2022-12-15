## Model V1 13.12 15.56
# Load table into R
#install.packages("readxl")
#install.packages("readODS")
library(readODS)
library(readr)
library(dplyr)
setwd("~/Desktop/Master/InfoMedica/RepoIFB")
table_raw<-readxl::read_excel("NASH factores and odds ratio.xlsx",col_names = TRUE)
names = unique(table_raw$Environmental_Factor)

pop = 1000
ID <- 1:pop
DF <- data.frame(ID)
vector_betas <- data.frame(ID)

for (element in names){
  P <- table_raw %>%
    filter(Environmental_Factor == element)%>%
    select(General_percentage_NAFLD_Patients, Cat, Betas)
    
  factor=sample(as.factor(P$Cat), pop, P$General_percentage_NAFLD_Patients/100, replace=TRUE)
  
  
  DF <-DF%>%
    cbind(factor)
  betas <- factor
  levels(betas)<- P$Betas
  vector_betas <- vector_betas%>%
    cbind(as.numeric(paste(betas)))
}


names(DF)= c("ID",names)
names(vector_betas)= c("ID",names)
vector_betas = tibble(vector_betas)
beta<- rowSums(vector_betas)-as.numeric(vector_betas$ID)

# We have to assign the RHS to an object to save the column to the object.
# It can be the same as the original tibble.
#vector_betas = vector_betas %>% rowwise() %>% mutate(myTidySum = sum(c_across(all_of())))

#Funcion de regresion logistica 

#=======Mirar la prevalencia====

odds= exp(beta)
Podds = (odds/(odds+1))

Y=c()
for (element in Podds){
  a=sample(c("ill","healthy"), 1,  c(element,1-element), replace = TRUE)
  
  Y = c(Y,a)
}

DF <- cbind(DF,Y)


View(table_raw)
View(vector_betas)
View(DF)

#Preguntas
#El alfa de donde l sacamos
#porque nos sale tanto enfermo?
#Meter los doomies (como deben ser?)
#De donde sacamos las probabilidades
#Que nombres tienen que tener las variables

