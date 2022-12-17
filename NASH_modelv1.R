## Model V1 13.12 15.56
# Load table into R
#install.packages("readxl")
#install.packages("readODS")
library(readODS)
library(readr)
library(dplyr)
library(ggplot2)
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
    select(pop_percentage, Cat, Betas)
    
  factor=sample(as.factor(P$Cat), pop, P$pop_percentage, replace=TRUE)
  
  
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

DF2 <- DF
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

beta_vector = beta-mean(beta)
beta_df = data.frame(beta_vector)
beta_df$cumsum = cumsum(beta_vector)

p<- ggplot(beta_df, aes(x=beta))
p+geom_histogram(binwidth=0.2)
p+geom_density()

#setting an age variable
AGE = rnorm(1000, mean=50, sd=15)
hist(AGE)
AGE <- abs(round(AGE,0))
DF2 <- cbind(DF2,AGE)
summary(DF2)

#Computing the illnes or not with normalised betas to mean = 0.
odds2= exp(beta_vector)
Podds2 = (odds2/(odds2+1))
Y2=c()
for (element in Podds2){
  a=sample(c("ill","healthy"), 1,  c(element,1-element), replace = TRUE)
  
  Y2 = c(Y2,a)
}

DF2 <- cbind(DF2,as.factor(Y2))

#Fixemnos que les proporcions de malalts i no malalts no són 50 i 50, sino mes aviat 43 i 57, perque? 
#Extreient-li la mediana passa semblant

#Inserir-hi la questió d'edat, seria generar una funció beta = a*(AGE) +b*(AGE)^2, per aixo es necessita generar una noav columna
#(mutate) a partir de la columna edat i els valors a i b (taula).
#Per altre banda s'ha de crear una columna d'edat amb una distribució normal



