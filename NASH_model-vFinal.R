## Model V1 13.12 15.56
# Load table into R
#install.packages("readxl")
#install.packages("readODS")
library(readODS)
library(readr)
library(dplyr)
library(ggplot2)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

table_raw<-readxl::read_excel("NASH factores and odds ratio.xlsx",col_names = TRUE)
names = unique(table_raw$Environmental_Factor)

pop = 10000
ID <- 1:pop
DF <- data.frame(ID)
vector_betas <- data.frame(ID)



for (element in names){

    P <- table_raw %>%
      filter(Environmental_Factor == element)
    
    # If the number of rows is just 1 means that it is a continuous variable, 
    # where the betas need to be multiplied by the variable number
    if (nrow(P) == 1)
    {
  P<- P %>%
      select(pop_percentage, Betas, Betas2, N1, N2, Distribution)
    
    # The distribution type is defined in the table (0= normal, 1= exponential)
      if(P$Distribution == 0)
      {
        p_data = rnorm(pop, mean=P$N1, sd=P$N2)
      }
      if(P$Distribution == 1)
      {
        p_data = rexp(pop, P$N1)
        p_data <- p_data * P$N2 / max(p_data)
      }
      if(P$Distribution == 2)
      {
        p_data = rchisq(pop, P$N1)
              }
    p_data <- abs(round(p_data,0))
    #Continuous model
    betas <- p_data* P$Betas +  p_data^2* P$Betas2
    #For insertion in the DF
    factor <- p_data
    }

  
  else{
    P <- P %>%
      select(pop_percentage, Cat, Betas)
    factor=sample(as.factor(P$Cat), pop, P$pop_percentage, replace=TRUE)
    
    #From factor to the associated betas
    betas <- factor
    levels(betas)<- P$Betas
  }
  
  #Dataframe of factors
  DF = cbind(DF,factor)
  #Dataframe of betas associated to factors and patients
  vector_betas <- cbind(vector_betas, as.numeric(paste(betas)))
}

#Inspecting the elements
View(vector_betas)
View(DF)

#Inserting names
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

# The vector beta is ordered in order to find the beta value that might be 
# substracted from the vector betas (alfa) in order to have a predefined percentage 
# (NAFLD_rate) of the population with positive betas and another the rest of the 
# population with negative values. 
s_beta <- sort(beta)

NAFLD_rate <- 0.3
alfa = -s_beta[pop*(1-NAFLD_rate)]

beta_vector = alfa + beta
beta_df = data.frame(beta_vector)
beta_df$cumsum = cumsum(beta_vector)

p<- ggplot(beta_df, aes(x=beta))
p+geom_histogram(binwidth=0.2)
p+geom_density()

#setting an age variable


summary(DF2)

#Computing the illnes or not with normalised betas to mean = 0.
odds2= exp(beta_vector)
Podds2 = (odds2/(odds2+1))
Y2=c()
for (element in Podds2){
  a=sample(c("ill","healthy"), 1,  c(element,1-element), replace = TRUE)
  
  Y2 = c(Y2,a)
}
Y = as.factor(Y2)
DF2 <- cbind(DF2,Y)

#Fixemnos que les proporcions de malalts i no malalts no són 50 i 50, sino mes aviat 43 i 57, perque? 
#Extreient-li la mediana passa semblant

#Inserir-hi la questió d'edat, seria generar una funció beta = a*(AGE) +b*(AGE)^2, per aixo es necessita generar una noav columna
#(mutate) a partir de la columna edat i els valors a i b (taula).
#Per altre banda s'ha de crear una columna d'edat amb una distribució normal


# Randomly taking out 30% of the variables and reordering variables



#Trobar el 30% de 15 numeros
#The number of actual variables is 15
n=round(15*0.3,0) #We wil take down the 30%
down_vector <- sample(2:14, size = n)

DF_down <-  subset(DF2, select = -down_vector )
# Different method same output -- DF2 <- DF2[, -down_vector]
var_reord <- sample(2:(length(colnames(DF_down))-1), size = (length(colnames(DF_down))-2))

var_reord2 <- c(1,var_reord, length(DF2))
Final_DF <- subset(DF2, select = var_reord2 )

View(Final_DF)
summary(Final_DF)


library("writexl")
write_xlsx(Final_DF,"Final_DF.xlsx")

