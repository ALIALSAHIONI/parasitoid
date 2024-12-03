#### Install packages 

install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
install.packages(c("tidyverse"), dependencies = TRUE)

 
#installing packages
 install.packages("readxl")
 install.packages("openxlsx")
 library(readxl)
 
 # Correct file path with proper quotes and escape characters
 data1 <- read_excel("C:/Users/cfp/Downloads/rawdata.xlsx")
 
 # Print the data
 view(data)
 
 # Logistic regression for host dropping and escaping
 logit_dropping <- glm(dropping ~ host.weight * parasitoid.species, 
 data = data1,                  
 family = binomial(link = "logit"))
summary(logit_dropping)
logit_escaping <- glm(escaping ~ host.weight * parasitoid.species,                  
                      data = data1, 
                      family = binomial(link = "logit"))summary(logit_escaping)
