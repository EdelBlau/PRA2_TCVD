directory <- getwd()
diabetic_data <- read.csv(file.path(directory,"diabetic_data.csv"), header=TRUE, na.strings = c("","NA"), stringsAsFactors=FALSE)
diabetic_data <- subset(diabetic_data,select=c(3,4,5,8,10,13, 14, 19, 20, 21, 22, 23, 24, 48, 49, 50))
str(diabetic_data)
sapply(diabetic_data, function(x) class(x))

sapply(diabetic_data, function(x) sum(is.na(x)))

#Instalacion del paquete VIM
>install.packages("VIM")
# Imputación de valores mediante la función kNN() del paquete VIM 

suppressWarnings(suppressMessages(library(VIM)))

diabetic_data$race <- kNN(diabetic_data)$race 
diabetic_data$diag_1 <- kNN(diabetic_data)$diag_1 
diabetic_data$gender <- kNN(diabetic_data)$gender

sapply(diabetic_data, function(x) sum(is.na(x)))

mean(diabetic_data$time_in_hospital,na.rm=T)+3*sd(diabetic_data$time_in_hospital,na.rm=T)
max(diabetic_data$time_in_hospital, na.rm=T)
hist(diabetic_data$time_in_hospital)


mean(diabetic_data$num_lab_procedures,na.rm=T)+3*sd(diabetic_data$num_procedures,na.rm=T)
max(diabetic_data$num_lab_procedures, na.rm=T)
hist(diabetic_data$num_lab_procedures)

mean(diabetic_data$num_procedures,na.rm=T)+3*sd(diabetic_data$num_procedures,na.rm=T)
max(diabetic_data$num_procedures, na.rm=T)
hist(diabetic_data$num_procedures)


write.csv(diabetic_data, "diabetic_data_clean.csv")

#sexo
diabetic_data$gender <- as.factor(diabetic_data$gender)
#edad
diabetic_data$age <- as.factor(diabetic_data$age)
#etnia
diabetic_data$race <- as.factor(diabetic_data$race)
#diagnostico
diabetic_data$diag_1 <- as.factor(diabetic_data$diag_1)

#histograma y qqplot
par(mfrow=c(2,2))
for(i in 1:ncol(diabetic_data)) {
  if (is.numeric(diabetic_data[,i])){
    qqnorm(diabetic_data[,i],main = paste("Normal Q-Q Plot for ",colnames(diabetic_data)[i]))
    qqline(diabetic_data[,i],col="red")
    hist(diabetic_data[,i], 
         main=paste("Histogram for ", colnames(diabetic_data)[i]), 
         xlab=colnames(diabetic_data)[i], freq = FALSE)
  }
}

#normalidad en las variables
library(nortest) 
alpha = 0.05 
col.names = colnames(diabetic_data) 

for (i in 1:ncol(diabetic_data)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n") 
  if (is.integer(diabetic_data[,i]) | is.numeric(diabetic_data[,i])) {
    p_val = ad.test(diabetic_data[,i])$p.value
    if (p_val < alpha) { 
      cat(col.names[i]) 
      # Format output 
      if (i < ncol(diabetic_data) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}


#correlacion con test de spearman
cor.test(diabetic_data$readmitted,diabetic_data$time_in_hospital,method="spearman")

#coeficiente tau-b
cor(diabetic_data$readmitted,diabetic_data$number_diagnoses,method="kendall", use="pairwise")
cor.test(diabetic_data$readmitted,diabetic_data$number_diagnoses,method="kendall", use="pairwise")

cor(diabetic_data$readmitted,diabetic_data$time_in_hospital,method="kendall", use="pairwise")
cor.test(diabetic_data$readmitted,diabetic_data$time_in_hospital,method="kendall", use="pairwise")

cor(diabetic_data$readmitted,diabetic_data$num_lab_procedures,method="kendall", use="pairwise")
cor.test(diabetic_data$readmitted,diabetic_data$num_lab_procedures,method="kendall", use="pairwise")

cor(diabetic_data$readmitted,diabetic_data$num_procedures,method="kendall", use="pairwise")
cor.test(diabetic_data$readmitted,diabetic_data$num_procedures,method="kendall", use="pairwise")

#wilcoxom para datos dicotomicos
wilcox.test(diabetic_data$readmitted~diabetic_data$gender)
wilcox.test(diabetic_data$readmitted~diabetic_data$change)
wilcox.test(diabetic_data$readmitted~diabetic_data$diabetesMed)

#kruskal-wallis para datos categoricos
kruskal.test(diabetic_data$readmitted~diabetic_data$race)
diabetic_data$max_glu_serum <- as.factor(diabetic_data$max_glu_serum)
kruskal.test(diabetic_data$readmitted~diabetic_data$max_glu_serum)
diabetic_data$A1Cresult <- as.factor(diabetic_data$A1Cresult)
kruskal.test(diabetic_data$readmitted~diabetic_data$A1Cresult)

#modelo de regresion lógica
train <- diabetic_data[1:80000,]
test <- diabetic_data[80001:88900,]

model <- glm(readmitted ~.,family=binomial(link='logit'),data=train)
summary(model)

model2 <- glm(readmitted ~ (age+num_procedures+number_diagnoses+diabetesMed),family=binomial(link='logit'),data=train)
summary(model2)

#precision del modelo
fitted <- predict(model2,newdata=test,type='response')
fitted <- ifelse(fitted > 0.5,1,0)
Error <- mean(fitted != test$readmitted)
print(paste('Accuracy',1-Error))

#representacion grafica
par(mfrow = c(1, 2))

with(diabetic_data, boxplot(num_procedures ~ readmitted, 
                            ylab = "Num procedures", 
                            xlab = "Reingreso",
                            main = "Figure A",
                            outline = FALSE))

with <- diabetic_data[diabetic_data$readmitted == 1, ]
without <- diabetic_data[diabetic_data$readmitted == 0, ]

plot(density(with$num_procedures), 
     xlim = c(0, 7),
     ylim = c(0, 2),
     xlab = "num_procedures",
     main = "Figure B",
     lwd = 2)
lines(density(without$num_procedures), 
      col = "red",
      lwd = 2)
legend("topleft", 
       col = c("black", "red"), 
       legend = c("With Readmission", "Without Readmission"), 
       lwd = 2,
       bty = "n")

par(mfrow = c(1, 2))

with(diabetic_data, boxplot(number_diagnoses ~ readmitted, 
                            ylab = "Num diagnoses", 
                            xlab = "Reingreso",
                            main = "Figure A",
                            outline = FALSE))

with <- diabetic_data[diabetic_data$readmitted == 1, ]
without <- diabetic_data[diabetic_data$readmitted == 0, ]

plot(density(with$number_diagnoses), 
     xlim = c(0, 10),
     ylim = c(0, 2),
     xlab = "number_diagnoses",
     main = "Figure B",
     lwd = 2)
lines(density(without$number_diagnoses), 
      col = "red",
      lwd = 2)
legend("topleft", 
       col = c("black", "red"), 
       legend = c("With Readmission", "Without Readmission"), 
       lwd = 2,
       bty = "n")