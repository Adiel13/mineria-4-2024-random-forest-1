install.packages("randomForest")
library(randomForest)

hogar <- read.csv('C:/Users/kevin/OneDrive/Documentos/MIIC/Mineria/Censo2018/HOGAR_BDP.csv', sep=',')
hogar <- na.omit(hogar)

hogar <- hogar[, c("DEPARTAMENTO", "PCH1", "PCH2","PCH3","PCH4","PCH5", "PCH8", "PCH9_A",
                   "PCH9_B","PCH9_C","PCH9_D","PCH9_E","PCH9_F","PCH9_G","PCH9_H")]

hogar$DEPARTAMENTO <- as.factor(hogar$DEPARTAMENTO)

set.seed(100)
hogar <- hogar[sample(1:nrow(hogar)),]

index <-sample(1:nrow(hogar), 0.8*nrow(hogar))

train <- hogar[index,]
test <- hogar[-index,]

bosque <- randomForest(DEPARTAMENTO ~ PCH1 + PCH2 + PCH3 + PCH4 + PCH5 + PCH8 +PCH9_A +
                         PCH9_B+ PCH9_C+PCH9_D+PCH9_E+PCH9_F+PCH9_G+PCH9_H,
                       data = train,
                       ntree = 1000,
                       mtry = 10
                       )
entreno <- predict(bosque, test)

entreno

dato_nuevo <- data.frame(
  PCH1=3,
  PCH2=1,
  PCH3=1,
  PCH4=3,
  PCH5=2,
  PCH8=1,
  PCH9_A=1,
  PCH9_B=1,
  PCH9_C=1,
  PCH9_D=1,
  PCH9_E=1,
  PCH9_F=2,
  PCH9_G=2,
  PCH9_H=2
)

prediccion <- predict(bosque, dato_nuevo)
prediccion


hogar2 <- read.csv('C:/Users/kevin/OneDrive/Documentos/MIIC/Mineria/Censo2018/HOGAR_BDP.csv', sep=',')

hogar2 <- hogar2[, c("DEPARTAMENTO","MUNICIPIO","ZONA","PCH1", "PCH2","PCH3","PCH4","PCH5", "PCH8", "PCH9_A",
                   "PCH9_B","PCH9_C","PCH9_D","PCH9_E","PCH9_F","PCH9_G","PCH9_H")]
hogar2 <- subset(hogar2, hogar2$DEPARTAMENTO==1 & hogar2$MUNICIPIO==101)
hogar2 <- na.omit(hogar2)

hogar2 <- hogar2[, c("ZONA","PCH1", "PCH2","PCH3","PCH4","PCH5", "PCH8", "PCH9_A",
                     "PCH9_B","PCH9_C","PCH9_D","PCH9_E","PCH9_F","PCH9_G","PCH9_H")]


hogar2$ZONA <- as.factor(hogar2$ZONA)

set.seed(100)
hogar2 <- hogar2[sample(1:nrow(hogar2)),]

index2 <-sample(1:nrow(hogar2), 0.8*nrow(hogar))

train2 <- hogar2[index2,]
test2 <- hogar2[-index2,]

bosque2 <- randomForest(ZONA ~ PCH1 + PCH2 + PCH3 + PCH4 + PCH5 + PCH8 +PCH9_A +
                         PCH9_B+ PCH9_C+PCH9_D+PCH9_E+PCH9_F+PCH9_G+PCH9_H,
                       data = train2,
                       ntree = 100,
                       mtry = 8
)

prueba <- predict(bosque2, test2)
prueba
dato_nuevo2 <- data.frame(
  PCH1=1,
  PCH2=1,
  PCH3=1,
  PCH4=3,
  PCH5=1,
  PCH8=1,
  PCH9_A=1,
  PCH9_B=1,
  PCH9_C=1,
  PCH9_D=1,
  PCH9_E=1,
  PCH9_F=1,
  PCH9_G=1,
  PCH9_H=1
)

prediccion <- predict(bosque2, dato_nuevo2)
prediccion

