#1. Instalación y carga de librerías
install.packages("randomForest")
library(randomForest)

#2. Carga y limpieza inicial de los datos
migracion <- read.csv('D:/Censo2018/Censo2018/MIGRACION_BDP.csv', sep = ',') 
migracion <- na.omit(migracion)

#3. Selección de columnas relevantes
migracion <- migracion[, c("DEPARTAMENTO","AREA","PEI3","PEI4","PEI5")]

#4. Conversión de variables categóricas
migracion$DEPARTAMENTO <- as.factor(migracion$DEPARTAMENTO)
migracion$AREA <- as.factor(migracion$AREA)
migracion$PEI3 <- as.factor(migracion$PEI3)

#5. Aleatorización de los datos
set.seed(100)
migracion <- migracion[sample(1:nrow(migracion)),]

#6. División de los datos en entrenamiento y prueba
index <- sample(1:nrow(migracion), 0.8*nrow(migracion))
train <- migracion[index,]
test <- migracion[-index,]

#7. Entrenamiento del modelo Random Forest
bosque <- randomForest(DEPARTAMENTO ~ AREA + PEI3 + PEI4 + PEI5,
                       data = train,
                       ntree = 500,
                       mtry = 3)

#8. Predicción con el modelo
entreno <- predict(bosque, test)
entreno

#9. Predicción para un nuevo dato
dato_nuevo <- data.frame(
  AREA="2",
  PEI3="2",
  PEI4=16,
  PEI5=2012
)
dato_nuevo$AREA <- factor(dato_nuevo$AREA, levels = levels(train$AREA))
dato_nuevo$PEI3 <- factor(dato_nuevo$PEI3, levels = levels(train$PEI3))

str(dato_nuevo)
str(train)


prediccion <- predict(bosque, dato_nuevo)
prediccion




