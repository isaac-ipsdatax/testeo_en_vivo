

# Revisaremos lo que hicimos en Excel y añadiremos muchísimas cosas más de machine learing.


# Proyecto Modelo Regresión

# Cargamos los datos
library(readxl)
Datos_Modelo <- read_excel("C:/IPS DATAX/2. Cursos/2.9 Curso Introducción a Ciencia de Datos/5. Proyecto 2/Datos_Modelo.xlsx", 
                           sheet = "Datos")


# Modelo 1 Salario ~ Experiencia
modelo_1 <- lm(Salario ~ Experiencia, data = Datos_Modelo)
summary(modelo_1)


# Modelo 2 Salario ~ Experiencia + Años_Educación
modelo_2 <- lm(Salario ~ Experiencia + Años_Educación, data = Datos_Modelo)
summary(modelo_2)


# Modelo 3 Salario ~ Experiencia + Años_Educación + Tamaño_Empresa
modelo_3 <- lm(Salario ~ Experiencia + Años_Educación + Tamaño_Empresa, data = Datos_Modelo)
summary(modelo_3)





# Modelo real de machine learning
# Es mucho más que solo correr un modelo de regresión 

# Necesitamos:
# - Separar los datos en entrenamiento y testeo
# - Entrenar varios modelos a la vez
# - Evaluar estos modelos con los datos de testeo
# - Concluir cuál es mejor
# - OJO, por defecto utilizaremos caret

# Instalamos los paquetes (si no están instalados)
install.packages("tidyverse")
install.packages("caret")

# Cargamos las librerías
library(tidyverse)
library(caret)

# Más adelante nos puede preguntar lo siguiente (indicamos 1):
# 1 package is needed and is not installed. (randomForest). Would you like to try to install it now?
#1: yes
#2: no


datos <- Datos_Modelo

# Convertimos variable categórica a factor
summary(datos)
datos$Tamaño_Empresa <- as.factor(datos$Tamaño_Empresa)

# Separamos los datos
set.seed(123)
trainIndex <- createDataPartition(datos$Salario, p = 0.8, list = FALSE)
trainData <- datos[trainIndex, ]
testData <- datos[-trainIndex, ]

# Entrenamos los modelos con método correspondiente
modelo_rl <- train(Salario ~ ., data = trainData, method = "lm")       # Regresión Lineal
modelo_dt <- train(Salario ~ ., data = trainData, method = "rpart")    # Árbol de decisión
modelo_rf <- train(Salario ~ ., data = trainData, method = "rf")       # Random Forest
modelo_knn <- train(Salario ~ ., data = trainData, method = "knn")     # K-Nearest Neighbors

# Guardamos todos los modelos entrenados en una lista
modelos <- list(
  "Regresión Lineal" = modelo_rl,
  "Árbol de Decisión" = modelo_dt,
  "Random Forest" = modelo_rf,
  "KNN" = modelo_knn
)

# Creamos un data.frame vacío para guardar los resultados
resultados <- data.frame(Modelo = character(), R2 = double(), MAE = double(), RMSE = double())

# Evaluamos los modelos ya entrenados con los datos de testeo
for (nombre in names(modelos)) {
  
  modelo <- modelos[[nombre]]  # extrae el modelo desde la lista
  
  pred <- predict(modelo, newdata = testData)
  r2 <- R2(pred, testData$Salario)
  mae <- MAE(pred, testData$Salario)
  rmse <- RMSE(pred, testData$Salario)
  
  resultados <- rbind(resultados, data.frame(Modelo = nombre, R2 = r2, MAE = mae, RMSE = rmse))
  
}

# Mostramos los resultados
print(resultados)

