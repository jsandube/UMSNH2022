#=====================================================================#
#    Facultad de Economía "Vasco de Quiroga" de la UMSNH
#    Seminario Análisis de datos aplicados a la Economía
#    Morelia, México, 6 de octubre de 2022
#_____________________________________________________________________#
#    Topic: Principales modelos de Machine Learning
#_____________________________________________________________________#
#    1. Carga de librerias necesarias
#    2. Algoritmos ML de Regresión
#    3. Algoritmos ML de Clasificación
#=====================================================================#
## 1 Carga de librerias necesarias -----
#_____________________________________________________________________#
library(caTools) # Para hacer el split train/test
library(ggplot2) # Para realizar graficos
library(e1071) # Para ajustar un modelo Support Vector Regression, SVM, Kernel SVM y Naïve Bayes
library(rpart) # Para ajustar un modelo Árbol de Decisión para Regresión y Clasificación
library(randomForest) # Para ajustar un modelo Random Forest Regression y Clasificación
library(ElemStatLearn) # Para realizar graficos
library(class) # Para ajustar un modelo K-Nearest Neighbors

## 2 Algoritmos ML de Regresión -----
#_____________________________________________________________________#
#  2.1 Regresión Lineal Simple
#  2.2 Regresión Lineal Múltiple
#  2.3 Regresión Polinómica
#  2.4 Support Vector Regression
#  2.5 Árbol de Decisión para Regresión
#  2.6 Random Forest Regression

## ********************************************************************
## 2.1 Regresión Lineal Simple: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Salary_Data.csv')
#dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar el modelo de regresión lineal simple con el conjunto de entrenamiento
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# Predecir resultados con el conjunto de test
y_pred = predict(regressor, newdata = testing_set)

# Visualización de los resultados en el conjunto de entrenamiento
#install.packages("ggplot2")
ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Sueldo vs Años de Experiencia (Conjunto de Entrenamiento)") +
  xlab("Años de Experiencia") +
  ylab("Sueldo (en $)")

# Visualización de los resultados en el conjunto de testing
ggplot() + 
  geom_point(aes(x = testing_set$YearsExperience, y = testing_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Sueldo vs Años de Experiencia (Conjunto de Testing)") +
  xlab("Años de Experiencia") +
  ylab("Sueldo (en $)")

## ********************************************************************
## 2.2 Regresión Lineal Múltiple: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('50_Startups.csv')
#dataset = dataset[, 2:3]

# Codificar las variables categóricas
dataset$State = factor(dataset$State,
                       levels = c("New York", "California", "Florida"),
                       labels = c(1, 2, 3))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar el modelo de Regresión Lineal Múltiple con el Conjunto de Entrenamiento
regression = lm(formula = Profit ~ .,
                data = training_set)

# Predecir los resultados con el conjunto de testing
y_pred = predict(regression, newdata = testing_set)

# Construir un modelo óptimo con la Eliminación hacia atrás
SL = 0.05
regression = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                data = dataset)
summary(regression)

regression = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = dataset)
summary(regression)

regression = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)
summary(regression)

regression = lm(formula = Profit ~ R.D.Spend,
                data = dataset)
summary(regression)

## ********************************************************************
## 2.3 Regresión Polinómica: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)

# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar Modelo de Regresión Lineal con el Conjunto de Datos
lin_reg = lm(formula = Salary ~ ., 
             data = dataset)

# Ajustar Modelo de Regresión Polinómica con el Conjunto de Datos
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

# Visualización del modelo lineal
# install.packages("ggplot2")
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = "blue") +
  ggtitle("Predicción lineal del suedlo en función del nivel del empleado") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")


# Visualización del modelo polinómico
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(poly_reg, 
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),color = "blue") +
  ggtitle("Predicción polinómica del sueldo en función del nivel del empleado") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

# Predicción de nuevos resultados con Regresión Lineal
y_pred = predict(lin_reg, newdata = data.frame(Level = 6.5))

# Predicción de nuevos resultados con Regresión Polinómica
y_pred_poly = predict(poly_reg, newdata = data.frame(Level = 6.5,
                                                     Level2 = 6.5^2,
                                                     Level3 = 6.5^3,
                                                     Level4 = 6.5^4))

## ********************************************************************
## 2.4 Support Vector Regression (SVR): ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)

# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar SVR con el Conjunto de Datos
#install.packages("e1071")
regression = svm(formula = Salary ~ ., 
                 data = dataset, 
                 type = "eps-regression", 
                 kernel = "radial")

# Predicción de nuevos resultados con SVR 
y_pred = predict(regression, newdata = data.frame(Level = 6.5))

# Visualización del modelo de SVR
# install.packages("ggplot2")
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regression, 
                                               newdata = data.frame(Level = dataset$Level))),
            color = "blue") +
  ggtitle("Predicción (SVR)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

## ********************************************************************
## 2.5 Árbol de Decisión para Regresión: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)


# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])


# Ajustar Modelo de Regresión con el Conjunto de Datos
# install.packages("rpart")
regression = rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 1))

# Predicción de nuevos resultados con Árbol Regresión 
y_pred = predict(regression, newdata = data.frame(Level = 6.5))



# Visualización del modelo de árbol de regresión
# install.packages("ggplot2")
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(regression, 
                                        newdata = data.frame(Level = x_grid))),
            color = "blue") +
  ggtitle("Predicción con Árbol de Decisión (Modelo de Regresión)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

## ********************************************************************
## 2.6 Random Forest Regression: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# testing_set = subset(dataset, split == FALSE)


# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Ajustar Modelo de Random Forest con el Conjunto de Datos
# install.packages("randomForest")
set.seed(1234)
regression = randomForest(x = dataset[1],
                          y = dataset$Salary,
                          ntree = 500)

# Predicción de nuevos resultados con Regresión 
y_pred = predict(regression, newdata = data.frame(Level = 6.5))

# Visualización del modelo de Random Forest
# install.packages("ggplot2")
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(regression, 
                                        newdata = data.frame(Level = x_grid))),
            color = "blue") +
  ggtitle("Predicción (Random Forest)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

## 3 Algoritmos ML de Clasificación -----
#_____________________________________________________________________#
#  3.1 Regresión Logística
#  3.2 K-Nearest Neighbors
#  3.3 Support Vector Machine
#  3.4 Kernel Support Vector Machine
#  3.5 Naïve Bayes
#  3.6 Árbol de Decisión para Clasificación
#  3.7 Random Forest para Clasificación

## ********************************************************************
## 3.1 Regresión Logística: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el modelo de regresión logística con el conjunto de entrenamiento.
classifier = glm(formula = Purchased ~ .,
                 data = training_set, 
                 family = binomial)

# Predicción de los resultados con el conjunto de testing
prob_pred = predict(classifier, type = "response",
                    newdata = testing_set[,-3])

y_pred = ifelse(prob_pred> 0.5, 1, 0)

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Clasificación (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Clasificación (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## ********************************************************************
## 3.2 K-Nearest Neighbors (K-NN): ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el clasificador con el conjunto de entrenamiento 
# y hacer las predicciones con el conjunto de testing.
# install.packages("class")
y_pred = knn(train = training_set[,-3],
             test = testing_set[,-3],
             cl = training_set[,3],
             k = 5)

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
# install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[,-3],
             test = grid_set,
             cl = training_set[,3],
             k = 5)
plot(set[, -3],
     main = 'K-NN (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[,-3],
             test = grid_set,
             cl = training_set[,3],
             k = 5)
plot(set[, -3],
     main = 'K-NN (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## ********************************************************************
## 3.3 Support Vector Machine (SVM): ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el SVM con el conjunto de entrenamiento.
#install.packages("e1071")
classifier = svm(formula = Purchased ~ ., 
                 data = training_set,
                 type = "C-classification",
                 kernel = "linear")

# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## ********************************************************************
## 3.4 Kernel Support Vector Machine (Kernel SVM): ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el clasificador con el conjunto de entrenamiento.
#install.packages("e1071")
classifier = svm(formula = Purchased ~ .,
                 data = training_set, 
                 type = "C-classification",
                 kernel = "radial")

# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM Kernel (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM Kernel (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## ********************************************************************
## 3.5 Naïve Bayes: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Codificar la variable de clasificación como factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el clasificador con el conjunto de entrenamiento.
#install.packages("e1071")
classifier = naiveBayes(x = training_set[,-3], 
                        y = training_set$Purchased)

# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naïve Bayes (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naïve Bayes (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## ********************************************************************
## 3.6 Árbol de Decisión para Clasificación: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Codificar la variable de clasificación como factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Ajustar el clasificador con el conjunto de entrenamiento.
#install.packages("rpart")
classifier = rpart(formula = Purchased ~ ., 
                   data = training_set)

# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3],
                 type = "class")

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 250)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Árbol de Decisión (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 250)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Árbol de Clasificación (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Representación del árbol de clasificación
plot(classifier)
text(classifier)

## ********************************************************************
## 3.7 Random Forest para Clasificación: ajuste, optimización y validación del modelo
## ********************************************************************
# Importar el dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Codificar la variable de clasificación como factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

# Ajustar el Random Forest con el conjunto de entrenamiento.
#install.packages("randomForest")
classifier = randomForest(x = training_set[,-3],
                          y = training_set$Purchased,
                          ntree = 10)

# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])

# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

# Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Random Forest (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Random Forest (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
