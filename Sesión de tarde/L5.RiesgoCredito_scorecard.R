# Descripción: Cálculo de una scorecard usando el paquete de R "scorecard"

# https://cran.r-project.org/web/packages/scorecard/vignettes/demo.html


rm(list=ls())

# Carga de librarías ----

#install.packages("mfx")
#install.packages("gmodels")
#install.packages("stargazer")
#install.packages("scorecard")
#install.packages("devtools")
#devtools::install_github("shichenxie/scorecard")


# Vamos a crear una tarjeta de puntuación utilizando regresión logística
library(stargazer) # esta librería es sólo para sacar los resultados de regresión en tablas
library(scorecard)
library(dplyr)

# Carga de Datos ----

# los datos están en la propia + load germancredit data
data("germancredit")



#+ descripción de las variables ----
# https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
#?germancredit


# El conjunto de datos tiene 1000 observaciones y 21 variables:  
  
#  - **creditability** Es la variable objetivo que califica a los 1000 clientes como de buenos o malos en función de su riesgo de crédito.



# Las 20 restantes variables (7 numéricas y 13 categóricas) son los atributos o características observadas de esos clientes que se utilizarán para predecir la probabilidad de que los clientes cometan un impago de sis créditos, esto es, de que sean malos clientes. La descripción de estas 20 variables es la siguiente:
  
#  - Attribute 1: (qualitative) **Status of existing checking account o cuenta corriente** 
#   - A11 : ... < 0 DM 
#   - A12 : 0 <= ... < 200 DM 
#   - A13 : ... >= 200 DM / salary assignments for at least 1 year 
#   - A14 : no checking account 

#- Attribute 2: (numerical) **Duration in month** 
  
#  - Attribute 3: (qualitative) **Credit history**  
#     - A30 : no credits taken/ all credits paid back duly (devultos sin mora)
#     - A31 : all credits at this bank paid back duly 
#     - A32 : existing credits paid back duly till now 
#     - A33 : delay in paying off in the past 
#     - A34 : critical account/ other credits existing (not at this bank) 

# - Attribute 4: (qualitative) **Purpose** 
#     - A40 : car (new) 
#     - A41 : car (used)
#     - A42 : furniture/equipment
#     - A43 : radio/television
#     - A44 : domestic appliances
#     - A45 : repairs
#     - A46 : education
#     - A47 : (vacation - does not exist?)
#     - A48 : retraining
#     - A49 : business
#    - A410 : others 

# - Attribute 5: (numerical) **Credit amount** 
  
# - Attribute 6: (qualitative) **Savings account/bonds** 
#    - A61 : ... < 100 DM
#    - A62 : 100 <= ... < 500 DM
#    - A63 : 500 <= ... < 1000 DM
#    - A64 : .. >= 1000 DM 
#    - A65 : unknown/ no savings account 

# - Attribute 7: (qualitative) **Present employment since** 
#     - A71 : unemployed 
#     - A72 : ... < 1 year 
#     - A73 : 1 <= ... < 4 years 
#     - A74 : 4 <= ... < 7 years 
#     - A75 : .. >= 7 years 

# - Attribute 8: (numerical) **Installment rate in percentage of disposable income** 
  
#  - Attribute 9: (qualitative) **Personal status and sex** 
#     - A91 : male : divorced/separated
#     - A92 : female : divorced/separated/married
#     - A93 : male : single
#     - A94 : male : married/widowed
#     - A95 : female : single 

# - Attribute 10: (qualitative) **Other debtors / guarantors**
#    - A101 : none
#    - A102 : co-applicant
#    - A103 : guarantor 

# - Attribute 11: (numerical) **Present residence since**
  
# - Attribute 12: (qualitative) **Property**
#     - A121 : real estate
#     - A122 : if not A121 : building society savings agreement/ life insurance
#     - A123 : if not A121/A122 : car or other, not in attribute 6 
#     - A124 : unknown / no property 

# - Attribute 13: (numerical) **Age in years** 
  
# - Attribute 14: (qualitative) **Other installment plans** Otros pagos por plazos
#      - A141 : bank
#      - A142 : stores
#      - A143 : none 

# - Attribute 15: (qualitative) **Housing**
#      - A151 : rent 
#      - A152 : own
#      - A153 : for free 

# - Attribute 16: (numerical) **Number of existing credits at this bank**
  
# - Attribute 17: (qualitative) **Job**
#   - A171 : unemployed/ unskilled - non-resident
#   - A172 : unskilled - resident
#   - A173 : skilled employee / official
#   - A174 : management/ self-employed/highly qualified employee/ officer 

# - Attribute 18: (numerical) **Number of people being liable to provide maintenance for** 
  
# - Attribute 19: (qualitative) **Telephone**
#   - A191 : none
#   - A192 : yes, registered under the customers name 

# - Attribute 20: (qualitative) **foreign worker** 
#   - A201 : yes 
#   - A202 : no 



# summary
summary(germancredit$creditability)

# Renombro la base de datos (no es necesario pero me sirve para trabajar con cualquier base de datos)

dt<-germancredit
str(dt)


# renombro creditability como y
dt$y<-as.numeric(dt$creditability)
# recodifico y= Default o Malo
dt$y<-ifelse(dt$y==2,0,1)
# elimino el campo creditability
dt<-dt[,-21]


# Habría que hacer un descriotivo inicial (que yo no hago aquí) 
# Creo la variable en Logs
dt$logamount<-log(dt$credit.amount)
#########OJO Borro Credit.amount
dt <- select(dt, -credit.amount)


# Conversión de factores a tipo texto ----
# Existe un problema en la forma en la que trata la librería las variables tipo factor...
# necesitamos convertirlas a variables tipo factor

dt <- dt %>% mutate_if(is.factor, as.character)


# selección inicial de Varaibles ----
#'  Selección inicial de variables
#'  Aquí se hacer una selección inicial
#'  por valores missing o por valores idénticos
#'  Hasta que no se agrupen y definan las variables WOE no debería
#'  filtrarse ningún otro criterio como IV o GINI

#+ ha creado una lista dt_s. En la que el dataframe dt_s$contiene sólo las variables no eliminadas
dt_s <- var_filter(dt,"y", iv_limit=0,return_rm_reason = TRUE)
# View(dt_s$rm)
# table(dt$foreign.worker)

#' dividimos la serie original entre entrenamiento (train) y validación (test), para trabajar por separado

dt_list <- split_df(dt_s$dt, y="y", ratio = 0.7, seed = 42) # 21

train <- dt_list$train 
test <- dt_list$test

prop.table(table(train$y))
prop.table(table(test$y))


# # woe binning con la muestra de entrenamiento (tramificación) ------
bins <- woebin(train, "y", print_step = 5)
woebin_plot(bins)

# recordad, un Iv menor que 0.02 la variable es muy débil

bins$purpose


# Variables woe ----
# Una vez que ya hemos determinado las tramificaciones 
# (con bins o bins_adj), calculo los WOE

train_woe <- woebin_ply(train, bins, print_step = 5)

# le aplico también la tramificacion bins calculada con la muestra
# de  entrenamiento a la muestra de validación
test_woe <- woebin_ply(test, bins, print_step = 5)


# Selección de variables -----
# Una vez que tenemos las variables transformadas en WOE se seleccionan
# las variables del modelo que tengan un iv>0.02

train_wf<- var_filter(train_woe,"y",return_rm_reason = TRUE)
#View(train_wf$rm)
train_woe<-train_wf$dt

# Para el conjunto test en realidad debería mantener las mismas que para l conjunto 
# train y quitar las que he quitado en train
train_kp<-train_wf$rm$variable[is.na(train_wf$rm$rm_reason)] 

train_rm<-train_wf$rm$variable[!is.na(train_wf$rm$rm_reason)]

test_wf  <- var_filter(test_woe,"y",var_rm=train_rm,
                        var_kp=train_kp,
                       return_rm_reason = TRUE)
# View(test_wf$rm)
test_woe<-test_wf$dt



#####################################
#' # ESTIMACION 
# Ahora con 'train_woe' realizaré el entrenamiento del modelo y
# con 'test_woe' la validación


#+ glm ------ Logistic regresion ----
m1 <- glm( y ~ ., family = binomial(link="logit"), data = train_woe)
summary(m1)

stargazer(m1,type = 'text')



#' # estimo una logística por pasos adelante y atrás para ver si se elimina alguna de las variables no significattivas

m_step <- step(glm( y ~ ., family = binomial(link="logit"), data = train_woe), direction="backward", trace = TRUE) # modelo hacia atrás
m2 <- eval(m_step$call)
summary(m2)


m_step <- step(glm( y ~ 1, family = binomial(link="logit"), data = train_woe),scope= formula(m1), direction="forward", trace = TRUE) # modelo hacia adelante
m3 <- eval(m_step$call)


m_step <- step(glm( y ~ ., family = binomial(link="logit"), data = train_woe), direction="both", trace = TRUE) # modelo Stepwise
m4 <- eval(m_step$call)


stargazer(m1,m2,m3,m4, type="text")





# Diagnosis ----
#' # Hago un apredicción de la probabilidad
train_pred <- predict(m2, type='response', train_woe) # type='response', Para predecir probabilidad
test_pred <- predict(m2, type='response', test_woe)

mean(train$y)
mean(train_pred)


#' # Diagnosis ks & roc plot
perf_eva(label=train$y, pred=train_pred,title = "train", confusion_matrix = TRUE, show_plot = c('ks','roc','f1', 'density'))


# OJO!!!! La validación siempre con la muestra test
perf_eva(label=test$y, pred=test_pred, title = "test", confusion_matrix = TRUE, show_plot = c('ks','roc','f1', 'density'))
mean(test_pred)

# Determino un Cut off o probabilidad de corte para realizar mis pronósticos
cuttoff<-mean(train$y) # o máximo F1
cuttoff
#cuttoff<-0.5

# creo los pronósticos con el cuttoff
train_predconf<-ifelse(train_pred<cuttoff,0,1)
test_predconf<-ifelse(test_pred<cuttoff,0,1)

#' # y la matriz de confusión con ese cut-off
conf_matrix<-table(test$y,test_predconf)
conf_matrix
accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
accuracy

#' #  Ahora calculo la puntuación o score a partir de las probabilidades estimadas

points0_0 <- 600
odds0_0 <- 1/50
pdo_0 <- 20

# Transformación lineal (véase las transparencias, básicamente es una recta srbitraria con pendiente negativa)
# score= Offset - Factor *ln(odds)
# Factor= (pdo_0/log(2))
# Offset = points0_0+(pdo_0/log(2))*log(odds0_0)


# Hay una función que estima directamente la tarjeta de puntuación
card <- scorecard(bins, m2, points0 = points0_0, odds0 = odds0_0, pdo = pdo_0)
card

card$logamount


#' ## la puntuación equivalente al Cut-off será
cuttoff_score<-points0_0+(pdo_0/log(2))*log(odds0_0) - (pdo_0/log(2)) *log((cuttoff)/(1-cuttoff))
cuttoff_score

train_scoreT <- scorecard_ply(train, card, print_step = 0, only_total_score = FALSE)
# para obtener la puntuaciónse suma los puntos al Offset-b0*Factor (ver apuntes)
# points0_0+(pdo_0/log(2))*log(odds0_0)-coefficients(m2)[1]*(pdo_0/log(2))

train_score <- scorecard_ply(train, card, print_step = 0)
test_score <- scorecard_ply(test, card, print_step = 0)





train$score<-train_score
train$prob<-train_pred
train_out<-cbind(train,train_scoreT)


View(data.frame(test_score,test_pred))


#' # psi (population stability index)
# Es una medida de diferencia en la distribución de dos muestras
# en nuestro caso entre la muestra test y entrenamiento (pero se aplica
# para ver cuando comienzan a verse diferencias con la predicciones de 
# nuestro modelo con la muestra -train- y los nuevos datos que vayan entrando,
# .... para detectar cuándo nustro modelo necesita una revisión)

perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train[,"y"], test = test[, "y"]),
  x_limits = c(250, 700),
  x_tick_break = 50
)

# PSI <que 0.1	No hay diferencias significativas entre las muestras de test y entrenamiento (resultado deseado, no se requiere más acciones)
# PSI entre 0.1 y  0.25	HAy cambio menores, valdría la pena revisar el modelo
# PSI mayor que 0.25 HAy cambios importantes entre las dos muestras HAY QUE CAMBIAR EL MODELO

