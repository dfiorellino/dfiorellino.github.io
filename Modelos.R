# REGRESIÓN LOGÍSTICA

# install.packages("caret")
library(caret)

set.seed(852);particion=createDataPartition(y=datos_regre$target,p=0.70,list=FALSE)
entreno=datos_regre[particion,]
testeo=datos_regre[-particion,]

modelo_logistico <- glm(target ~ . , data = entreno, family = binomial)


# Evaluación del modelo
prob <- predict(modelo_logistico, newdata = testeo,type="response") 
pred <- rep(0, length(prob))
pred[prob > 0.5] <- 1
pred <- as.factor(pred)


# Crear función de predicción
prediccion_disponible <- function(barrio, precio, fecha, min_noches, max_noches, huespedes, porcentajeR, puntaje, es_finde) {
  
  barrio_binario <- c(0, 0, 0, 0, 0)
  if (barrio == "Belgrano") {
    barrio_binario[1] <- 1
  } else if (barrio == "Recoleta") {
    barrio_binario[2] <- 1
  } else if (barrio == "Colegiales") {
    barrio_binario[3] <- 1
  } else if (barrio == "Nunez") {
    barrio_binario[4] <- 1
  } else if (barrio == "Retiro") {
    barrio_binario[5] <- 1
  } else {
    stop("Barrio inválido.")
  }
  
  # Crear un data frame con los valores de las variables predictoras
  datos_prediccion <- data.frame(
    Belgrano = barrio_binario[1],
    Recoleta = barrio_binario[2],
    Colegiales = barrio_binario[3],
    Nunez = barrio_binario[4],
    Retiro = barrio_binario[5],
    dolar_price = precio,
    date = as.Date(fecha),
    minimum_nights.y = min_noches,
    maximum_nights.y = max_noches,
    accommodates = huespedes,
    percent_booked = porcentajeR,
    review_scores_rating = puntaje,
    es_finde = es_finde)
  
  # Hacer la predicción utilizando el modelo ajustado
  prediccion <- predict(modelo_logistico, newdata = datos_prediccion, type = "response")
  
  # Devolver la predicción
  return(prediccion)
}




