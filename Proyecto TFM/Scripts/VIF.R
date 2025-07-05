library(car)

predictoras <- pre_covid_data[, c("Sexo", "Edad - Decenios", "Nivel de renta", "Situación laboral",
                                  "Comunidad Autónoma", "Trastornos de ansiedad", "Trastornos de estrés",
                                  "Trastornos depresivos y estado del ánimo", "Trastornos psicóticos",
                                  "Trastorno de la personalidad", "Conducta suicida", "Trastorno del neurodesarrollo",
                                  "Trastornos neurocognitivos", "Trastornos del sueño", "Trastornos por consumo de sustancias")]

matriz_predictoras <- model.matrix(~ ., data = predictoras)[, -1]

data_glm <- data.frame(antidepresivos_bin = pre_covid_data$antidepresivos_bin, matriz_predictoras)

modelo_glm <- glm(antidepresivos_bin ~ ., data = data_glm, family = binomial())

vif_result <- vif(modelo_glm)

print(vif_result)