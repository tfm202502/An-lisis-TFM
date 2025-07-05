library(data.table)
library(dplyr)
library(tidyr)
library(future)
library(ggplot2)
library(biglm)
library(speedglm)
plan(multisession, workers = parallel::detectCores() - 1)

base_modelo <- fread("base_modelo_actualizada.csv")
base_modelo[] <- lapply(base_modelo, function(x) if (is.character(x)) as.factor(x) else x)

table(subset(base_modelo, COVID == 1)$antidepresivos_bin)
table(base_modelo$COVID, useNA = "ifany")
table(base_modelo$antidepresivos_bin, base_modelo$COVID, useNA = "ifany")


pre_covid_data <- subset(base_modelo, COVID == "Pre-COVID")
pre_covid_data$`Edad - Decenios` <- as.numeric(pre_covid_data$`Edad - Decenios`)
pre_covid_data$Sexo <- factor(pre_covid_data$Sexo, levels = c("Varón", "Mujer"))
pre_covid_data$`Nivel de renta` <- factor(
  pre_covid_data$`Nivel de renta`,
  levels = c("18.000–99.999 €", "≥100.000 €", "<18.000 €", "Muy baja")
)
pre_covid_data$`Situación laboral` <- factor(
  pre_covid_data$`Situación laboral`,
  levels = c("Activos", "Pensionistas", "Desempleados", "No activos")
)

pre_covid_data$COVID <- factor(
  pre_covid_data$COVID,
  levels = c("Pre-COVID", "Post-COVID")
)
pre_covid_data$`Comunidad Autónoma` <- relevel(
  pre_covid_data$`Comunidad Autónoma`,
  ref = "Madrid"
)


#bucle:

library(speedglm)
psicofarmacos <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin", 
                   "antipsicoticos_bin", "estimulantes_bin", "adicciones_bin")

trastornos_redundantes <- list(
  antidepresivos_bin = "`Trastornos depresivos y estado del ánimo`",
  ansioliticos_bin = "`Trastornos de ansiedad`",
  hipnoticos_bin = "`Trastornos del sueño`",
  antipsicoticos_bin = "`Trastornos psicóticos`",
  estimulantes_bin = "`Trastorno del neurodesarrollo`",
  adicciones_bin = "`Trastornos por consumo de sustancias`"
)

predictores <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`",
                 "`Comunidad Autónoma`", "`Trastornos de ansiedad`", "`Trastornos de estrés`",
                 "`Trastornos depresivos y estado del ánimo`", "`Trastornos psicóticos`",
                 "`Trastorno de la personalidad`", "`Conducta suicida`", "`Trastorno del neurodesarrollo`",
                 "`Trastornos neurocognitivos`", 
                 "`Trastornos del sueño`", "`Trastornos por consumo de sustancias`")

resultados_modelos <- list()

for (var in psicofarmacos) {
  
  predictores_filtrados <- setdiff(predictores, trastornos_redundantes[[var]])
  
  formula_texto <- paste(var, "~", paste(predictores_filtrados, collapse = " + "))
  formula_completa <- as.formula(formula_texto)
  
  modelo <- speedglm(formula_completa, data = pre_covid_data, family = binomial(), fitted=TRUE)
  
  coeficientes <- summary(modelo)$coefficients
  odds_ratios <- data.frame(
    Variable = rownames(coeficientes),
    Coeficiente = coeficientes[, "Estimate"],
    Error_Std = coeficientes[, "Std. Error"],
    p_valor = coeficientes[, "Pr(>|z|)"]
  )
  odds_ratios$OR <- exp(odds_ratios$Coeficiente)
  odds_ratios$IC_inf <- exp(odds_ratios$Coeficiente - 1.96 * odds_ratios$Error_Std)
  odds_ratios$IC_sup <- exp(odds_ratios$Coeficiente + 1.96 * odds_ratios$Error_Std)
  
  resultados_modelos[[var]] <- odds_ratios
}


formatea_resultado <- function(OR, IC_inf, IC_sup, p_valor, sig_nivel=0.05) {
  if (is.na(p_valor) || p_valor >= sig_nivel) {
    return("ns")
  }
  paste0(
    format(round(OR, 2), decimal.mark = ","),
    " [",
    format(round(IC_inf, 2), decimal.mark = ","),
    " - ",
    format(round(IC_sup, 2), decimal.mark = ","),
    "]"
  )
}

tablas_resultados <- list()

for (modelo_nombre in names(resultados_modelos)) {
  df <- resultados_modelos[[modelo_nombre]]
  
  df <- df %>%
    mutate(Resultado = mapply(formatea_resultado, OR, IC_inf, IC_sup, p_valor)) %>%
    dplyr::select(Variable, Resultado)
  
  tablas_resultados[[modelo_nombre]] <- df
}


install.packages("openxlsx")
library(openxlsx)

wb <- createWorkbook()

for (modelo_nombre in names(tablas_resultados)) {
  addWorksheet(wb, modelo_nombre)  # Crear hoja con el nombre del modelo
  writeData(wb, sheet = modelo_nombre, tablas_resultados[[modelo_nombre]])  # Escribir tabla
}

ruta_archivo <- "resultados_modelos_psicofarmacos.xlsx"
saveWorkbook(wb, file = ruta_archivo, overwrite = TRUE)

cat("Archivo Excel creado:", ruta_archivo, "\n")

resultados_modelos$adicciones_bin <- NULL


library(speedglm)

psicofarmacos <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin", 
                   "antipsicoticos_bin", "estimulantes_bin", "adicciones_bin")

trastornos_redundantes <- list(
  antidepresivos_bin = "`Trastornos depresivos y estado del ánimo`",
  ansioliticos_bin = "`Trastornos de ansiedad`",
  hipnoticos_bin = "`Trastornos del sueño`",
  antipsicoticos_bin = "`Trastornos psicóticos`",
  estimulantes_bin = "`Trastorno del neurodesarrollo`",
  adicciones_bin = "`Trastornos por consumo de sustancias`"
)

predictores <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`",
                 "`Comunidad Autónoma`", "`Trastornos de ansiedad`", "`Trastornos de estrés`",
                 "`Trastornos depresivos y estado del ánimo`", "`Trastornos psicóticos`",
                 "`Trastorno de la personalidad`", "`Conducta suicida`", "`Trastorno del neurodesarrollo`",
                 "`Trastornos neurocognitivos`", "`Trastornos del sueño`", "`Trastornos por consumo de sustancias`")

resultados_modelos <- list()
matrices_confusion <- list()

for (var in psicofarmacos) {
  
  predictores_filtrados <- setdiff(predictores, trastornos_redundantes[[var]])
  formula_texto <- paste(var, "~", paste(predictores_filtrados, collapse = " + "))
  formula_completa <- as.formula(formula_texto)
  
  modelo <- speedglm(formula_completa, data = pre_covid_data, family = binomial(), fitted = TRUE)
  
  coeficientes <- summary(modelo)$coefficients
  odds_ratios <- data.frame(
    Variable = rownames(coeficientes),
    Coeficiente = coeficientes[, "Estimate"],
    Error_Std = coeficientes[, "Std. Error"],
    p_valor = coeficientes[, "Pr(>|z|)"]
  )
  odds_ratios$OR <- exp(odds_ratios$Coeficiente)
  odds_ratios$IC_inf <- exp(odds_ratios$Coeficiente - 1.96 * odds_ratios$Error_Std)
  odds_ratios$IC_sup <- exp(odds_ratios$Coeficiente + 1.96 * odds_ratios$Error_Std)
  
  resultados_modelos[[var]] <- odds_ratios
  
  prob_pred <- predict(modelo, newdata = pre_covid_data, type = "response")
  pred_bin <- ifelse(prob_pred >= 0.5, 1, 0)
  real <- pre_covid_data[[var]]
  
  matriz <- table(Predicho = pred_bin, Real = real)
  matrices_confusion[[var]] <- matriz
}


calcula_sens_espec <- function(matriz) {
  TP <- matriz["1", "1"]
  TN <- matriz["0", "0"]
  FP <- matriz["1", "0"]
  FN <- matriz["0", "1"]
  sensibilidad <- TP / (TP + FN)
  especificidad <- TN / (TN + FP)
  return(c(sensibilidad = sensibilidad, especificidad = especificidad))
}

sens_espec_resultados <- lapply(matrices_confusion, calcula_sens_espec)
print(sens_espec_resultados)


library(speedglm)
library(pROC)

psicofarmacos <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin", 
                   "antipsicoticos_bin", "estimulantes_bin")  # sin adicciones

trastornos_redundantes <- list(
  antidepresivos_bin = "`Trastornos depresivos y estado del ánimo`",
  ansioliticos_bin = "`Trastornos de ansiedad`",
  hipnoticos_bin = "`Trastornos del sueño`",
  antipsicoticos_bin = "`Trastornos psicóticos`",
  estimulantes_bin = "`Trastorno del neurodesarrollo`"
)

predictores <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`",
                 "`Comunidad Autónoma`", "`Trastornos de ansiedad`", "`Trastornos de estrés`",
                 "`Trastornos depresivos y estado del ánimo`", "`Trastornos psicóticos`",
                 "`Trastorno de la personalidad`", "`Conducta suicida`", "`Trastorno del neurodesarrollo`",
                 "`Trastornos neurocognitivos`", "`Trastornos del sueño`", "`Trastornos por consumo de sustancias`")

curvas_roc <- list()
umbral_youden <- list()
sens_espec_resultados <- list()

for (var in psicofarmacos) {
  predictores_filtrados <- setdiff(predictores, trastornos_redundantes[[var]])
  formula_texto <- paste(var, "~", paste(predictores_filtrados, collapse = " + "))
  formula_modelo <- as.formula(formula_texto)
  
  modelo <- speedglm(formula_modelo, data = pre_covid_data, family = binomial(), fitted = TRUE)
  
  predicho <- predict(modelo, type = "response")
  real <- pre_covid_data[[var]]
  
  roc_obj <- roc(real, predicho, quiet = TRUE)
  curvas_roc[[var]] <- roc_obj
  
  coords_roc <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  umbral_youden[[var]] <- coords_roc
  
  sens_espec_resultados[[var]] <- c(
    sensibilidad = coords_roc["sensitivity"],
    especificidad = coords_roc["specificity"]
  )
}

print(sens_espec_resultados)

auc_resultados <- sapply(curvas_roc, auc)
print(auc_resultados)

library(pROC)
colors <- c(
  "#1b9e77",  # verde sobrio
  "#7570b3",  # azul-grisáceo
  "#d95f02",  # naranja suave
  "#666666",  # gris medio
  "#e7298a"   # fucsia tenue
)
modelos <- names(curvas_roc)

plot(curvas_roc[[1]], col = colors[1], lwd = 2,
     main = "Curvas ROC - Modelos Pre-COVID", legacy.axes = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

for (i in 2:length(curvas_roc)) {
  plot(curvas_roc[[i]], col = colors[i], lwd = 2, add = TRUE)
}

legend("bottomright", legend = c("Antidepresivos", "Ansiolíticos", "Hipnóticos", "Antipsicóticos", "Psicoestimulantes"), col = colors, lwd = 2)




library(speedglm)
library(ResourceSelection)  
library(pscl)               
library(DescTools)         
psicofarmacos_sin_adicciones <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin",
                                  "antipsicoticos_bin", "estimulantes_bin")

resultados_diagnostico <- list()

for (var in psicofarmacos_sin_adicciones) {
  predictores_filtrados <- setdiff(predictores, trastornos_redundantes[[var]])
  formula_texto <- paste(var, "~", paste(predictores_filtrados, collapse = " + "))
  formula_modelo <- as.formula(formula_texto)
  
  modelo <- speedglm(formula_modelo, data = pre_covid_data, family = binomial(), fitted = TRUE)
  
  real <- pre_covid_data[[var]]
  predicho <- predict(modelo, type = "response")
  
  glm_model <- glm(formula_modelo, data = pre_covid_data, family = binomial())
  r2_mcfadden <- as.numeric(pR2(glm_model)["McFadden"])
  
  brier <- BrierScore(real, predicho)
  
  hl_test <- hoslem.test(real, predicho, g = 10)
  hl_p <- hl_test$p.value
  
  resultados_diagnostico[[var]] <- list(
    R2_McFadden = r2_mcfadden,
    Brier = brier,
    HL_p = hl_p
  )
}

for (nombre in names(resultados_diagnostico)) {
  cat("Modelo:", nombre, "\n")
  print(resultados_diagnostico[[nombre]])
  cat("\n")
}

library(DescTools)

psicofarmacos <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin", 
                   "antipsicoticos_bin", "estimulantes_bin")

r2_nagelkerke_resultados <- list()

for (var in psicofarmacos) {
  predictores_filtrados <- setdiff(predictores, trastornos_redundantes[[var]])
  formula_texto <- paste(var, "~", paste(predictores_filtrados, collapse = " + "))
  formula <- as.formula(formula_texto)
  
  modelo_glm <- glm(formula, data = pre_covid_data, family = binomial())
  
  r2_nagelkerke_resultados[[var]] <- PseudoR2(modelo_glm, which = "Nagelkerke")
}

print(r2_nagelkerke_resultados)

