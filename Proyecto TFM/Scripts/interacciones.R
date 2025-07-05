library(data.table)
library(dplyr)
library(speedglm)
library(openxlsx)
library(future)
plan(multisession, workers = parallel::detectCores() - 1)

base_modelo <- fread("base_modelo_actualizada.csv")
base_modelo[] <- lapply(base_modelo, function(x) if (is.character(x)) as.factor(x) else x)
base_modelo$`Edad - Decenios` <- as.numeric(base_modelo$`Edad - Decenios`)

base_modelo$Sexo <- factor(base_modelo$Sexo, levels = c("Varón", "Mujer"))

base_modelo$`Nivel de renta` <- factor(base_modelo$`Nivel de renta`,
                                       levels = c("18.000–99.999 €", "≥100.000 €", "<18.000 €", "Muy baja"))

base_modelo$`Situación laboral` <- factor(base_modelo$`Situación laboral`,
                                          levels = c("Activos", "Pensionistas", "Desempleados", "No activos"))

base_modelo$COVID <- factor(base_modelo$COVID, levels = c("Pre-COVID", "Post-COVID"))

base_modelo$`Comunidad Autónoma` <- factor(base_modelo$`Comunidad Autónoma`)
base_modelo$`Comunidad Autónoma` <- relevel(base_modelo$`Comunidad Autónoma`, ref = "Madrid")



vars_interaccion_sin_covid <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`")

interacciones <- paste(vars_interaccion_sin_covid, collapse = " + ")

formula_text <- paste(
  "estimulantes_bin ~ COVID * (", interacciones, ") + ",
  "`Comunidad Autónoma` + `Trastornos depresivos y estado del ánimo` + `Trastornos de estrés` + ",
  "`Trastornos de ansiedad` + `Trastorno de la personalidad` + `Conducta suicida` + ",
  "`Trastornos del sueño` + `Trastornos neurocognitivos` + ",
  "`Trastornos psicóticos` + `Trastornos por consumo de sustancias`"
)

formula_modelo <- as.formula(formula_text)

modelo <- glm(formula_modelo, data = base_modelo, family = binomial())

summary(modelo)



library(dplyr)
library(pROC)
library(ResourceSelection)
library(DescTools)
library(openxlsx)

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

odds_ratios <- odds_ratios %>%
  mutate(`OR con IC 95%` = sprintf("%.2f [%.2f - %.2f]", OR, IC_inf, IC_sup)) %>%
  dplyr::select(Variable, `OR con IC 95%`, p_valor)
wb_or <- createWorkbook()
addWorksheet(wb_or, "Odds Ratios")
writeData(wb_or, "Odds Ratios", odds_ratios)
saveWorkbook(wb_or, "odds_ratios.xlsx", overwrite = TRUE)


logLik_model <- logLik(modelo)
null_model <- glm(formula = update(formula(modelo), . ~ 1), data = modelo$data, family = binomial())
logLik_null <- logLik(null_model)

r2_mcfadden <- 1 - (as.numeric(logLik_model) / as.numeric(logLik_null))
r2_nagelkerke <- DescTools::PseudoR2(modelo, which = "Nagelkerke")

hl_test <- hoslem.test(modelo$y, fitted(modelo), g = 10)

roc_obj <- roc(modelo$y, fitted(modelo))
youden_index <- coords(roc_obj, x = "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))

sens <- youden_index["sensitivity"]
spec <- youden_index["specificity"]
cutoff <- youden_index["threshold"]
auc_val <- auc(roc_obj)
hl_p <- if (is.numeric(hl_test$p.value)) hl_test$p.value else hl_test$p.value[[1]]
list(
  r2_mcfadden = r2_mcfadden,
  r2_nagelkerke = r2_nagelkerke,
  hl_p = hl_test$p.value,
  auc_val = auc_val,
  sens = sens,
  spec = spec,
  cutoff = cutoff
)

  library(openxlsx)
  
  hl_p <- as.numeric(hl_test$p.value)
  auc_val <- as.numeric(auc_val)
  sens <- as.numeric(youden_index["sensitivity"])
  spec <- as.numeric(youden_index["specificity"])
  cutoff <- as.numeric(youden_index["threshold"])
  
  indicadores <- data.frame(
    Indicador = c(
      "R² McFadden",
      "R² Nagelkerke",
      "p Hosmer-Lemeshow",
      "AUC",
      "Sensibilidad",
      "Especificidad",
      "Corte óptimo (Youden)"
    ),
    Valor = round(c(
      r2_mcfadden,
      as.numeric(r2_nagelkerke),
      hl_p,
      auc_val,
      sens,
      spec,
      cutoff
    ), 3)
  )
  
  wb <- createWorkbook()
  addWorksheet(wb, "Indicadores")
  writeData(wb, "Indicadores", indicadores)
  
  saveWorkbook(wb, "indicadores_modelo.xlsx", overwrite = TRUE)
  
