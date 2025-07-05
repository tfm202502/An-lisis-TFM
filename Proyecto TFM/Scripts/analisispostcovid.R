install.packages("DescTools")
library(data.table)
library(dplyr)
library(openxlsx)
library(DescTools)
library(ResourceSelection)
library(pROC)
library (future)
plan(multisession, workers = parallel::detectCores() - 1)


base_modelo <- fread("base_modelo_actualizada.csv")
base_modelo[] <- lapply(base_modelo, function(x) if (is.character(x)) as.factor(x) else x)

post_covid_data <- subset(base_modelo, COVID == "Post-COVID")
post_covid_data$`Edad - Decenios` <- as.numeric(post_covid_data$`Edad - Decenios`)
post_covid_data$Sexo <- factor(post_covid_data$Sexo, levels = c("Varón", "Mujer"))
post_covid_data$`Nivel de renta` <- factor(post_covid_data$`Nivel de renta`,
                                           levels = c("18.000–99.999 €", "≥100.000 €", "<18.000 €", "Muy baja"))
post_covid_data$`Situación laboral` <- factor(post_covid_data$`Situación laboral`,
                                              levels = c("Activos", "Pensionistas", "Desempleados", "No activos"))
post_covid_data$`Comunidad Autónoma` <- relevel(post_covid_data$`Comunidad Autónoma`, ref = "Madrid")


var_outcome <- "estimulantes_bin"  # cambiar el nombre
trastorno_redundante <- "`Trastornos neurocognitivos`"  # cambiar el trastorno

predictores <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`",
                 "`Comunidad Autónoma`", "`Trastornos de ansiedad`", "`Trastornos de estrés`",
                 "`Trastornos depresivos y estado del ánimo`", "`Trastornos psicóticos`",
                 "`Trastorno de la personalidad`", "`Conducta suicida`", "`Trastorno del neurodesarrollo`",
                 "`Trastornos neurocognitivos`", "`Trastornos del sueño`", "`Trastornos por consumo de sustancias`")

predictores_filtrados <- setdiff(predictores, trastorno_redundante)

formula <- as.formula(paste(var_outcome, "~", paste(predictores_filtrados, collapse = " + ")))


modelo <- glm(formula, data = post_covid_data, family = binomial())


coef <- summary(modelo)$coefficients

df_or <- data.frame(
  Variable = rownames(coef),
  OR = exp(coef[, "Estimate"]),
  IC_inf = exp(coef[, "Estimate"] - 1.96 * coef[, "Std. Error"]),
  IC_sup = exp(coef[, "Estimate"] + 1.96 * coef[, "Std. Error"]),
  p_valor = coef[, "Pr(>|z|)"]
)

df_or <- df_or %>%
  mutate(`OR [IC95%]` = ifelse(is.na(p_valor) | p_valor >= 0.05, "ns",
                               paste0(round(OR, 2), " [", round(IC_inf, 2), " - ", round(IC_sup, 2), "]"))) %>%
  dplyr::select(Variable, `OR [IC95%]`, p_valor)

print(df_or)


predicho <- predict(modelo, type = "response")
real <- post_covid_data[[var_outcome]]



modelo_nulo <- glm(as.formula(paste(var_outcome, "~ 1")), data = post_covid_data, family = binomial())

ll_m <- as.numeric(logLik(modelo))
ll_0 <- as.numeric(logLik(modelo_nulo))
n <- nobs(modelo)

r2_nagelkerke <- (1 - exp((2 / n) * (ll_0 - ll_m))) / (1 - exp((2 / n) * ll_0))


hl_test <- tryCatch(hoslem.test(real, predicho, g = 10), error = function(e) NA)
hl_p <- ifelse(is.na(hl_test), NA, hl_test$p.value)

roc_obj <- roc(real, predicho, quiet = TRUE)
auc_value <- auc(roc_obj)

youden <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

df_metrica <- data.frame(
  Outcome = var_outcome,
  R2_Nagelkerke = round(r2_nagelkerke, 3),
  Hosmer_Lemeshow_p = round(hl_p, 3),
  AUC = round(auc_value, 3),
  Youden_threshold = round(youden["threshold"], 3),
  Sensibilidad = round(youden["sensitivity"], 3),
  Especificidad = round(youden["specificity"], 3)
)


print(df_metrica)

write.xlsx(df_or, file = paste0("OR_postcovid_", var_outcome, ".xlsx"))

write.xlsx(df_metrica, file = paste0("Metricas_postcovid_", var_outcome, ".xlsx"))

plot(roc_obj, col = "#1b9e77", lwd = 2, main = paste("Curva ROC -", var_outcome))
abline(a = 0, b = 1, col = "gray", lty = 2)
