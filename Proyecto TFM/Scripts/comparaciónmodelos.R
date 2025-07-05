library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(speedglm)
library(broom)

base_modelo <- fread("base_modelo_actualizada.csv")
base_modelo[] <- lapply(base_modelo, function(x) if (is.character(x)) as.factor(x) else x)

pre_covid_data <- subset(base_modelo, COVID == "Pre-COVID")

pre_covid_data$`Edad - Decenios` <- as.numeric(pre_covid_data$`Edad - Decenios`)
pre_covid_data$Sexo <- factor(pre_covid_data$Sexo, levels = c("Varón", "Mujer"))
pre_covid_data$`Nivel de renta` <- factor(pre_covid_data$`Nivel de renta`,
                                          levels = c("18.000–99.999 €", "≥100.000 €", "<18.000 €", "Muy baja"))
pre_covid_data$`Situación laboral` <- factor(pre_covid_data$`Situación laboral`,
                                             levels = c("Activos", "Pensionistas", "Desempleados", "No activos"))
pre_covid_data$`Comunidad Autónoma` <- relevel(pre_covid_data$`Comunidad Autónoma`, ref = "Madrid")

outcome <- "antidepresivos_bin"
trastorno_tautologico <- "`Trastornos depresivos y estado del ánimo`"

todos_los_predictores <- c("Sexo", "`Edad - Decenios`", "`Nivel de renta`", "`Situación laboral`",
                           "`Comunidad Autónoma`", "`Trastornos de ansiedad`", "`Trastornos de estrés`",
                           "`Trastornos depresivos y estado del ánimo`", "`Trastornos psicóticos`",
                           "`Trastorno de la personalidad`", "`Conducta suicida`", "`Trastorno del neurodesarrollo`",
                           "`Trastornos neurocognitivos`", "`Trastornos del sueño`", "`Trastornos por consumo de sustancias`")

formula_con <- as.formula(paste(outcome, "~", paste(todos_los_predictores, collapse = " + ")))
modelo_con <- speedglm(formula_con, data = pre_covid_data, family = binomial())
names(modelo_con)


deviance_nula <- modelo_con$nulldev
deviance_modelo <- modelo_con$deviance
pseudoR2 <- 1 - (deviance_modelo / deviance_nula)
pseudoR2

deviance_nulas <- modelo_sin$nulldev
deviance_modelos <- modelo_sin$deviance
pseudoR2s <- 1 - (deviance_modelos / deviance_nulas)
pseudoR2s


predictores_sin_trastorno <- setdiff(todos_los_predictores, trastorno_tautologico)
formula_sin <- as.formula(paste(outcome, "~", paste(predictores_sin_trastorno, collapse = " + ")))
modelo_sin <- speedglm(formula_sin, data = pre_covid_data, family = binomial())

logLik_con <- logLik(glm(formula_con, data = pre_covid_data, family = binomial()))
logLik_sin <- logLik(glm(formula_sin, data = pre_covid_data, family = binomial()))
AIC_con <- -2 * as.numeric(logLik_con) + 2 * attr(logLik_con, "df")
AIC_sin <- -2 * as.numeric(logLik_sin) + 2 * attr(logLik_sin, "df")

cat("AIC con trastorno depresivo incluido: ", AIC_con, "\n")
cat("AIC sin trastorno depresivo:        ", AIC_sin, "\n")

resultados_modelos <- list(
  modelo_con_trastorno = modelo_con,
  modelo_sin_trastorno = modelo_sin,
  AICs = list(
    con = AIC_con,
    sin = AIC_sin
  )
)

resultados_modelos



or_tabla <- data.frame(
  Variable = c("Sexo (Mujer)", "Edad (decenios)", "Renta muy baja", "Desempleado",
               "Trastornos de ansiedad", "Trastorno personalidad", "Conducta suicida", "Trastorno depresivo"),
  OR_con = c(1.38, 1.05, 1.09, 1.10, 1.87, 1.93, 2.68, 5.52),
  OR_sin = c(1.52, 1.06, 1.08, 1.13, 1.66, 2.00, 3.21, NA)
)

library(tidyr)

or_largo <- pivot_longer(or_tabla, cols = c("OR_con", "OR_sin"),
                         names_to = "Modelo", values_to = "OR")
or_largo$Modelo <- factor(or_largo$Modelo,
                          levels = c("OR_con", "OR_sin"),
                          labels = c("Con trastorno", "Sin trastorno"))

ggplot(or_largo, aes(x = OR, y = Variable, color=Modelo)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  scale_x_log10() +  # escala logarítmica para mejor visualización
  labs(title = "Comparación de Odds Ratios",
       x = "RO", y = "Variables") +
  theme_minimal() +
  theme(legend.position = "bottom")



or_tabla <- data.frame(
  Variable = c(
    "Sexo (Mujer)", "Edad (decenios)", "Renta ≥100.000 €", "Renta <18.000 €", "Renta Muy baja",
    "Situación laboral: Pensionistas", "Situación laboral: Desempleados", "Situación laboral: No activos",
    "Comunidad Autónoma: Andalucía", "Comunidad Autónoma: Aragón", "Comunidad Autónoma: Asturias",
    "Comunidad Autónoma: Balears", "Comunidad Autónoma: C. Valenciana", "Comunidad Autónoma: Canarias",
    "Comunidad Autónoma: Cantabria", "Comunidad Autónoma: Castilla-La Mancha", "Comunidad Autónoma: Castilla y León",
    "Comunidad Autónoma: Cataluña", "Comunidad Autónoma: Extremadura", "Comunidad Autónoma: Galicia",
    "Comunidad Autónoma: La Rioja", "Comunidad Autónoma: Murcia", "Comunidad Autónoma: Navarra",
    "Comunidad Autónoma: País Vasco", "Trastornos de ansiedad", "Trastornos de estrés",
    "Trastornos depresivos y estado del ánimo", "Trastornos psicóticos", "Trastorno de la personalidad",
    "Conducta suicida", "Trastorno del neurodesarrollo", "Trastornos neurocognitivos",
    "Trastornos del sueño", "Trastornos por consumo de sustancias"
  ),
  OR_con = c(
    1.38, 1.05, 1.05, 1.03, 1.09,
    1.22, 1.10, 1.15,
    1.08, 1.05, 0.91,
    1.29, 0.44, 0.86,
    0.86, 2.03, 0.90,
    1.55, 1.51, 1.27,
    0.82, 0.92, 0.84,
    0.82, 1.87, 3.26,
    5.52, 1.54, 1.93,
    2.69, 0.70, 1.51,
    0.79, 1.12
  ),
  OR_sin = c(
    1.52, 1.06, 0.99, 1.03, 1.08,
    1.24, 1.13, 1.14,
    0.74, 1.07, 1.01,
    1.13, 0.47, 0.85,
    0.90, 1.38, 0.78,
    1.13, 1.17, 1.19,
    0.83, 1.02, 0.92,
    0.62, 1.66, 3.12,
    NA, 1.47, 2.00,
    3.21, 0.64, 1.60,
    0.80, 1.20
  )
)
