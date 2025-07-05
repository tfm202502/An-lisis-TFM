library(data.table)
library(glmnet)

dt <- as.data.table(base_modelo)

psicofarmacos <- c("antidepresivos_bin", "ansioliticos_bin", "hipnoticos_bin", 
                   "antipsicoticos_bin", "estimulantes_bin", "adicciones_bin")

formula_base <- as.formula(paste("~ Sexo + `Edad - Decenios` + `Nivel de renta` + `Situación laboral` +",
                                 "`Comunidad Autónoma` + `Trastornos de ansiedad` + `Trastornos de estrés` +",
                                 "`Trastornos depresivos y estado del ánimo` + `Trastornos psicóticos` +",
                                 "`Trastorno de la personalidad` + `Conducta suicida` + `Trastorno del neurodesarrollo` +",
                                 "`Trastornos neurocognitivos` + `Trastorno de la conducta alimentaria` +",
                                 "`Trastornos sexuales` + `Trastornos del sueño` + `Trastornos por consumo de sustancias`"))

X <- model.matrix(formula_base, data = dt)[, -1]

variables_seleccionadas <- list()

for (var in psicofarmacos) {
  y <- dt[[var]]
  
  cv_fit <- cv.glmnet(X, y, alpha = 1, family = "binomial", nfolds = 5)
  
  coef_lasso <- coef(cv_fit, s = "lambda.1se")
  
  nz <- which(coef_lasso != 0)
  vars <- rownames(coef_lasso)[nz]
  vars <- vars[vars != "(Intercept)"]
  
  variables_seleccionadas[[var]] <- vars
  
  cat(sprintf("Modelo: %s - Variables seleccionadas: %d\n", var, length(vars)))
  cat("Variables:\n")
  cat(paste(vars, collapse = ", "), "\n\n")
}

variables_comunes <- Reduce(intersect, variables_seleccionadas)

frecuencia <- table(unlist(variables_seleccionadas))
variables_frecuentes <- names(frecuencia[frecuencia >= 4])

print(variables_comunes)

print(variables_frecuentes)