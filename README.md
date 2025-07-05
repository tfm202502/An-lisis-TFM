# Análisis-TFM
Este repositorio contiene los scripts en R desarrollados para el proyecto del trabajo de Fin de Máster sobre la prescripción de psicofármacos, cuyo objetivo es:

*Analizar los tipos de psicofármacos prescritos y su relación con variables sociodemográficas y diagnósticos clínicos generales en distintos contextos temporales asociados a la pandemia de COVID-19.*

## Estructura del repositorio
scripts/ # Scripts en R utilizados en el análisis
data/ # [No se incluyen por motivos éticos]
README.md # Este archivo con la descripción del proyecto


## Requisitos

Este proyecto fue desarrollado con:
- **Lenguaje**: R  
- **Versión**: R 4.4.1 (2024-06-14 ucrt)

### Paquetes utilizados

```r
data.table
dplyr
tidyr
ggplot2
speedglm
broom
glmnet
car
pROC
future
openxlsx
DescTools
ResourceSelection

Para instalar los paquetes necesarios:
install.packages(c(
  "data.table", "dplyr", "tidyr", "ggplot2", "speedglm", "broom", "glmnet",
  "car", "pROC", "future", "openxlsx", "DescTools", "ResourceSelection"
))
```
Los scripts se encuentran en la carpeta scripts y se recomienda ejecutarlos en el siguiente orden:
parsimona.R
comparaciónmodelos.R
VIF.R
analisisprecovid.R
analisispostcovid.R
interacciones.R


## Notas
Los datos no se incluyen en este repositorio por motivos éticos y de confidencialidad.
Este repositorio es público y fue creado únicamente con fines de evaluación académica del proyecto.
