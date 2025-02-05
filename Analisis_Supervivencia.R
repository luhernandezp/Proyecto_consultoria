##--------------------------- Paquetes utilizados -----------------------------#
require(magrittr)
require(readxl)
require(tidyverse)
require(ggplot2)
require(vcd)
require(corrplot)
require(RColorBrewer)


#------------------------------------------------------------------------------#

##------------------------Lectura de la base de datos -------------------------#

Datos <- read_excel("BDD.Análisis_PD-L1_26-09-2024.xlsx",sheet = "BD_330")

#---------------------- Cambio del tipo numérico a factor ---------------------#

Datos$codigo <- as.factor(Datos$codigo)
Datos$estado_vital_5años <- as.factor(Datos$estado_vital_5años)
Datos$estado_vital_2 <- as.factor(Datos$estado_vital_2)
Datos$ciudad <- as.factor(Datos$ciudad)
Datos$edad_cat <- as.factor(Datos$edad_cat)
Datos$`edad-cat2` <- as.factor(Datos$`edad-cat2`)
Datos$estrato <- as.factor(Datos$estrato)
Datos$estrato_cat <- as.factor(Datos$estrato_cat)
Datos$educacion <- as.factor(Datos$educacion)
Datos$educacion_cat <- as.factor(Datos$educacion_cat)
Datos$afiliacion <- as.factor(Datos$afiliacion)
Datos$lateralidad_cat <- as.factor(Datos$lateralidad_cat)
Datos$tipo_histologico <- as.factor(Datos$tipo_histologico)
Datos$tipo_histol_cat <- as.factor(Datos$tipo_histol_cat)
Datos$grado_histologico <- as.factor(Datos$grado_histologico)
Datos$grado_nuclear <- as.factor(Datos$grado_nuclear)
Datos$`gh/gn` <- as.factor(Datos$`gh/gn`)
Datos$T <- as.factor(Datos$T)
Datos$N <- as.factor(Datos$N)
Datos$M <- as.factor(Datos$M)
Datos$estadio <- as.factor(Datos$estadio)
Datos$estadio_cat <- as.factor(Datos$estadio_cat)
Datos$estadio_cat3 <- as.factor(Datos$estadio_cat3)
Datos$`estadio-Early_late` <- as.factor(Datos$`estadio-Early_late`)
Datos$ER <- as.factor(Datos$ER)
Datos$PR <- as.factor(Datos$PR)
Datos$HER2 <- as.factor(Datos$HER2)
Datos$subtipo_molecular_definitivo <- as.factor(Datos$subtipo_molecular_definitivo)
Datos$EUR_cat <- as.factor(Datos$EUR_cat)
Datos$NAM_cat <- as.factor(Datos$NAM_cat)
Datos$AFR_cat <- as.factor(Datos$AFR_cat)
Datos$recaidas <- as.factor(Datos$recaidas)
Datos$`Año dx` <- as.factor(Datos$`Año dx`)
Datos$`Cuartil_fecha dx` <- as.factor(Datos$`Cuartil_fecha dx`)
Datos$`PD-L1` <- as.factor(Datos$`PD-L1`)
Datos$`Interacción_Reg/stage` <- as.factor(Datos$`Interacción_Reg/stage`)

Datos %>% str()

#------------------- Mirar correlaciones entre las variables ------------------#

# Función para calcular Cramer's V entre dos variables
cramerV <- function(var1, var2) {
  tabla <- table(var1, var2)
  assocstats(tabla)$cramer
}

Datos1 <- Datos %>% select_if(is.factor) %>% select(-c('PD-L1','codigo'))

# Función para crear una matriz de Cramer's V
cramerV_matrix <- function(data) {
  n <- ncol(data)
  mat <- matrix(NA, n, n)
  colnames(mat) <- rownames(mat) <- colnames(data)
  for (i in 1:n) {
    for (j in 1:n) {
      if(i==j){
        mat[i,j] <- 1
      }else if (is.nan(cramerV(data[[i]],data[[j]]))) {
        mat[i,j] <-- 0
      }else{
        mat[i, j] <- cramerV(data[[i]], data[[j]])
      }
    }
  }
  return(mat)
}

corrplot(cramerV_matrix(Datos1),method = "circle",type = "upper")

#----Análisis descriptivos de las variables mas relevantes con la respuesta----#

variables <- c('estado_vital_5años','estado_vital_2','ciudad','estrato','T','afiliacion',
               'tipo_histologico','T','N','M','estadio','estadio_cat','estadio_cat3',
               'estadio-Early_late','ER','PR','subtipo_molecular_definitivo','recaidas',
               'Interacción_Reg/stage')

##---------------------- variables individuales ------------------------------##


# Bucle para crear un gráfico de barras por variable
for (var in variables) {
  Datos1[[var]] %>% table() %>% barplot(xlab = 'Categorias',main = paste("Grafico de barras de",var),
                                        col = brewer.pal(n = nlevels(Datos1[[var]]), name = "Blues"))
}


##----------------- Análisis descriptivo de vvaraibles en conjunto------------##
for (i in 1:length(variables)-1){
  plot(Datos1[[i+1]],Datos1$estado_vital_5años,xlab=variables[i+1],ylab='estado_vital_5años',
       col=c("#99cc99", "#cc9999", "#9999cc"),main= paste("Mosaico variable estado_vital_5_años y ",
                                                          variables[i+1]))
 }

