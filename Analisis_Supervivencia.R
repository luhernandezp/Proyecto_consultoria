##--------------------------- Paquetes utilizados -----------------------------#
require(magrittr)
require(readxl)
require(tidyverse)
require(ggplot2)
require(vcd)
require(corrplot)
require(RColorBrewer)
require(openxlsx)
library(reshape2)
require(plotly)

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
Datos$codigo <- as.factor(Datos$codigo)

Datos %>% str()
Datos <- data.frame(Datos)

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

##------------------------------ Grafito de Gantt ----------------------------##
################################----------------################################
# Con este gráfico revisamos que tan altos puede ser la probabilidad de supervivencia
# de las mujeres con cáncer de mama 


inicio <- rep(0,length(Datos$codigo))

# Crear el gráfico de Gantt
gantt_plot <- ggplot(Datos, aes(x = inicio, xend =tiempo_supervivencia_años , y = codigo, yend = codigo,color = as.factor(estado_vital_5años))) +
  geom_segment(linewidth = 0.5) +  # Líneas horizontales
  labs(title = "Gráfico de Gantt de Tiempo de Vida",
       x = "Tiempo (días)",
       y = "Individuo (ID)",
       grupo = "Estado") +
  theme_minimal()

# Mostrar el gráfico
print(gantt_plot)
ggplotly(gantt_plot, tooltip = "text")

Datos$estado_vital_5años %>% table()

set.seed(021)
db <- Datos[sample(1:nrow(Datos),50),]

db %>% mutate(
  ID=1:nrow(db),
  Base = 0, End = Base + tiempo_supervivencia_dias) %>% 
  ggplot(aes(x=Base,y = ID,yend=ID,xend=End))+
  geom_segment(linewidth = 0.8,aes(color=factor(estado_vital_5años)))+
  geom_point(aes(x=End,shape = factor(estado_vital_5años)),size=2,show.legend = FALSE)+
  geom_point(aes(x=End),shape = 16,size = 0.5)+
  scale_shape_manual(values = c("0","x"))+theme_classic()+
  scale_color_manual(values = c("skyblue3","red3"))+
  theme(legend.position = "bottom")+
  labs(x = "Tiempo de seguiemiento (dias)",shape="Status",color="Estatus",title = "Tiempo de supervivencia")+
  geom_vline(xintercept = 1825,linetype=2,linewidth=0.5)
 

Datos_modelo %>%  names() %>% length()

# Con esto podemos tener indicios de que las curvas de supervivencia tendrán altas 
# probabilidades de supervivencia ya que en nuestro conjunto de datos son mas las personas 
# no fallecidas y se evidencia que tiene  un tiempo de supervivencia en días extenso.



## ------------------------- Análisis de supervivencia------------------------##
##################################-----#########################################
##################################-----#########################################


library(survival)
library(survminer)
library(ISwR)
library(ggfortify)

# Revisenos las tablas y curvas de supervivencia para nuestros datos.

cancer_km <- survfit(Surv(tiempo_evento_bx_5años,
                          estado_vital_5años==1) ~ 1, 
                     data = Datos, 
                     type = "kaplan-meier"
                     )

cancer_km |> summary()

# gráfica de la curva de supervivencia 

#ggsurvplot(fit = cancer_km, data = Datos, 
#           conf.int = T, 
#           title = "Curva de Supervivencia", 
#           xlab = "Tiempo (dias)", 
#           ylab = "Probabilidad de supervivencia", 
#           legend.title = "Estimación", 
#           legend.labs = "Kaplan-Meier",
#           ggtheme = theme_minimal())


autoplot(cancer_km,type="fill",surv.alpha=0.9,
         surv.size=1,
         conf.int.alpha=0.2,
         censor.size=5,
         censor.colour="purple",col="red")+
  labs(x = "\n Survival Time (Años) ", y = "Survival Probabilities \n", 
       title = "Estimated Survival Curves Of \n Cancer Patients Using KM")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        color = "blue")+
  theme_bw()

## Como se dijo en el paso anterior la curva de supervivencia de los pacientes 
## tiene probabilidades de supervivencia altas a medida que transcurre el tiempo. 

#-----------------------------------------------------------------------------##

# Estimación de la media mediana y percentiles del tiempo  de supervivencia

print(cancer_km, print.rmean = TRUE)

#------------------------------------------------------------------------------#

# Comparación de las curvas de supervivencia entre grupos (Ciudad)
# Este análisis de curvas de supervivencia entre grupos de variables es interesante revisarlo
# ya que en nuestro conjunto de datos tenemos muchas variables que nos sirven para categorizar 
# por grupos es decir variables categóricas

cancer.km <- survival::survfit(Surv(tiempo_evento_bx_5años,
                          estado_vital_5años==1) ~ ciudad, 
                     data = Datos, 
                     type = "kaplan-meier")
cancer.km |> summary()

#ggsurvplot(fit = cancer.km, data = Datos, conf.int = T, 
#           title = "Curva de Supervivencia por Ciudad", 
#           xlab = "Tiempo (dias)", 
#           ylab = "Probabilidad de supervivencia",
#           palette = "ciudad",
#           legend.title = "Estimación",
#           legend.labs = c("Medellin", "Barranquilla", "Cali"),
#           ggtheme = theme_minimal())

autoplot(cancer.km,type="fill",surv.alpha=1.5,
         surv.size=1,
         conf.int.alpha=0.25,
         censor.size=5,
         censor.colour="purple")+
  labs(x = "\n Survival Time (Dias) ", y = "Survival Probabilities \n", 
       title = "Estimated Survival Curves Of \n Cancer Patients Using KM",
       fill = "Ciudad") +
  scale_fill_manual(values=c("pink","green","blue"),labels=c("Medellín","Barranquilla","Cali"))+
  guides(color="none")+
theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", siz = 12,colour = "green"))+
  theme_bw()

# Observado principalmente el comportamiento de las curvas de supervivencia por ciudad 
# Notamos que los pacientes de la ciudad de medellín presentan mayores 
# probabilidades de supervivencia que las otras ciudades, así que la ciudad puede influenciar
# en el análisis de supervivencia.


## ---------------------------------------------------------------------------##
## Busquemos alternativas de ajustar un modelo de supervivencia con todas nuestras variables
## para lograr identificar que variables son influyentes y así incluirlas dentro de nuestro
## modelo final.


# antes de ajustar el modelo es importante saber y tener en cuenta que nuestra base de datos cuenta }
# con categorías que son categorías de una misma variable es decir tenemos categorías que son de una misma
# variable pero con un menor numero de categorías, lo que debemos de hacer ahora es verificar que esas categorias 
# si den información importante al modelo para determinar que con  que variables debemos quedarnos  


## ---------------------------- Variable edad_cat ----------------------------##
##################################---------------###############################

cancer.km.edad.cat <- survfit(Surv(tiempo_evento_bx_5años,
                          estado_vital_5años==1) ~ edad_cat, 
                     data = Datos, 
                     type = "kaplan-meier")

cancer.km |> summary()



autoplot(cancer.km.edad.cat,type="fill",surv.alpha=1.5,
         surv.size=1,
         conf.int.alpha=0.25,
         conf.int = FALSE,
         censor.size=5,
         censor.colour="purple")+
  labs(x = "\n Survival Time (Dias) ", y = "Survival Probabilities \n", 
       title = "Estimated Survival Curves Of \n Cancer Patients Using KM",
       fill = "Ciudad") +
  scale_color_manual(values=c("pink","green","blue","red","orange"),labels=c("<40 años","40-49 años","50-59 años","≥60 años"))+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", siz = 12,colour = "green"))+
  theme_bw()

Datos$edad_cat %>% factor()


# Inicial tenemos estas graficas donde se ve las funciones de supervivencia de cada grupo, lo que ahremos a 
# continuacion es mirar la prueba del log-rank test para determinar si las curvas son estadisticamente 
# diferentes (globalmete) o si definitivamete no ha evidnecia de esto


# Prueba de hipotesiss 
# H_0: Las curvas son iguales estadisticamente vs H_1: Las curvas son diferentes 
survdiff(Surv(tiempo_evento_bx_5años,
              estado_vital_5años==1) ~ edad_cat, 
         data = Datos)

ggsurvplot(cancer.km.edad.cat, data = Datos,
           size = 1,                 # change line size
           palette = 
             c("#E7B800", "#2E9FDF","#4cee0b","#ee0b27"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("<40 años","40-49 años","50-59 años","≥60 años"),
           legend.title = "Grupos de Edad",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)
# El p valor de este gráfico pertenece a la prueba del lon rank test, ya que el p-valor de dicha prueba 
# fue de 0.71 aceptamos la hipótesis nula y decimos que estadística mente las curvas de supervivencia
# de los diferentes grupos son iguales, por tanto esta caracterización de la variable edaad no perimite 
# apreciar la significancia de cada edad 


##------------------------------ Variable edad_cat2 --------------------------##
################################-------------------#############################
cancer.km.edad.cat2 <- survfit(Surv(tiempo_evento_bx_5años,
                                   estado_vital_5años==1) ~ edad.cat2, 
                              data = Datos, 
                              type = "kaplan-meier")

cancer.km |> summary()



Datos$edad.cat2 %>% factor()


ggsurvplot(cancer.km.edad.cat2, data = Datos,
           size = 1,                 # change line size
           palette = 
             c("#E7B800", "#2E9FDF","#4cee0b"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("<50 años","50-59 años","≥60 años"),
           legend.title = "Grupos de Edad",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)


## ----------------------------- Variable ciudad -----------------------------##
#################################---------------################################


cancer.km.ciudad <- survfit(Surv(tiempo_evento_bx_5años,
                                    estado_vital_5años==1) ~ ciudad, 
                               data = Datos, 
                               type = "kaplan-meier")



ggsurvplot(cancer.km.ciudad, data = Datos,
           size = 1,                 # change line size
           palette = 
             c("#E7B800", "#2E9FDF","#4cee0b"),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Medellín","Barranquilla","Cali"),
           legend.title = "Grupos de Ciudades",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)

# Tenemos que si existe diferencia estadística entre los grupos de ciudades..(p-valor 0.00062)

Datos$ciudad %>% factor()


##---------------------------- Variable estrato -------------------------------##
###############################----------------#################################


cancer.km.estrato <- survfit(Surv(tiempo_evento_bx_5años,
                                 estado_vital_5años==1) ~ estrato, 
                            data = Datos, 
                            type = "kaplan-meier")


ggsurvplot(cancer.km.estrato, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("1","2","3","4","5","6"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)

##---------------------------- Variable estrato cat --------------------------##
###############################----------------#################################


cancer.km.estrato.cat <- survfit(Surv(tiempo_evento_bx_5años,
                                 estado_vital_5años==1) ~ estrato_cat, 
                            data = Datos, 
                            type = "kaplan-meier")


ggsurvplot(cancer.km.estrato.cat, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Estratos 5-6","Estrato 3-4","Estratos 1-2"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)


# las curvas de supervivencia para la variable estrato_cat si resultan ser estadisticamente diferentes
Datos$estrato_cat %>% factor()


##---------------------------- Variable educación --------------------------##
###############################----------------#################################

cancer.km.educacion <- survfit(Surv(tiempo_evento_bx_5años,
                                      estado_vital_5años==1) ~ educacion, 
                                 data = Datos, 
                                 type = "kaplan-meier")

ggsurvplot(cancer.km.educacion, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Ninguno","Primaria completa","Secundaria completa","Profesional completa",
                           "Postgrado completo"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)

Datos$educacion %>% factor()

##---------------------------- Variable educación_cat ------------------------##
###############################----------------#################################

cancer.km.educacion_cat <- survfit(Surv(tiempo_evento_bx_5años,
                                    estado_vital_5años==1) ~ educacion_cat, 
                               data = Datos, 
                               type = "kaplan-meier")

ggsurvplot(cancer.km.educacion_cat, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Ninguno o hasta básica primaria","Hasta básica secundaria","Educación superior"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)


##---------------------------- Variable tipo_histologico -----------------------##
###############################----------------#################################


cancer.km.tipo_histoloogico <- survfit(Surv(tiempo_evento_bx_5años,
                                        estado_vital_5años==1) ~ tipo_histologico, 
                                   data = Datos, 
                                   type = "kaplan-meier")

ggsurvplot(cancer.km.tipo_histoloogico, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Ductal","Ductal in situ","Ductal infiltrante","Indinferenciado","Infiltrante",
                           "Intraductal","Lobulillar in situ","Lobulillar infiltrante"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)

Datos$tipo_histologico %>% factor()

##---------------------------- Variable tipo_histologico_cat -----------------##
###############################----------------#################################


cancer.km.tipo_histoloogico_cat <- survfit(Surv(tiempo_evento_bx_5años,
                                            estado_vital_5años==1) ~ tipo_histol_cat, 
                                       data = Datos, 
                                       type = "kaplan-meier")

ggsurvplot(cancer.km.tipo_histoloogico_cat, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Ductal","Otros"),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)


##---------------------------- Variable estadio ------------------------------##
###############################----------------#################################

cancer.km.estadio <- survfit(Surv(tiempo_evento_bx_5años,
                                                estado_vital_5años==1) ~ estadio, 
                                           data = Datos, 
                                           type = "kaplan-meier")


ggsurvplot(cancer.km.estadio, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           #legend.labs = c("",""),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.6, 1)
)


# P-valor 0.0001


##---------------------------- Variable estadio_cat --------------------------##
###############################----------------#################################

cancer.km.estadio_cat <- survfit(Surv(tiempo_evento_bx_5años,
                                  estado_vital_5años==1) ~ estadio_cat, 
                             data = Datos, 
                             type = "kaplan-meier")


ggsurvplot(cancer.km.estadio_cat, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           #legend.labs = c("",""),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.4, 1)
)

# P-valor  0.00001


##---------------------------- Variable estadio_cat3 --------------------------##
###############################----------------#################################

cancer.km.estadio_cat3 <- survfit(Surv(tiempo_evento_bx_5años,
                                      estado_vital_5años==1) ~ estadio_cat3, 
                                 data = Datos, 
                                 type = "kaplan-meier")


ggsurvplot(cancer.km.estadio_cat3, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = FALSE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c("Estadíos 0-IA-IB"," Estadíos IIB-IIIA"," Estadíos IIIB- IIIC- IV"),
           legend.title = "Categorias",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.4, 1)
)

Datos$estadio_cat3 %>% factor()
# P-valor  0.00001

##---------------------------- Variable estadio-Early_late -------------------##
###############################----------------#################################

cancer.km.estadio_Early_late <- survfit(Surv(tiempo_evento_bx_5años,
                                       estado_vital_5años==1) ~ estadio.Early_late, 
                                  data = Datos, 
                                  type = "kaplan-meier")


ggsurvplot(cancer.km.estadio_Early_late, data = Datos,
           size = 1,                 # change line size# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           pval = TRUE, 
           pval.coord = c(1, 0.75),
           risk.table = TRUE,        # Add risk table
           risk.table.col = "strata",# Risk table color by groups
           #legend.labs = c("",""),
           legend.title = "Grupos de Estratos",# Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           surv.median.line = "hv", alpha=0.7,
           ylim = c(0.4, 1)
)



##------------------- Modelo de riesgos proporcionales de Cox ----------------##
######################################-----#####################################

# Para este momento ya sabemos cuales de las variables recategorizadas debemos 
# incluir en el modelo, ya que anteriormente identificamos la categorización que presenta 
# mejor importancia al modelo. dichas variables son las que se incluirán el este modelo
# Para tener en cuenta 
# para algunas variables se tienen en numéricas y categóricas como el caso de EUR,NAM,AFR
# por tanto probaremos un modelo usando las variables numéricas y luego ajustaremos uno usando 
# las variables categóricas para otro modelo, luego usaremos algunos criterios de selección 
# para quedarnos con un modelo final.
# preguntar por la variable PD-L1 para ser incluida en el modelo 
# # Para tener en cuenta: muchas variables que se quieren incluir en el modelo presentan un 
# gran numero de datos faltantes, lo que nos pude afectar en el análisis,
# casi la mitad de los valores de la varible son daos faltantes.


Datos_modelo <- Datos %>% dplyr::select(-c("codigo","tiempo_supervivencia_dias","tiempo_evento_bx_2",
                                           "edad_cat","edad.cat2","estrato","educacion",
                                           "tipo_histologico","grado_histologico",
                                           "estadio","estadio_cat","estadio.Early_late",
                                           "EUR_cat","NAM_cat","AFR_cat","recaidas","fecha_corte_seguimiento",
                                           "fecha_dx","Año.dx","Cuartil_fecha.dx","tiempo_supervivencia_años",
                                           "fecha_dx_paciente","fecha_bx","años_supervivencia_dx","años_supervivencia_bx",
                                           "tiempo_supervivencia_5_años_dx","PD.L1","Área.ocupada.por.los.TILs.estromales...total"))



##------------------------------------

# Estas son las variables que vamos a incluir en el modelo,Ahora con métodos de 
# selección de variables elegiremos las variables que son significativas para el modelo.
##----------------------- Selección hacia atrás ------------------------------##
#########################-----------------------################################

# Creamos el modelos surv

Datos_modelo %>% is.na() %>% table()
Datos_modelo <- na.omit(Datos_modelo)
Datos_modelo %>% dim()
Datos %>% dim

modelo.cox <- coxph(Surv(tiempo_evento_bx_5años,estado_vital_5años == 1) ~. ,
                       data = Datos_modelo)
summary(modelo.cox) 

# Selección hacia atrás usando el criterio AIC

modelo_cox_back <- step(modelo.cox, direction = "backward")

summary(modelo_cox_back)
AIC(modelo_cox_back)

# Selección hacia adelante
modelo_cox_forward <- step(coxph(Surv(tiempo_evento_bx_5años,estado_vital_5años == 1) ~ 1, data = Datos_modelo), 
                           direction = "forward", 
                           scope = formula(coxph(Surv(tiempo_evento_bx_5años,estado_vital_5años == 1) ~ ., data = Datos_modelo)))
summary(modelo_cox_forward)
AIC(modelo_cox_forward)


# Primeras observaciones: el modelo seleccionado por el método backware es un modelo
# mas variables significativas que el modelo por el método de forward ademas presenta un
# menor AIC por lo que en principio nos quedaremos con este modelo.


Datos%>% dim()
Datos_modelo %>% names()
