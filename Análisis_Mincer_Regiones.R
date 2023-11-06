#############################################################################################################
################################################   PRELIMINARES   ###########################################
#############################################################################################################

####################################### Carga de librerías ##################################################

library(tidyverse)     # Manipulación y visualización de datos de manera eficiente
library(scales)        # Escalas para gráficos
library(ggthemes)      # Adicionales a ggplot
library(summarytools)  # Resumen de datos
library(readxl)        # Leer archivo de Excel
library(knitr)         # Paquete para crear tablas presentables
library(writexl)       # Paquete para exportar archivos de excel
library(openxlsx)      # Paquete para editar archivos de excel
library(WRS2)          # Estadísticas robustas
library(robustbase)    # Estadísticos robustos
library(MASS)          # Estadísticos robustos
library(Hmisc)         # Paquete con herramientas estadísticas adicionales
library(gmodels)       # Adicionales para tablas de contingencia 
library(kableExtra)    # Formatear cuadros
library(stargazer)     # Formater cuadros estadísticos y de regresión
library(cowplot)       # Personalizar gráficas
library(DataExplorer)  # AED con poco código
library(psych)
library(nortest)       # Pruebas de normalidad
library(stats)
library(viridis)       # Paleta de muchos colores 
library(corrplot)

# Evitar notación científica en la consola
options(scipen = 999)

# Ubicación del directorio de trabajo
setwd("C:/Users/Barutch/Desktop/Para Publicar 2023/Mincer_Género_Sector_Región/Datos")

# Se importa el archivo de Excel con la base de datos
ENOE_Mincer <- read_xlsx("ENOE_Mincer.xlsx", sheet = "2005") %>% 
  as.tibble()

# Muestra la base de datos
View(ENOE_Mincer)

#Construir variables de experiencia y logaritmo del ingreso
ENOE_Mincer <- ENOE_Mincer %>% 
  filter(anios_esc != 99) %>% 
  mutate(exp = eda - anios_esc - 6, exp_2 = exp^2)

## Se revisan el tipo de datos #####
class(ENOE_Mincer)

## Se conocen el número de filas y columnas #####
dim(ENOE_Mincer)

#Se conocen el nombre de las variables ######
names(ENOE_Mincer)

#Se conocen el tipo de variables #######
str(ENOE_Mincer)

# Contar los valores NA en columnas específicas
ENOE_Mincer%>% 
  summarise_all(funs(sum(is.na(.))))

#Elimnar los datos perdidos
ENOE_Mincer<-na.omit(ENOE_Mincer)

######################################## REGIONALIZAR LA BASE DE DATOS #######################################
##############################################################################################################

#Se crea una columna con los nombres de las entidades
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(entidades = factor(ent,
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila",
                                       "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                                       "Hidalgo", "Jalisco", "Estado de México", "Michoacán", "Morelos", "Nayarit", "Nuevo León",
                                       "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                                       "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))
  )

#Se contabilizan los observaciones con los nombres de las entidades y se elabora la tabla de frecuencia

Frecuencia_Entidades<-ENOE_Mincer %>% 
  count(" " = entidades) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta))
print(Frecuencia_Entidades)
kable(Frecuencia_Entidades) 

Frecuencia_Entidades %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Entidad Federativa" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

# Definir las variables dicotómicas regionales 
regiones <- c("reg_1", "reg_2","reg_3","reg_4","reg_5")

# Utiliza la función 'mutate' para crear variables dicotómicas para cada región, con base en la regionalización propuesta por Rey y Sastre-Gutierrez (2010)
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    reg_1 = ifelse(entidades %in% c("Campeche","Nayarit","Quintana Roo","San Luis Potosí","Tabasco","Veracruz","Yucatán","Zacatecas"), 1, 0),
    reg_2 = ifelse(entidades %in% c("Baja California","Baja California Sur","Chihuahua","Sinaloa","Sonora"), 1, 0),
    reg_3 = ifelse(entidades %in% c("Chiapas","Guerrero","Hidalgo","Michoacán","Oaxaca","Puebla","Tlaxcala"), 1, 0),
    reg_4 = ifelse(entidades %in% c("Aguascalientes","Coahuila","Colima","Durango","Jalisco","Nuevo León","Tamaulipas"), 1, 0),
    reg_5 = ifelse(entidades %in% c("Ciudad de México","Guanajuato","Estado de México", "Morelos","Querétaro"), 1, 0)
  )
# Utiliza la función 'mutate' para crear una variable categórica indicando la región de la entidad
ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    region = case_when(
      entidades %in% c("Campeche","Nayarit","Quintana Roo","San Luis Potosí","Tabasco","Veracruz","Yucatán","Zacatecas") ~ "Región 1",
      entidades %in% c("Baja California","Baja California Sur","Chihuahua","Sinaloa","Sonora") ~ "Región 2",
      entidades %in% c("Chiapas","Guerrero","Hidalgo","Michoacán","Oaxaca","Puebla","Tlaxcala") ~ "Región 3", 
      entidades %in% c("Aguascalientes","Coahuila","Colima","Durango","Jalisco","Nuevo León","Tamaulipas") ~ "Región 4",
      entidades %in% c("Ciudad de México","Guanajuato","Estado de México","Morelos","Querétaro") ~ "Región 5",
      TRUE ~ "Otra Región"
    )
  )

#Se contabilizan los observaciones con los nombres de las regiones y se elabora la tabla de frecuencia

Frecuencia_Regiones<-ENOE_Mincer %>% 
  count(" " = region) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Regiones)
kable(Frecuencia_Regiones) 

Frecuencia_Regiones %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Región" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

######################################### SE DEFINE LA VARIABLE DE GÉNERO #####################################
###############################################################################################################

# Definir la variable dicotómica de género 
genero <- c("genero")

# Utiliza la función 'mutate' para crear variables dicotómicas para el género y cualitativas con texto
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    gen = ifelse(sex=="1", 1, 0),
    genero = ifelse(sex=="1", "Hombre", "Mujer")
  )

#Se contabilizan los observaciones por género y se elabora la tabla de frecuencia
Frecuencia_Genero<-ENOE_Mincer %>% 
  count(" " = genero) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Genero)
kable(Frecuencia_Genero) 

Frecuencia_Genero %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Género" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

######################################### SE DEFINE LA VARIABLE DE NIVEL DE INSTRUCCIÓN #####################################
###############################################################################################################

#Se eliminan aquellas observaciones que no especificaron nivel de instrucción
ENOE_Mincer <- ENOE_Mincer %>% 
  filter(niv_ins != "5" & niv_ins !="0") 

# Definir las variables dicotómicas por nivel de instrucción
niv_instruc <- c("prim_trunc","prim","sec","sup_med")

# Utiliza la función 'mutate' para crear variables dicotómicas para cada nivel de instrucción
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    prim_trunc = ifelse(niv_ins=="1", 1, 0),
    prim = ifelse(niv_ins=="2", 1, 0),
    sec = ifelse(niv_ins=="3", 1, 0),
    sup_med = ifelse(niv_ins=="4", 1, 0)
  )

# Utiliza la función 'mutate' para crear una variable categórica indicando el nivel de instrucción del individuo

ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    niv_instruc = case_when(
      ENOE_Mincer$niv_ins %in% c("1") ~ "Primaria Incompleta",
      ENOE_Mincer$niv_ins %in% c("2") ~ "Primaria",
      ENOE_Mincer$niv_ins %in% c("3") ~ "Secundaria", 
      ENOE_Mincer$niv_ins %in% c("4") ~ "Media Superior y Sup.",
      TRUE ~ "Otra respuesta"
    )
  )

#Se contabilizan los observaciones por nivel de instrucción y se elabora la tabla de frecuencia
Frecuencia_Niv_Instruc<-ENOE_Mincer %>% 
  count(" " = niv_instruc) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Niv_Instruc)
kable(Frecuencia_Niv_Instruc) 

Frecuencia_Niv_Instruc %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Nivel de Instrucción" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

###################################### SE DEFINE LA VARIABLE DE CONDICIÓN LABORAL #####################################
###############################################################################################################

# Se eliminan aquellas observaciones que no especificaron condición laboral
ENOE_Mincer <- ENOE_Mincer %>%
  filter(pos_ocu != "5", pos_ocu != "0")

# Definir la variable dicotómica de condición laboral 
cond_lab <- c("subord","emplead","emprend","no_remun")

# Utiliza la función 'mutate' para crear variables dicotómicas para la condición laboral
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    subord = ifelse(pos_ocu=="1", 1, 0),
    emplead = ifelse(pos_ocu=="2", 1, 0),
    emprend = ifelse(pos_ocu=="3", 1, 0),
    no_remun = ifelse(pos_ocu=="4", 1, 0)
  )

# Utiliza la función 'mutate' para crear una variable categórica indicando la condición laboral del individuo

ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    cond_lab = case_when(
      ENOE_Mincer$pos_ocu %in% c("1") ~ "Subordinado y Remunerado",
      ENOE_Mincer$pos_ocu %in% c("2") ~ "Empleador",
      ENOE_Mincer$pos_ocu %in% c("3") ~ "Emprendedor", 
      ENOE_Mincer$pos_ocu %in% c("4") ~ "Trabajador no remunerado",
      TRUE ~ "Otra respuesta"
    )
  )

#Se contabilizan los observaciones por condición laboral y se elabora la tabla de frecuencia
Frecuencia_Cond_Lab<-ENOE_Mincer %>% 
  count(" " = cond_lab) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Cond_Lab)
kable(Frecuencia_Cond_Lab) 

Frecuencia_Cond_Lab %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Condición Laboral" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

###################################### SE DEFINE LA VARIABLE DE OCUPACIÓN #####################################
############################################################################################################

#Se eliminan aquellas observaciones que no especificaron tipo de ocupacion

ENOE_Mincer <- ENOE_Mincer %>% 
  filter(c_ocu11c != "11" & c_ocu11c != "0")

# Definir la variable dicotómica por cada ocupación

cond_ocup <- c("arte","educ","directivo","oficinista","manufact","comerc","transport","serv_personal","protec_vigil","agropec")

# Utiliza la función 'mutate' para crear variables dicotómicas para cada tipo de ocupación
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    arte = ifelse(c_ocu11c=="1", 1, 0),
    educ = ifelse(c_ocu11c=="2", 1, 0),
    directivo = ifelse(c_ocu11c=="3", 1, 0),
    oficinista= ifelse(c_ocu11c=="4", 1, 0),
    manufact = ifelse(c_ocu11c=="5", 1, 0),
    comerc = ifelse(c_ocu11c=="6", 1, 0),
    transport = ifelse(c_ocu11c=="7", 1, 0),
    serv_personal = ifelse(c_ocu11c=="8", 1, 0),
    protec_civil = ifelse(c_ocu11c=="9", 1, 0),
    agropec = ifelse(c_ocu11c=="10", 1, 0)
  )

# Utiliza la función 'mutate' para crear una variable categórica indicando la ocupación del individuo

ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    ocup = case_when(
      ENOE_Mincer$c_ocu11c %in% c("1") ~ "Profesionales, técnicos y trabajadores del arte",
      ENOE_Mincer$c_ocu11c %in% c("2") ~ "Trabajadores de la educación",
      ENOE_Mincer$c_ocu11c %in% c("3") ~ "Funcionarios y directivos", 
      ENOE_Mincer$c_ocu11c %in% c("4") ~ "Oficinistas",
      ENOE_Mincer$c_ocu11c %in% c("5") ~ "Trabajadores industriales, artesanos y ayudantes",
      ENOE_Mincer$c_ocu11c %in% c("6") ~ "Comerciantes",
      ENOE_Mincer$c_ocu11c %in% c("7") ~ "Operadores de Transporte", 
      ENOE_Mincer$c_ocu11c %in% c("8") ~ "Trabajadores en servicios personales",
      ENOE_Mincer$c_ocu11c %in% c("9") ~ "Trabajadores en protección y vigilancia",
      ENOE_Mincer$c_ocu11c %in% c("10") ~ "Trabajadores agropecuarios",
      TRUE ~ "Otra respuesta"
    )
  )

#Se contabilizan los observaciones por ocupación y se elabora la tabla de frecuencia
Frecuencia_Ocup<-ENOE_Mincer %>% 
  count(" " = ocup) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Ocup)
kable(Frecuencia_Ocup) 

Frecuencia_Ocup %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Ocupación" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

###################################### SE DEFINE LA VARIABLE DE SECTOR ECONÓMICO #####################################
############################################################################################################

#Se eliminan aquellas observaciones que no aplican en la categoría de sector económico
ENOE_Mincer <- ENOE_Mincer %>% 
  filter(rama_est2 != "0")

# Definir la variable dicotómica de sector económico 
sector <- c("primario","ind_extrac","manufact","construc","comerc","rest_aloj","transport","serv_prof_financ","serv_soc","serv_divrs","gobierno")

# Utiliza la función 'mutate' para crear variables dicotómicas para cada sector económico
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    primario = ifelse(rama_est2=="1", 1, 0),
    ind_extract = ifelse(rama_est2=="2", 1, 0),
    manufact = ifelse(rama_est2=="3", 1, 0),
    construc = ifelse(rama_est2=="4", 1, 0),
    comerc = ifelse(rama_est2=="5", 1, 0),
    rest_aloj = ifelse(rama_est2=="6", 1, 0),
    transport = ifelse(rama_est2=="7", 1, 0),
    serv_prof_financ = ifelse(rama_est2=="8", 1, 0),
    serv_soc = ifelse(rama_est2=="9", 1, 0),
    serv_divrs = ifelse(rama_est2=="10", 1, 0),
    gobierno = ifelse(rama_est2=="11", 1, 0)
  )

# Utiliza la función 'mutate' para crear una variable categórica indicando el sector económico del individuo

ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    sector = case_when(
      ENOE_Mincer$rama_est2 %in% c("1") ~ "Agricultura, ganadería, silvicultura, caza y pesca",
      ENOE_Mincer$rama_est2 %in% c("2") ~ "Industria extractiva y de la electricidad",
      ENOE_Mincer$rama_est2 %in% c("3") ~ "Industria manufacturera", 
      ENOE_Mincer$rama_est2 %in% c("4") ~ "Construcción",
      ENOE_Mincer$rama_est2 %in% c("5") ~ "Comercio",
      ENOE_Mincer$rama_est2 %in% c("6") ~ "Restaurantes y servicios de alojamiento",
      ENOE_Mincer$rama_est2 %in% c("7") ~ "Transporte, comunicaciones, correo y almacenamiento", 
      ENOE_Mincer$rama_est2 %in% c("8") ~ "Servicios profesionales, financieros y corporativos",
      ENOE_Mincer$rama_est2 %in% c("9") ~ "Servicios sociales",
      ENOE_Mincer$rama_est2 %in% c("10") ~ "Servicios diversos",
      ENOE_Mincer$rama_est2 %in% c("10") ~ "Gobierno y organismo internacionales",
      TRUE ~ "Otra respuesta"
    )
  )

#Se contabilizan los observaciones por sector económico y se elabora la tabla de frecuencia
Frecuencia_Sector<-ENOE_Mincer %>% 
  count(" " = sector) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Sector)
kable(Frecuencia_Sector) 

Frecuencia_Sector %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Sector Económico" = 1, "Frecuencia" = 2)) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "white", background = "gray")

#########################  SE DEFINE LA VARIABLE DE POBLACION OCUPADA POR NIVEL DE INGRESO #################
############################################################################################################

#Se eliminan aquellas observaciones que no aplican en la categoría de nivel de ingreso
ENOE_Mincer <- ENOE_Mincer %>%
  filter(ing7c != "0" & ing7c != "7")

# Definir la variable dicotómica de nivel de ingreso 
niv_ingreso <- c("un_sal","un_dos_sal","dos_tres_sal","tres_cinco_sal","cinco_mas_sal","sin_ingrs")


# Utiliza la función 'mutate' para crear variables dicotómicas para cada nivel de ingreso
ENOE_Mincer <- ENOE_Mincer %>%
  mutate(
    un_sal = ifelse(ing7c=="1", 1, 0),
    un_dos_sal = ifelse(ing7c=="2", 1, 0),
    dos_tres_sal = ifelse(ing7c=="3", 1, 0),
    tres_cinco_sal = ifelse(ing7c=="4", 1, 0),
    cinco_mas_sal = ifelse(ing7c=="5", 1, 0),
    sin_ingrs = ifelse(ing7c=="6", 1, 0)
  )

# Utiliza la función 'mutate' para crear una variable categórica indicando el nivel de ingreso del individuo

ENOE_Mincer <- ENOE_Mincer %>% 
  mutate(
    niv_ingreso = case_when(
      ENOE_Mincer$ing7c %in% c("1") ~ "Hasta un salario mínimo",
      ENOE_Mincer$ing7c %in% c("2") ~ "Más de 1 hasta 2 salarios mínimos",
      ENOE_Mincer$ing7c %in% c("3") ~ "Más de 2 hasta 3 salarios mínimos", 
      ENOE_Mincer$ing7c %in% c("4") ~ "Más de 3 hasta 5 salarios mínimos",
      ENOE_Mincer$ing7c %in% c("5") ~ "Más de 5 salarios mínimos",
      ENOE_Mincer$ing7c %in% c("6") ~ "No recibe ingresos",
      TRUE ~ "Otra respuesta"
    )
  )

#Se contabilizan los observaciones por nivel de ingreso y se elabora la tabla de frecuencia
Frecuencia_Niv_Ing<-ENOE_Mincer %>% 
  count(" " = niv_ingreso) %>%
  rename(Absoluta = n) %>% 
  mutate(Relativa = Absoluta / sum(Absoluta)) 
print(Frecuencia_Niv_Ing)
kable(Frecuencia_Niv_Ing) 

Frecuencia_Niv_Ing %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Nivel de Ingreso" = 1, "Frecuencia Absoluta" = 1, "Frecuencia Relativa" = 1)) %>%
  column_spec(1, bold = F) %>%
  row_spec(0, bold = TRUE, color = "gray", background = "gray")

#############################################################################################################
################################ ESTADÍSTICOS DESCRIPTIVOS DE LOS DATOS #####################################              #
#############################################################################################################

########################### Resumen descriptivo de los datos cuantitativos ##################################
str(ENOE_Mincer)
summary(ENOE_Mincer)

datos_cuantitativos<-ENOE_Mincer %>%
  select(eda, anios_esc, hrsocup, ingocup, ing_x_hrs, exp, exp_2)

as_tibble(datos_cuantitativos)
descriptivos <- describe(datos_cuantitativos)
print(descriptivos)
write.xlsx(descriptivos, file = "estadisticas_descriptivas.xlsx")

kable(descriptivos)

descriptivos %>%
  kable("html", escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Variable" = 2, "N" = 1, "Media" = 1, "Desviación Est." = 1, "Mediana" = 1, "Media Robusta" = 1,
                     "Desviación Media Absoluta (MAD)" = 1, "Mínimo" = 1, "Máximo" = 1,
                     "Rango" = 1, "Sesgo" = 1, "Curtosis" = 1, "SE" = 1 )) %>%
  column_spec(1, bold = F) %>%
  column_spec(2, width = "2cm") %>%
  row_spec(0, bold = TRUE, color = "gray", background = "gray")

########################### Matriz de Correlaciones ##################################
round(cor(datos_cuantitativos),2)
rcorr(as.matrix(datos_cuantitativos))
correlacion<-round(cor(datos_cuantitativos), 1)
corrplot(correlacion, method="number", type="upper")

# Crear histogramas de las variables cuantitativas y reporte general de estadísticas descriptivas
plot_histogram(datos_cuantitativos, title = "Histogramas de Variables Cuantitativas")
plot_density(datos_cuantitativos) 
plot_correlation(datos_cuantitativos)

create_report(datos_cuantitativos)

##################################################################################################################################
################################################# HISTOGRAMAS Y NORMALIDAD  ######################################################
##################################################################################################################################

################ Elaboración del histograma, prueba de normalidad y boxplot de la variable edad ##################################

# Crea el objeto ggplot y establece los datos y el mapeo estético
edad<-ENOE_Mincer %>% 
  select(eda)
plot_h1 <- ggplot(edad, aes(x = edad))

# Agrega el histograma y la función de densidad
plot_h1 <- plot_h1 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$eda), sd = sd(ENOE_Mincer$eda)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h1 <- plot_h1 +
  theme_solarized() +
  labs(x = "Edad", y = "Densidad", title= "EDAD") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_edad.png"
ggsave(nombre_archivo, plot_h1, width = 8, height = 6, dpi = 300)

print(plot_h1)

#Se realiza la prueba de normalidad Kolmorov-Smirnov
norm_edad<-lillie.test(ENOE_Mincer$eda)

# Crear un dataframe con la variable 'edad' de ENOE_Mincer
vector_edad <- ENOE_Mincer$eda
df_edad <- data.frame(Edad = vector_edad)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_edad, aes(x = "", y = ENOE_Mincer$eda)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable EDAD",
    x = "",
    y = "Edad"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_edad.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos (opcional)
print(grafico_boxplot)


######################### Elaboración del histograma, prueba de normalidad y boxplot de la variable años de escolaridad ###########################

# Crea el objeto ggplot y establece los datos y el mapeo estético
escolar<-as.data.frame(ENOE_Mincer$anios_esc)
plot_h2 <- ggplot(escolar, aes(x = ENOE_Mincer$anios_esc))

# Agrega el histograma y la función de densidad
plot_h2<- plot_h2 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$anios_esc), sd = sd(ENOE_Mincer$anios_esc)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h2<- plot_h2 +
  theme_solarized() +
  labs(x = "Escolaridad", y = "Densidad", title= "ESCOLARIDAD") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_escolar.png"
ggsave(nombre_archivo, plot_h2, width = 8, height = 6, dpi = 300)

plot(plot_h2)

lillie.test(ENOE_Mincer$anios_esc)

# Crear un dataframe con la variable 'escolaridad' de ENOE_Mincer
vector_anios_esc <- ENOE_Mincer$anios_esc
df_anios_esc <- data.frame(escolar = vector_anios_esc)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_anios_esc, aes(x = "", y = ENOE_Mincer$anios_esc)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable ESCOLARIDAD",
    x = "",
    y = "Escolaridad"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_escolar.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

#################### Elaboración del histograma, prueba de normalidad y boxplot de la variable horas trabajadas por semana #######################

# Crea el objeto ggplot y establece los datos y el mapeo estético
hrs_trab<-as.data.frame(ENOE_Mincer$hrsocup)
plot_h3 <- ggplot(hrs_trab, aes(x = ENOE_Mincer$hrsocup))

# Agrega el histograma y la función de densidad
plot_h3<- plot_h3 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$hrsocup), sd = sd(ENOE_Mincer$hrsocup)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h3 <- plot_h3 +
  theme_solarized() +
  labs(x = "Horas Trabajadas por Semana", y = "Densidad", title= "HORAS TRABAJADAS/SEMANA") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_hrs_semana.png"
ggsave(nombre_archivo, plot_h3, width = 8, height = 6, dpi = 300)

plot(plot_h3)

lillie.test(ENOE_Mincer$hrsocup)

# Crear un dataframe con la variable 'horas trabajadas por semana' de ENOE_Mincer
vector_hrs_sem <- ENOE_Mincer$hrsocup
df_horas_sem <- data.frame(horas_sem = vector_hrs_sem)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_horas_sem, aes(x = "", y = ENOE_Mincer$hrsocup)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable HORAS TRABAJADAS/SEMANA",
    x = "",
    y = "Horas Trabajadas por Semana"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_hrs_sem.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

######################### Elaboración de histograma, prueba de normalidad y boxplot de la variable ingresos por semana ##########################

# Crea el objeto ggplot y establece los datos y el mapeo estético
ing_sem <-as.data.frame(ENOE_Mincer$ingocup)
plot_h4 <- ggplot(ing_sem, aes(x = ENOE_Mincer$ingocup))

# Agrega el histograma y la función de densidad
plot_h4 <- plot_h4 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$ingocup), sd = sd(ENOE_Mincer$ingocup)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h4 <- plot_h4 +
  theme_solarized() +
  labs(x = "Ingresos por Semana", y = "Densidad", title= "INGRESOS/SEMANA") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_ing_semana.png"
ggsave(nombre_archivo, plot_h4, width = 8, height = 6, dpi = 300)

plot(plot_h4)


lillie.test(ENOE_Mincer$ingocup)

# Crear un dataframe con la variable 'ingresos por semana' de ENOE_Mincer
vector_ing_sem <- ENOE_Mincer$ingocup
df_ing_sem <- data.frame(ing_sem = vector_ing_sem)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_ing_sem, aes(x = "", y = ENOE_Mincer$ingocup)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable INGRESOS POR SEMANA",
    x = "",
    y = "Ingresos por Semana"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_ing_sem.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

########################## Elaboración de histograma, prueba de normalidad y boxplot de ingresos por hora ###########################

# Crea el objeto ggplot y establece los datos y el mapeo estético
ing_hora <-as.data.frame(ENOE_Mincer$ing_x_hrs)
plot_h5 <- ggplot(ing_hora, aes(x = ENOE_Mincer$ing_x_hrs))

# Agrega el histograma y la función de densidad
plot_h5 <- plot_h5 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$ing_x_hrs), sd = sd(ENOE_Mincer$ing_x_hrs)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h5 <- plot_h5 +
  theme_solarized() +
  labs(x = "Ingresos por Hora", y = "Densidad", title= "INGRESOS/HORA") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_ing_hrs.png"
ggsave(nombre_archivo, plot_h5, width = 8, height = 6, dpi = 300)

plot(plot_h5)


lillie.test(ENOE_Mincer$ing_x_hrs)

# Crear un dataframe con la variable 'ingresos por hora' de ENOE_Mincer
vector_ing_horas <- ENOE_Mincer$ing_x_hrs
df_ing_horas <- data.frame(ing_horas = vector_ing_horas)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_ing_horas, aes(x = "", y = ENOE_Mincer$ing_x_hrs)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable INGRESOS POR HORA",
    x = "",
    y = "Ingresos por Hora"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_ing_x_hrs.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

#############################Elaboración de histograma, prueba de normalidad y boxplot de experiencia ###############################

# Crea el objeto ggplot y establece los datos y el mapeo estético
experiencia <-as.data.frame(ENOE_Mincer$exp)
plot_h6 <- ggplot(experiencia, aes(x = ENOE_Mincer$exp))

# Agrega el histograma y la función de densidad
plot_h6 <- plot_h6 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$exp), sd = sd(ENOE_Mincer$exp)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h6 <- plot_h6 +
  theme_solarized() +
  labs(x = "Experiencia", y = "Densidad", title= "EXPERIENCIA") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_exp.png"
ggsave(nombre_archivo, plot_h6, width = 8, height = 6, dpi = 300)

plot(plot_h6)

lillie.test(ENOE_Mincer$exp)  

# Crear un dataframe con la variable 'experiencia' de ENOE_Mincer
vector_experiencia <- ENOE_Mincer$exp
df_experiencia <- data.frame(experiencia = vector_experiencia)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_experiencia, aes(x = "", y = ENOE_Mincer$exp)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable EXPERIENCIA",
    x = "",
    y = "Experiencia"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_exp.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

###################### Elaboración de histograma, prueba de normalidad y boxplot de experiencia al cuadrado ###########################

# Crea el objeto ggplot y establece los datos y el mapeo estético
experiencia_2 <-as.data.frame(ENOE_Mincer$exp_2)
plot_h7 <- ggplot(experiencia_2, aes(x = ENOE_Mincer$exp_2))

# Agrega el histograma y la función de densidad
plot_h7 <- plot_h7 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$exp_2), sd = sd(ENOE_Mincer$exp_2)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h7 <- plot_h7 +
  theme_solarized() +
  labs(x = "Experiencia^2", y = "Densidad", title= "EXPERIENCIA AL CUADRADO") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_exp_2.png"
ggsave(nombre_archivo, plot_h7, width = 8, height = 6, dpi = 300)

plot(plot_h7)

norm_exp_2<-lillie.test(ENOE_Mincer$exp_2)

# Crear un dataframe con la variable 'experiencia al cuadrado' de ENOE_Mincer
vector_experiencia_2 <- ENOE_Mincer$exp^2
df_experiencia_2 <- data.frame(experiencia_2 = vector_experiencia_2)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_experiencia_2, aes(x = "", y = ENOE_Mincer$exp^2)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable EXPERIENCIA AL CUADRADO",
    x = "",
    y = "Experiencia al Cuadrado"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_exp^2.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

############################# CONSTRUCCIÓN Y ANÁLISIS DE LA VARIABLE DEPENDIENTE LN_ING ####################
############################################################################################################

###################### Elaboración de histograma, prueba de normalidad y boxplot del logaritmo del ingreso ###########################

### Se construye la variable de logaritmo del ingreso
ENOE_Mincer$ln_ing<-log(ENOE_Mincer$ing_x_hrs)
### Se eliminan los valores en los que no se puede calcular el logaritmo
ENOE_Mincer <- subset(ENOE_Mincer, !is.infinite(ln_ing))  


# Crea el objeto ggplot y establece los datos y el mapeo estético
ln_ingreso <-as.data.frame(ENOE_Mincer$ln_ing)
plot_h8 <- ggplot(ln_ingreso, aes(x = ENOE_Mincer$ln_ing))

# Agrega el histograma y la función de densidad
plot_h8 <- plot_h8 +
  geom_histogram(aes(y = ..density..), color = "black", size = 0.1, fill = "gray", alpha = 0.5, binwidth = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(ENOE_Mincer$ln_ing), sd = sd(ENOE_Mincer$ln_ing)), color = "brown", size = 0.6) 

# Personaliza el tema y las etiquetas
plot_h8 <- plot_h8 +
  theme_solarized() +
  labs(x = "log_ingreso", y = "Densidad", title= "LOGARITMO NATURAL DEL INGRESO POR HORA") + 
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "histograma_ln_ing.png"
ggsave(nombre_archivo, plot_h8, width = 8, height = 6, dpi = 300)

plot(plot_h8)

norm_exp_2<-lillie.test(ENOE_Mincer$ln_ing)

# Crear un dataframe con la variable 'logaritmo del ingreso' de ENOE_Mincer
vector_ln_ingreso <- ENOE_Mincer$ln_ing
df_ln_ingreso <- data.frame(ln_ingreso = vector_ln_ingreso)

# Crear el boxplot utilizando ggplot2
grafico_boxplot <- ggplot(df_ln_ingreso, aes(x = "", y = ENOE_Mincer$ln_ing)) +
  geom_boxplot() +  # Agregar el boxplot
  theme_solarized() + 
  labs(
    title = "Boxplot de la variable LOGARITMO NATURAL DEL INGRESO POR HORA",
    x = "",
    y = "Logaritmo del Ingreso"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "boxplot_ln_ing.png"
ggsave(nombre_archivo, grafico_boxplot, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_boxplot)

############################## ANÁLISIS EXPLORATORIO DE LOS DATOS CON LA VARIABLE LN_ING ###################
############################################################################################################

# Calcula la cantidad de celdas vacías por variable
valores_faltantes_por_variable <- colSums(is.na(ENOE_Mincer) | ENOE_Mincer == "")
valores_faltantes_por_variable

# Resumen descriptivo de los datos cuantitativos ##################################

variables_cuantitativas<- c("eda", "anios_esc","hrsocup","ingocup","ing_x_hrs","exp","exp_2","ln_ing")
datos_cuantitativos_2 <- ENOE_Mincer[, variables_cuantitativas]
datos_cuantitativos_df_2 <-as.data.frame(datos_cuantitativos_2)

descriptivos_2 <- describe(datos_cuantitativos_df_2)
descriptivos_df_2 <-data.frame(descriptivos_2)
print(descriptivos_2)
nombre_archivo <- "estadisticas_descriptivas_2.xlsx"
write.xlsx(descriptivos_2, file = nombre_archivo)

################## SE CALCULAN MEDIA Y DESVIACIÓN STANDAR  WINSORIZADAS ###########
edad_rob <- huber(ENOE_Mincer$eda)
escolar_rob <- huber(ENOE_Mincer$anios_esc)
hrs_sem_rob <- huber(ENOE_Mincer$hrsocup)
ing_sem_rob <- huber(ENOE_Mincer$ingocup)
ing_hrs_rob <- huber(ENOE_Mincer$ing_x_hrs)
exp_rob <- huber(ENOE_Mincer$exp)
exp_2_rob <- huber(ENOE_Mincer$exp_2)
ln_ing_rob <- huber(ENOE_Mincer$ln_ing)

# Extraer los valores numéricos de los objetos 

edad_rob_value <- as.numeric(ENOE_Mincer$eda)
escolar_rob_value <- as.numeric(ENOE_Mincer$anios_esc)
hrs_sem_rob_value <- as.numeric(ENOE_Mincer$hrsocup)
ing_sem_rob_value <- as.numeric(ENOE_Mincer$ingocup)
ing_hrs_rob_value <- as.numeric(ENOE_Mincer$ing_x_hrs)
exp_rob_value <- as.numeric(ENOE_Mincer$exp)
exp_2_rob_value <- as.numeric(ENOE_Mincer$exp_2)
ln_ing_rob_value <- as.numeric(ENOE_Mincer$ln_ing)

# Crear un data frame con los estadísticos y nombres de columna
estadisticos_robustos <- data.frame(
  edad = c(mean(edad_rob_value), sd(edad_rob_value)),
  escolaridad = c(mean(escolar_rob_value), sd(escolar_rob_value)),
  horas_semana = c(mean(hrs_sem_rob_value), sd(hrs_sem_rob_value)),
  ing_semana = c(mean(ing_sem_rob_value), sd(ing_sem_rob_value)),
  ing_hora = c(mean(ing_hrs_rob_value ), sd(ing_hrs_rob_value )),
  experiencia = c(mean(exp_rob_value), sd(exp_rob_value)),
  experiencia_2 = c(mean(exp_2_rob_value), sd(exp_2_rob_value)),
  ln_ing = c(mean(ln_ing_rob_value), sd(ln_ing_rob_value))
)

# Asignar nombres a las filas
rownames(estadisticos_robustos) <- c("Media Robusta", "SD Robusta")

# Transponer las estadisticas robustas
stat_robust <-data.frame(t(estadisticos_robustos))
print(stat_robust)
nombre_archivo <- "estadisticas_robustas.xlsx"
write.xlsx(stat_robust, file = nombre_archivo)

########################### Matriz de Correlaciones ##################################
round(cor(datos_cuantitativos_2),2)
rcorr(as.matrix(datos_cuantitativos_2))
correlacion<-round(cor(datos_cuantitativos_2), 1)
corrplot(correlacion, method="number", type="upper")

# Crear histogramas de las variables cuantitativas y reporte general de estadísticas descriptivas
plot_histogram(datos_cuantitativos_2, title = "Histogramas de Variables Cuantitativas")
plot_density(datos_cuantitativos_2) 
plot_correlation(datos_cuantitativos_2)
plot_qq(datos_cuantitativos_2)
create_report(datos_cuantitativos_2)

################### SE ANALIZAN LA RELACIÓN ENTRE LAS VARIABLES CON GRÁFICAS DE DISPERSIÓN #################
############################################################################################################

###### Relación del ingreso y edad ###########
grafico_disper<-ggplot(ENOE_Mincer, aes(x = eda, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Edad")+
  labs(title = "Relación entre Edad e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous() +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_edad.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)

###### Relación acotada del ingreso y edad ###########
grafico_disper<-ggplot(ENOE_Mincer, aes(x = eda, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Edad")+
  labs(title = "Relación entre Edad e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous(limits= c(0,1000)) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_edad_acotado.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)


###### Relación del ingreso y escolaridad ###############
grafico_disper<-ggplot(ENOE_Mincer, aes(x = anios_esc, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Escolaridad")+
  labs(title = "Relación entre Escolaridad e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous() +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_escolar.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)

###### Relación acotada del ingreso y escolaridad ###############
grafico_disper<-ggplot(ENOE_Mincer, aes(x = anios_esc, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Escolaridad")+
  labs(title = "Relación entre Escolaridad e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous(limits= c(0,1000)) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_escolar_acotado.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)


###### Relación del ingreso y la experiencia ###############
grafico_disper<-ggplot(ENOE_Mincer, aes(x = exp, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Experiencia")+
  labs(title = "Relación entre Experiencia e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous() +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_exp.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)

###### Relación acotada del ingreso y la experiencia ###############
grafico_disper<-ggplot(ENOE_Mincer, aes(x = exp, y = ing_x_hrs )) +
  geom_point(colour="paleturquoise3") + 
  ylab("Ingreso")+
  xlab("Experiencia")+
  labs(title = "Relación entre Experiencia e Ingreso") +
  stat_smooth(method = "lm", se= T, level= 0.90, formula = y~x, color= "black") +
  scale_x_continuous() + 
  scale_y_continuous(limits= c(0,1000)) +
  theme(plot.title = element_text(hjust = 0.5))

#Guardar el gráfico en un archivo de alta calidad (por ejemplo, formato PNG)
nombre_archivo <- "ing_exp_acotada.png"
ggsave(nombre_archivo, grafico_disper, width = 8, height = 6, dpi = 300)

# Mostrar el gráfico en la ventana de gráficos
print(grafico_disper)
