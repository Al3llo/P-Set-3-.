#TALLER R - Estadistica y programación

# Alejandro Correa Jimenez - 202022775

#Versión R: R version 4.3.1 (2023-06-16 ucrt)


########################################################################################################################################################

# Antes de empezar clono los archivos del repositorio en mi computadora de la siguiente forma:

# Abro Powershell y voy a la carpeta donde almacenare la informacion:
# cd C:/Taller3EstadisticaconR
# #Clone todo el repositorio con la siguiente linea de comando:
# git clone https://github.com/taller-r-202401/problem-sets.git
# Defino la carpeta de los archivos de entrada de la Encuesta: 
# "C:\Taller3EstadisticaconR\problem-sets\pset-3\input\2023"

########################################################################################################################################################

# Instalo pacman:
install.packages("pacman")

# Instalo y/o cargo los paquetes con pacman:

pacman::p_load(rio, data.table, tidyverse, ggplot2)


##################################################################################
############################ Taller 3: Problem set 3  ############################
##################################################################################

########################### 1) Bucle de importación #######################

#### Parte 1.1 Lista de Archivos: ####

# Como cada archivo esta en una carpeta aparte con el nombre del mes, entonces hago una lista de la ruta de todas las carpetas
# uso el recursive para que se obtenga el nombre completo con las subcarpetas, además empleo ".rds$", ya que todos los archivos
# que me interesan tienen esta extension:

ListaarchivosRDS <- list.files("C:/Taller3EstadisticaconR/problem-sets/pset-3/input/2023", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)


#### Parte 1.2 Lista de Archivos: ####

#Creo una función para importar los archivos de interes que son: Fuerza de trabajo, No ocupados y Ocupados, para los doce meses:
# Esta función incluye dos partes en su contenido una para leer los archivos empleo la funcion readRDS de R Base y la otra es para
# agregar un indice que nos sirva como punto de referencia de cada archivo:

ImportarArchivoRDS <- function(archivo) {
  nombre_archivo <- basename(archivo)  
  contenido <- readRDS(archivo)
  contenido$INDICEDOCUMENTO <- seq_len(nrow(contenido)) 
  return(contenido) 
}

#Uso la funcion anterior y aplico lapply para almacenar todo en una lista que llamo ArchivosImportados
ArchivosImportados <- lapply(ListaarchivosRDS, ImportarArchivoRDS)

#Renombro la lista para tener los nombres originales sin la extension:
names(ArchivosImportados) <- gsub(".rds", "", basename(ListaarchivosRDS))

#### Parte 1.3 Combinar conjuntos de datos: ####

# Extraigo 3 nuevos dataframes para cada grupo de archivos de interés de ArchivosImportados usando rbindlist.
# fill = TRUE se usa para manejar diferencias en columnas o nombres.

# Como en el punto anterior renombre los archivos en la lsita para quitarle la extension y url completa, 
# entonces creo indices de donde estan los archivos con el mismo nombre en Archivos importados:

IndiceFuerzaTrabajo <- which(names(ArchivosImportados) == "Fuerza de trabajo")
IndiceNoOcupado <- which(names(ArchivosImportados) == "No ocupados")
IndiceOcupado <- which(names(ArchivosImportados) == "Ocupados")

# Ahora si creo los tres dataframes para trabajar usando la funcion rbindlist de data.table, que me ayudara
# a unir todos los archivos de la lista en los tres dataframes de cada grupo:

FuerzaDeTrabajo <- rbindlist(ArchivosImportados[IndiceFuerzaTrabajo], fill = TRUE)
NoOcupados <- rbindlist(ArchivosImportados[IndiceNoOcupado], fill = TRUE)
Ocupados <- rbindlist(ArchivosImportados[IndiceOcupado], fill = TRUE)

############################## 2) Preparacion ##############################

#### Parte 2.1 Creacion de bases de datos #### 

# a)Base de datos de Fuerza de Trabajo

# Usando la base de datos "FuerzaDeTrabajo", he creado una otra base de datos llamada "BDFuerzatrabajo". 
# Inicialmente agrupo los datos por mes usando group_by(MES). 
# Luego, creo dos columnas, en una  calculo la poblacion en edad de trabajar por mes sumando el numero de individuos que hacen parte 
# de la población en edad de trabajar (PET == 1) y multiplicando este valor por el factor de expansion (FEX_C18) para tener en cuenta la 
# representatividad de la muestra. Despues, en la otra columna calculo la fuerza laboral por mes sumando el numero de individuos que hacen 
# parte de la fuerza laboral (FT == 1), y tambien multiplicando este valor por el factor de expansion (FEX_C18).
#Finalmente selecciono las columnas de MES, mi guia INDICEDOCUMENTO y las dos que acabo de calcular


BDFuerzatrabajo <- FuerzaDeTrabajo %>%
  group_by(MES) %>% mutate(PoblacionEnEdadParaTrabajar = sum(PET == 1, na.rm = TRUE) * FEX_C18, FuerzaLaboral = sum(FT == 1, na.rm = TRUE) * FEX_C18) %>%
  select(MES, INDICEDOCUMENTO,FT,FFT, PoblacionEnEdadParaTrabajar, FuerzaLaboral)


# B)Base de datos de Ocupados

# Algo similar a lo realizado en el punto anterior hago en la base de datos de Ocupados pero aca sumo el numero de individuos empleados (FT == 1) 
# por mes y lo multiplico por el factor de expancion (FEX_C18). Tambien selecciono las columnas MES, INDICE DOCUMENTO y OCupados

BDOcupados <- Ocupados %>%
  group_by(MES) %>%
  mutate(Ocupados = sum(FT == 1, na.rm = TRUE) * FEX_C18) %>%
  select(MES, INDICEDOCUMENTO,FT, Ocupados)



# C)Base de datos de No Ocupados

# Para terminar hago lo mismo pero ahora creo la columna Desempleados multiplicando DSI ==1 por Fex_C18 y repito la misma seleccion que los casos
# anteriores.

BDNoOcupados <- NoOcupados %>%
  group_by(MES) %>%
  mutate(Desempleados = sum(DSI == 1, na.rm = TRUE) * FEX_C18) %>%
  select(MES, INDICEDOCUMENTO,F, Desempleados)

#### 2.2 Colapsar datos a nivel mensual #### 

# Creo el dataframe de salida llamado output, esto lo hago con full_join para unir todo por MES y por la columna
# INDICEDOCUMENTO que agregue al inicio que me sirve como punto de referencia de cada fila
# Primero combino mis datos de BDFuerza con BDOcupados agrupandolos por MES e INDICEDOCUMENTO:

PreOutPut <- full_join(FuerzaDeTrabajo, BDOcupados, by = c("MES", "INDICEDOCUMENTO","FT"))
output <- full_join(PreOutPut, BDNoOcupados, by = c("MES", "INDICEDOCUMENTO","FFT"))


#### 2.3 Tasas de desempleo y ocupacion #### 

# creo dos columnas con la tasa de desempleo y ocupacion. Para la tasa de desempleo solo divido la columna desempleados sobre FuerzaLaboral
# y para Tasa de Ocupacion tomo la columna Ocupados sobre PoblacionEnEdadParaTrabajar:

output <- output %>% 
  mutate(TasaDesempleo = Desempleados / FuerzaLaboral, 
         TasaOcupacion = Ocupados / PoblacionEnEdadParaTrabajar)

names(output)
############################## 3) GGplot2 ###############################


library(ggplot2)

# Suponiendo que 'output' es tu dataframe y tiene las columnas 'MES', 'TasaDesempleo', y 'TasaOcupacion':
ggplot(data = output, aes(x = MES)) + 
  geom_line(aes(y = TasaDesempleo, colour = "Tasa de Desempleo")) + 
  geom_line(aes(y = TasaOcupacion, colour = "Tasa de Ocupación")) +
  labs(title = "Tasas de Desempleo y Ocupación por Mes",
       x = "Mes",
       y = "Tasa (%)",
       colour = "Leyenda") +
  theme_minimal()





# Pivot wider para tener las tasas de desempleo y ocupación en una sola columna
output_pivot <- output %>%
  pivot_wider(names_from = MES, values_from = c(TasaDesempleo, TasaOcupacion))

# Convertir a formato long para facilitar la graficación
output_long <- output_pivot %>%
  pivot_longer(cols = starts_with("Tasa"), names_to = "Tipo", values_to = "Tasa", names_prefix = "Tasa")

# Graficar las tasas de desempleo y ocupación para cada mes
ggplot(output_long, aes(x = INDICEDOCUMENTO, y = Tasa, color = Tipo)) +
  geom_line() +
  labs(x = "Mes", y = "Tasa", color = "Tipo") +
  theme_minimal()


library(ggplot2)
library(tidyr)

# Seleccionar las columnas necesarias
tasas <- output %>%
  select(MES, TasaDesempleo, TasaOcupacion) %>%
  distinct()

# Pivotear los datos para tener tasas en una columna
tasas <- pivot_longer(tasas, cols = c(TasaDesempleo, TasaOcupacion), names_to = "tasa", values_to = "valor")

# Graficar las tasas
ggplot(tasas, aes(x = MES)) +
  geom_line(aes(y = `TasaDesempleo`), color = "red") +
  geom_line(aes(y = `TasaOcupacion`), color = "blue") +
  labs(x = "Mes", y = "Tasa", title = "Tasas de Desempleo y Ocupación") +
  theme_bw()

# Contar el número de NA en la columna MES
 sum(is.na(output$TasaOcupacion))


