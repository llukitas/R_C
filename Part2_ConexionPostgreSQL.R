#install.packages("RPostgreSQL")
require("RPostgreSQL")

# Guardar el Password para poder posteriormente eliminarlo
pw <- { "sdc2019PERU"}

# Leer el driver de PostgreSQL
drv <- dbDriver("PostgreSQL")

# Crear la conexion con la base de datos
con <- dbConnect(drv, dbname = "voto_capitales_2016",
                 host = "69.164.192.245", port = 5432,
                 user = "postgres", password = pw)

# Eliminar el Password
rm(pw) 

# Extraer datos de la base de datos
dataclase <- dbGetQuery(con, "SELECT * from tabla_resultados")