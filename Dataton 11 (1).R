# instalar librerias 
install.packages("dplyr")
library(dplyr)
library(readr)

# importar Bases de datos train (lite)


Datos_transaccionales_train <- read_csv("DT19_Datos_transaccionales_train_lite.csv")

Datos_Var_Rpta_train <- read_csv("DT19_Datos_Var_Rpta_train_lite.csv")

# manipulacion para obtener variables independientes


## nueva variable: valor total transferido por usuario
### Se suma el valor de todas las transacciones de cada usario

valor_transaccion <-aggregate(vlrtran~id,Datos_transaccionales_train,sum)

# añadiendo la nueva variable a los datos del modelo

Datos_modelo_train <- merge(Datos_Var_Rpta_train,valor_transaccion,by="id")


##nueva variable: numnero de transacciones exitosas
### se suman el numero de transacciones financieras exitosas de cada individuo

#### primero seleccionamos la variable codigo de respuesta
n_transacciones_exitosas <- select(Datos_transaccionales_train,id,cdgrpta)

#### Luego filtramos las transacciones exitosas (codigo de respuesta =0)
n_transacciones_exitosas <- filter(n_transacciones_exitosas,cdgrpta == 0)

#### por ultimo sumamos el numero de transacciones exitosas por usuario
n_transacciones_exitosas <- aggregate(cdgrpta~id,n_transacciones_exitosas+1,sum)


View(n_transacciones_exitosas)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_train_2 <- merge(Datos_modelo_train,n_transacciones_exitosas,by="id",all = TRUE )

# remplasamos los NA por 0 (que indica q este usuario no realizo transacciones exitosas)
Datos_modelo_train_2$cdgrpta [is.na(Datos_modelo_train_2$cdgrpta )] <- 0

# cambiando el nombre de las variables
names (Datos_modelo_train_2)[6] = "n_transacciones_exitosas"
names (Datos_modelo_train_2)[5] = "valor_total_transferido"

View(Datos_modelo_train_2)

## nueva variable: valortotal/ n transacciones

#### primero seleccionamos 
val_por_transaccion  <- select(Datos_modelo_train_2,id,n_transacciones_exitosas,valor_total_transferido)

#### Luego filtramos, obteniendo a los usuarios que al menos hayan echo una transaccion exitosa
val_por_transaccion  <- filter(val_por_transaccion,n_transacciones_exitosas > 0)

#### por ultimo sumamos el numero de transacciones exitosas por usuario
val_por_transaccion [4] <-val_por_transaccion$valor_total_transferido/val_por_transaccion$n_transacciones_exitosas

# asigando el nombre a la nueva variable
names (val_por_transaccion)[4] = "val_por_transaccion"

View(val_por_transaccion)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_train_2 <- merge(Datos_modelo_train_2,select(val_por_transaccion,id,val_por_transaccion),by="id",all = TRUE )

#remplazando na por 0
Datos_modelo_train_2$val_por_transaccion [is.na(Datos_modelo_train_2$val_por_transaccion )] <- 0



#nueva variable: numero de dispositivos

cantidad_de_dispositivos <-aggregate(disposit~id,Datos_transaccionales_train,n_distinct)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_train_2 <- merge(Datos_modelo_train_2,cantidad_de_dispositivos,by="id",all = TRUE )

View(Datos_modelo_train_2)

# por ultimo exportamos la base de datos para el modelo
write.csv(Datos_modelo_train_2,"Datos_modelo_train_3.csv")


# Realizamos el mismo proceso para la base de datos predict, si desea puede consultar el codigo a continuacion


####  importar Bases de datos predict (lite) ####


# importar Bases de datos train (lite)


Datos_transaccionales_predict <- read_csv("DT19_Datos_transaccionales_predict.csv")

# manipulacion para obtener variables independientes


## nueva variable: valor total transferido por usuario
### Se suma el valor de todas las transacciones de cada usario

valor_transaccion <-aggregate(vlrtran~id,Datos_transaccionales_predict,sum)

# añadiendo la nueva variable a los datos del modelo

Datos_modelo_predict <- valor_transaccion


##nueva variable: numnero de transacciones exitosas
### se suman el numero de transacciones financieras exitosas de cada individuo

#### primero seleccionamos la variable codigo de respuesta
n_transacciones_exitosas <- select(Datos_transaccionales_predict,id,cdgrpta)

#### Luego filtramos las transacciones exitosas (codigo de respuesta =0)
n_transacciones_exitosas <- filter(n_transacciones_exitosas,cdgrpta == 0)

#### por ultimo sumamos el numero de transacciones exitosas por usuario
n_transacciones_exitosas <- aggregate(cdgrpta~id,n_transacciones_exitosas+1,sum)


View(n_transacciones_exitosas)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_predict_2 <- merge(Datos_modelo_predict,n_transacciones_exitosas,by="id",all = TRUE )

# remplasamos los NA por 0 (que indica q este usuario no realizo transacciones exitosas)
Datos_modelo_predict_2$cdgrpta [is.na(Datos_modelo_predict_2$cdgrpta )] <- 0

# cambiando el nombre de las variables
names (Datos_modelo_predict_2)[3] = "n_transacciones_exitosas"
names (Datos_modelo_predict_2)[2] = "valor_total_transferido"

View(Datos_modelo_predict_2)

## nueva variable: valortotal/ n transacciones

#### primero seleccionamos 
val_por_transaccion  <- select(Datos_modelo_predict_2,id,n_transacciones_exitosas,valor_total_transferido)

#### Luego filtramos, obteniendo a los usuarios que al menos hayan echo una transaccion exitosa
val_por_transaccion  <- filter(val_por_transaccion,n_transacciones_exitosas > 0)

#### por ultimo sumamos el numero de transacciones exitosas por usuario
val_por_transaccion [4] <-val_por_transaccion$valor_total_transferido/val_por_transaccion$n_transacciones_exitosas

# asigando el nombre a la nueva variable
names (val_por_transaccion)[4] = "val_por_transaccion"

View(val_por_transaccion)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_predict_2 <- merge(Datos_modelo_predict_2,select(val_por_transaccion,id,val_por_transaccion),by="id",all = TRUE )

#remplazando na por 0
Datos_modelo_predict_2$val_por_transaccion [is.na(Datos_modelo_predict_2$val_por_transaccion )] <- 0



#nueva variable: numero de dispositivos

cantidad_de_dispositivos <-aggregate(disposit~id,Datos_transaccionales_predict,n_distinct)

# añadiendo la nueva variable a los datos del modelo_2
Datos_modelo_predict_2 <- merge(Datos_modelo_predict_2,cantidad_de_dispositivos,by="id",all = TRUE )
View(Datos_modelo_predict_2)

# por ultimo exportamos la base de datos para el modelo

write.csv(Datos_modelo_predict_2,"Datos_modelo_predict_3.csv")
