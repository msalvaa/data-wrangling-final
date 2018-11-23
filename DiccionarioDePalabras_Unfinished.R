library(stringr)
library(lubridate)

#files para hacer el loop
files <- list.files(path="Mes1Data", pattern="*.csv", full.names=TRUE, recursive=TRUE)

#diccionario de codigos
dicc = data.frame(matrix(vector(), 10, 2,
                         dimnames=list(c(), c("Codigo", "ACABADO"))),
                  stringsAsFactors=F)
dicc$COD=c("01","02","03","04","05","06","07","08","23","63")
dicc$ACABADO=c("MF","AN","BR","NE","CH","BL","MRT","AI","MNT","CT")

#columnas que se necesitan
nombres=c("FECHA","TIENDA","CODPROD","DESCRIPCION","MEDIDA","UNI","VENTA")

#df donde se va a guardar todo
table=data.frame()

#loop
for(file in files){
  diario <- read.csv(file,header=FALSE)
  #para la columna de fecha
  name <- basename(file)
  diario$FECHA=dmy(substr(name,start=1,stop=6))
  
  #nombre de tienda
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("BODEGA O TIENDA", x))))[[1]]]="TIENDA"
  diario$TIENDA=sub(".*: ", "", diario$TIENDA)
  
  #columnas que sirven - asumiendo que siempre están 9 después
  #problamemente se puede optimizar con otro lapply pero no he logrado que sirva
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("CODPROD", x))))[[2]]+9]="CODPROD"
  
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("DESCRIPCION", x))))[[2]]+9]="DESCRIPCION"
  
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("MEDIDA", x))))[[2]]+9]="MEDIDA"
  
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("UNI", x))))[[2]]+9]="UNI"
  
  colnames(diario)[which(apply(diario, 2, function(x) any(grepl("\\VENTA\\b", x))))[[2]]+9]="VENTA"
  
  #subset de estas columnas
  diario=diario[,nombres]
  
  #quitarle lo que está en parentesis
  diario$DESCRIPCION=gsub("\\s*\\([^\\)]+\\)","",as.character(diario$DESCRIPCION))
  
  #precio unitario
  diario$PRECIOUNITARIO=diario$VENTA/diario$UNI
  
  #quitar las ' que están en algunas dimensiones (también se podran remplazar todas las X mayúsuculas
  #por minúsculas para consistencia)
  diario$DESCRIPCION=gsub("\"","",diario$DESCRIPCION)
  
  #separar los primeros dos numeros del codigo para el diccionario
  diario$COD=substr(diario$CODPROD,start=1,stop=2)
  
  #merge de esto con el diccionario
  diario=merge(diario,dicc,by="COD",all.x = TRUE)
  
  #eliminar la columna que se hizo
  diario$COD=NULL
  diario$Codigo=NULL
  
  #quito si tiene NAs en las columnas numericas
  diario=diario[complete.cases(diario[,6:8]),]
  
  #vuelvo esta columna caracter porque sino me tira error
  diario$CODPROD=as.character(diario$CODPROD)
  
  
  #bind con la tabla principal
  table=rbind(table,diario)
  
}



write.csv(file="abla.csv",x=table)


#tabla de NAs en acabado
library(dplyr)

na=table%>%
  filter(is.na(ACABADO))



#//////////////////////////////// Yuri Kaffaty /////////////////////////////////////////
##-------------------------------- Prodcuto Mas Vendido (Propuesta 1) _______________________________
library(dplyr)
library(ggplot2)
library(scales)

prod_ventas = select(table,DESCRIPCION,UNI,VENTA,ACABADO)
prod_ventas <- mutate(prod_ventas, 
                      PrecioUnitario = VENTA/UNI)                     
prod_ventas = group_by(prod_ventas, DESCRIPCION,ACABADO)
Resumen <- summarize(prod_ventas, PRODUCTO=length(DESCRIPCION),
                     TOTAL_VENTA = sum(VENTA, na.rm = TRUE))
Resumen = arrange(Resumen,desc(TOTAL_VENTA))

ggplot(data = head(Resumen,10),
       aes(x = DESCRIPCION, y= TOTAL_VENTA, TOTAL_VENTA))+ coord_flip() +
  geom_bar(aes(colour = TOTAL_VENTA), stat = "identity")  +
  scale_fill_gradient(low = "mediumblue", high = "aquamarine2")



# -------------------------------- Ventas por Dia ---------------------------



ventasxdia = select(table,FECHA,DESCRIPCION,UNI,VENTA,ACABADO )
ventasxdia = group_by(ventasxdia,FECHA)
Resumenxdia <- summarize(ventasxdia, VENTASDELDIA= length(FECHA),
                         VENTAPORDIA = sum(VENTA, na.rm = TRUE))
Resumenxdia = arrange(Resumenxdia, desc(VENTAPORDIA))

ggplot(data = Resumenxdia,
       aes(x = FECHA, y= VENTAPORDIA, VENTAPORDIA))+ coord_flip() +
  geom_bar(aes(colour = VENTAPORDIA), stat = "identity")  +
  scale_fill_gradient(low = "mediumblue", high = "aquamarine2")


#--------------------------------- Diccionario de Palabras -----------------------

dicc_codigos = read.csv("codigos.csv")
indice_dicc = 1 #que linea de la clumna codprod de la tabla na 
indice_cod = 1 #que linea de la clumna codigo de la tabla codigos
while (indice_cod < length( dicc_codigos$�..PrefijoCodigo))
{
  if(grepl(dicc_codigos$�..PrefijoCodigo[indice_dicc],na$CODPROD[indice_cod])==TRUE)
  {
    na$ACABADO[indice_cod] = dicc_codigos$Valor_En_Comun[indice_dicc]
    indice_cod = indice_cod + 1
    
  } else if(grepl(dicc_codigos$�..PrefijoCodigo[indice_dicc],na$CODPROD[indice_cod])==FALSE)
  {
    indice_dicc = indice_dicc + 1
  }
}
#///////////////////////////  



