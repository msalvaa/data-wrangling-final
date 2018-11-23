library(stringr)
library(lubridate)

#files para hacer el loop
files <- list.files(path="C:/Users/Toshiba UL/Dropbox/semestre 8/data/final", pattern="*.csv", full.names=TRUE, recursive=TRUE)
#files <- list.files(path = "D:/geord/Docs/Data Wrangling/final/files")

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

# FUNCION QUE RECIBE CUALQUIER ARCHIVO .TXT, .CSV, .XLSX Y LO TRANSFORMA A UN DATA FRAME
library(readxl)

ext <- "any"

transformar <- function(archivoCualquiera){
  if (isTRUE(str_detect(ext,"any"))){
    punto = regexpr("\\.", archivoCualquiera)
    ext = substr(archivoCualquiera, punto+1, str_length(archivoCualquiera))
    ext = toupper(ext)
  }
  punto = regexpr("\\.", archivoCualquiera)
  if (ext == "XLSX"){
    dataSample <- read_excel(archivoCualquiera, col_names = FALSE)
  } else if (ext == "CSV"){
    dataSample <- read.csv(archivoCualquiera, header = FALSE, stringsAsFactors=FALSE )
  } else if (ext == "TXT"){
    #dataSample <- read.table(archivoCualquiera, header = FALSE, sep = ",")
    dataSample <-read.delim(textConnection(archivoCualquiera),header=FALSE,sep=":",strip.white=TRUE, stringsAsFactors = FALSE)
    #cols<-levels(dataSample[,'V1'])
    #dataSample <-data.frame(sapply(cols,function(x) {dataSample['V2'][dataSample['V1']==x]}, USE.NAMES=TRUE)) # normaliza en columnas
  }
  return(dataSample)
}



#loop
for(file in files){
  #diario <- read.csv(file,header=FALSE)
  diario <- transformar(file)
  
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
  
  #quitar las ' que están en algunas dimensiones (también se podrían remplazar todas las X mayúsuculas
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

View(table)
write.csv(file="C:/Users/Toshiba UL/Dropbox/semestre 8/data/final/tabla.csv",x=table)





#tabla de NAs en acabado
library(dplyr)

na=table%>%
  filter(is.na(ACABADO))

