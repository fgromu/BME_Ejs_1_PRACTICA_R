#NO HE PODIDO/SABIDO HACER LO DEL SONIDO. ASI QUE AL FINAL LO HE QUITADO
rm(list=ls())

#setwd("C:/Users/Lenovo/OneDrive/Escritorio/BNE/dat")
#cargo udf8
#CARGAR LAS LIBRERIAS xlsx, readxl Y svDialogs.
library(xlsx)
library("readxl")
library(svDialogs)
#library(profvis)


#################1. DESCARGARBASE DE DATOS####################
#########LA UNICA FORMA QUE HE CONSEGUIDO PARA CARGARLO HA SICO CONVERTIRLO ANTES EN CSV
######## NO SE SI EL PROBLEMA ES EL JAVA O QUE ES LO QUE ES
######## PARA HACERLO SIN JAVA, HE TENIDO QUE USAR LA FUCNION read_excel YA QUE read.xlsx ME DA PROBLEMAS

dic=read_excel("Palabras.xlsx",col_names = FALSE)
#read.xlsx(paste(getwd(),"/warrants.xls", sep = ""), sheetName="Warrants", startRow=6, endRow=NULL, , as.data.frame=TRUE, header=TRUE, keepFormulas=FALSE, encoding="UTF-8")


#################2. REVISION GENERICA DE DF####################
print(class(dic)) #checkeo preliminar
print(dim(dic))
print(length(dic))
print(names(dic))
titulos=names(dic)
print(str(dic))
print(summary(dic))


########REVISO QUE NO TIENE NULLS, NAS Y VACIOS

for( i in 1:ncol(dic)){
  p_uno = sapply(dic[i], is.null)
  p_dos = as.character(p_uno)
  print(paste("De columna",i, "Hay NULLs?",p_dos))
}

for( i in 1:ncol(dic)){
  uno = any(is.na(dic[i]))
  dos = sum(is.na(dic[i]))
  longitud = nrow(dic[i])
  print(paste("De columna",i, "hay NAs:",uno, "en una cantidad de:",dos,"de un total de:",longitud ))
}

for( i in 1:ncol(dic)){#hay un problema es la fila 6
  cuatro = sum(substr(dic[,i],1,1)=="")
  longitud = nrow(dic[i])
  print(paste("De columna",i, "hay espacios vacios en una cantidad de:",cuatro,"de un total de:",longitud ))
}

########No hay vacios, NAs o NULLs


#############################MAIN#############

#######1. aleatorios de 10 palabras a preguntar
n_preguntas = 10
n_preguntas_c <- dlgInput(paste("Cuantas preguntas quiere que le hagamos? Por defecto son 10"), Sys.info()["user"])$res
n_preguntas = as.numeric(as.character(n_preguntas_c))
if(is.na(n_preguntas) == T | is.null(n_preguntas) == T ){n_preguntas = 10}
if(!length(n_preguntas_c)){n_preguntas = 10}
sum(substr(n_preguntas,1,1) == "")

winDialog(type = "ok", message = paste("Numero de preguntas sera ",n_preguntas))

es_in_o_in_es_c <- dlgInput(paste("Quiere que le aparezcan palabras en espanol y opciones en ingles(clique 1), (o al contrario)(clique 2). Por defecto es 1"), Sys.info()["user"])$res
es_in_o_in_es = as.numeric(as.character(es_in_o_in_es_c))
if(is.na(es_in_o_in_es) == T|is.null(es_in_o_in_es) == T){es_in_o_in_es = 1}
if(!length(es_in_o_in_es_c)){es_in_o_in_es = 1}

elegido = ifelse(es_in_o_in_es == 1,"ESPANOL A INGLES","INGLES A ESPANOL")
winDialog(type = "ok", message = paste("Ha elegido",elegido))



pos_palabras_preg_df = data.frame()
for (i in 1:n_preguntas){
  numero_de_palabras = nrow(dic[1])
  pos_palabras_preg = sample.int(numero_de_palabras,3,replace = FALSE)
  pos_palabras_preg_df = rbind(pos_palabras_preg_df,pos_palabras_preg)
}


#######2. aleatorios de 3 posiciones para i recolocando en la pregunta
recolocar_df = data.frame()
for (i in 1:n_preguntas){
  recolocar = sample.int(3,3,replace = FALSE)# de 1 a 3 para recolocar en la pregunta y luego que no sea siepre la primera la correcta
  recolocar_df = rbind(recolocar_df,recolocar)
}


###########Y aqui empieza el programita de 10(o x) preguntas

acertado = 0
no_acertado = 0

for (i in 1:n_preguntas){

  if (es_in_o_in_es == 2){
    X = dic[pos_palabras_preg_df[i,1],1]
    Y = c(0,0,0)
    Y[recolocar_df[i,1]] = dic[pos_palabras_preg_df[i,1],2]# esta es la correcta, entonces en la pregunta la correcta es recolocar[1] 
    Y[recolocar_df[i,2]] = dic[pos_palabras_preg_df[i,2],2]
    Y[recolocar_df[i,3]] = dic[pos_palabras_preg_df[i,3],2]
  }
######Recoloco si lo quiere desde ingles
  if (es_in_o_in_es == 1){
    X = dic[pos_palabras_preg_df[i,1],2]
    Y = c(0,0,0)
    Y[recolocar_df[i,1]] = dic[pos_palabras_preg_df[i,1],1]# esta es la correcta, entonces en la pregunta la correcta es recolocar[1] 
    Y[recolocar_df[i,2]] = dic[pos_palabras_preg_df[i,2],1]
    Y[recolocar_df[i,3]] = dic[pos_palabras_preg_df[i,3],1] 
    }

respuesta_c <- dlgInput(paste("Teclea 1, 2 o 3. Indica la opcion con la traduccion correcta de: ", X, 
                             "; de la siguiente palabra;      opcion 1:",Y[1],",    opcion 2: ",Y[2],",    opcion 3: ",Y[3]), Sys.info()["user"])$res
respuesta = as.numeric(respuesta_c)
if(is.na(respuesta) == T|is.null(respuesta)==T){respuesta = 4}#si teclea mal, que cargue 4
if(!length(respuesta_c)){respuesta = 4}#si cancela, que cargue 4 y siga. Pero como esta dentro de un FOR, me hace un BREAK y sale. Por mi bien, pero se podria mejorar cambiando este BREAK a un NEXT
#print(respuesta)


correcto = recolocar_df[i,1]
if(respuesta == correcto){
  acertado = acertado+1
  b = "CORRECTA"
}else{
    no_acertado = no_acertado + 1
    b = "INCORRECTA"
    }

winDialog(type = "ok", message = paste("ES REPUESTA ",b,".DESEA CONTINUAR?"))  
  
  
}


porcentaje_acierto = (acertado / (acertado+no_acertado)) * 100
if(acertado == 0){porcentaje_acierto = 0}
terminar=winDialog(type = "ok", message=paste("Tus resultados son:  \\ Numero de aciertos:",acertado,"\\ Numero de errores:",no_acertado,"\\ Porcentaje de acierto:",round(porcentaje_acierto,2),"%"))


rm(pos_palabras_preg_df,recolocar_df)#elimino los df de memoria pq ya no se necesitan. Y cuando tenga que hacer un rbind, no se van uniendo desde el ultimo





