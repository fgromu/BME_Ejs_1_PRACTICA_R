rm(list=ls()) #limpiar
##setwd("C:/Users/Lenovo/OneDrive/Escritorio/BNE/dat")
#install.packages("dplyr")
library(dplyr)


Ibex_data <- read.table("Ibex_data.csv",header=T,sep=",",stringsAsFactors = F,dec=".") #cargar

lista_empresas_id = unique(Ibex_data$X)
Tabla_resultados = matrix(0,7,length(lista_empresas_id))
rownames(Tabla_resultados) = c("B medio por operacion",
                                             "Beneficio acumulado",
                                             "% dias positivos",
                                             "%dias negativos",
                                             "Horquilla superior media",
                                             "Horquilla inferior media",
                                             "Numero de Operaciones")
colnames(Tabla_resultados) = lista_empresas_id

Numero_empresas_revisa = length(lista_empresas_id)

for(j in 1:Numero_empresas_revisa){
  
  aux_nombre_empresa = lista_empresas_id[j]
  aux_Ibex_data = filter(Ibex_data, Ibex_data$X == lista_empresas_id[j])#hago un df auxiliar para ir viendo la inversion por empresa

  beneficio_acumado = rep(0,length(aux_Ibex_data$open))
  beneficio_por_mov = rep(0,length(aux_Ibex_data$open))
  positivo_negativo = rep(0,length(aux_Ibex_data$open))
  que_paso = rep(0,length(aux_Ibex_data$open))
  HSM = rep(0,length(aux_Ibex_data$open))
  HIM = rep(0,length(aux_Ibex_data$open))
  contador = 0
  
  for (i in 1: length(aux_Ibex_data$open)){
    
    if(length(aux_Ibex_data$open) >= 30){
      contador = contador + 1
      HSM[contador] = aux_Ibex_data[i,4] - aux_Ibex_data[i,6]
      HIM[contador] = aux_Ibex_data[i,6] - aux_Ibex_data[i,5]
      
      n_acciones_compro = round(30000/aux_Ibex_data$open[i],3)
      invierto = round(30000,3)
      gasto_compra = round(invierto*0.0003,3)

      if(aux_Ibex_data$low[i] < (aux_Ibex_data$open[i] - 0.1)){
        precio_salir=(aux_Ibex_data$open[i] - 0.1) * n_acciones_compro
      }else if(aux_Ibex_data$high[i] > (aux_Ibex_data$open[i] + 0.03)){
        precio_salir = (aux_Ibex_data$open[i] + 0.03) * n_acciones_compro
      }else{
        precio_salir = aux_Ibex_data$close[i] * n_acciones_compro 
      }

      precio_salir = round(precio_salir,3)
      gasto_venta = round(precio_salir * 0.0003,3)
      beneficio_sinform = precio_salir - invierto - gasto_venta - gasto_compra
      beneficio = round(beneficio_sinform,3)
      if(contador == 1){beneficio_acumado[1]=beneficio} #calculo el beneficio acumulado a partir de 30000 de primera inversion
      if(contador > 1){
        beneficio_acumado[contador] = beneficio_acumado[contador-1] + beneficio
      }
      beneficio_por_mov[contador] = beneficio

      
      
    }   
  }
  
  beneficio_por_mov = beneficio_por_mov[1:contador]
  beneficio_acumado = beneficio_acumado[1:contador]
  
  #B medio por operacion
  b_med_oper = sum(beneficio_por_mov) / length(beneficio_por_mov)
  
  #Beneficio acumulado
  beneficio_acumado_final = beneficio_acumado[length(beneficio_acumado)]
  
  #% dias positivos
  dias_positivos = length(which(beneficio_por_mov>=0))/length(beneficio_por_mov)
  
  #dias negativos
  dias_negativos = length(which(beneficio_por_mov<0))/length(beneficio_por_mov)

  #Horquilla superior media
  Horquilla_superior_media_sinform = sum(HSM)/length(HSM)
  Horquilla_superior_media = round(Horquilla_superior_media_sinform,3)
  
  #Horquilla inferior media
  Horquilla_inferior_media_sinform = sum(HIM)/length(HIM)
  Horquilla_inferior_media = round(Horquilla_inferior_media_sinform,3)
  
  #Numero de Operaciones
  numero_operaciones = nrow(aux_Ibex_data)
  
  
  
  beneficio_acumado_new = round(beneficio_acumado,3)
  beneficio_por_mov_new = round(beneficio_por_mov,3)
  b_med_oper_new = round(b_med_oper,3)
  beneficio_acumado_final_new = round(beneficio_acumado_final,3)
  dias_positivos_new = round(dias_positivos,3)
  dias_negativos_new = round(dias_negativos,3)
  Horquilla_superior_media_new = round(Horquilla_superior_media,3)
  Horquilla_inferior_media_new = round(Horquilla_inferior_media,3)
  numero_operaciones_new = round(numero_operaciones,3)
 
  if(length(aux_Ibex_data$open) >= 30 ) { 
    Tabla_resultados[1,j] = b_med_oper_new
    Tabla_resultados[2,j] = beneficio_acumado_final_new
    Tabla_resultados[3,j] = dias_positivos_new
    Tabla_resultados[4,j] = dias_negativos_new
    Tabla_resultados[5,j] = Horquilla_superior_media_new
    Tabla_resultados[6,j] = Horquilla_inferior_media_new
    Tabla_resultados[7,j] = numero_operaciones_new
  
    nombre_a_plotear_querido=lista_empresas_id[j]
    plot(beneficio_acumado_new,type = "l",main=nombre_a_plotear_querido,xlab = "Interaciones",ylab = "Beneficio")
  }
  else{
    Tabla_resultados[1,j] = NA
    Tabla_resultados[2,j] = NA
    Tabla_resultados[3,j] = NA
    Tabla_resultados[4,j] = NA
    Tabla_resultados[5,j] = NA
    Tabla_resultados[6,j] = NA
    Tabla_resultados[7,j] = NA
  }
}
