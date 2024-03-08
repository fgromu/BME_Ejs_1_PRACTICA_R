rm(list=ls()) #limpiar
############################install.packages("dplyr")
library(dplyr)
#############################setwd("C:/Users/Lenovo/OneDrive/Escritorio/BNE/dat")



Ibex_data<-read.table("Ibex_data.csv",header=T,sep=",",stringsAsFactors = F,dec=".") #cargar
#str(Ibex_data) 
#summary(Ibex_data) 
#LINEA NUEVA
price_departure<-read.table("price_departures.csv",header=T,sep=",",stringsAsFactors = F,dec=".") #cargar

lista_empresas_id = unique(Ibex_data$X)
Tabla_resultados = matrix(0,11,length(lista_empresas_id))
rownames(Tabla_resultados) = c("Importe medio por operacion",
                                             "B medio por operacion",
                                             "B medio por euro invertido",
                                             "Beneficio acumulado",
                                             "% dias positivos",
                                             "% dias negativos",
                                             "Horquilla superior media",
                                             "Horquilla inferior media",
                                             "Stop profit objetivo",
                                             "Stop loss",
                                             "Numero de operaciones")
colnames(Tabla_resultados) = lista_empresas_id

Numero_empresas_revisa = length(lista_empresas_id)

media_todos_los_cierres = rep(0,Numero_empresas_revisa)#NUEVO
stop_profit = rep(0,Numero_empresas_revisa)#NUEVO
stop_loss = rep(0,Numero_empresas_revisa)#NUEVO
dinero_max = rep(0,Numero_empresas_revisa)#NUEVO
volumen_gloval = rep(0,Numero_empresas_revisa)#NUEVO

for(j in 1:Numero_empresas_revisa){

  aux_nombre_empresa = lista_empresas_id[j]
  aux_Ibex_data = filter(Ibex_data, Ibex_data$X == lista_empresas_id[j])#hago un df auxiliar para ir viendo la inversion por empresa

  media_todos_los_cierres[j] = (sum(aux_Ibex_data$close)) / (length(aux_Ibex_data$close))#NUEVO
  volumen_gloval[j] = (sum(aux_Ibex_data$vol)) / (length(aux_Ibex_data$vol))
  # el profesor dijo que es el 0,005 del volumen gloval. Pero si lo hago asi me sale que doy mucho dinero. Creo que quiere decir la media 
  dinero_max[j] = media_todos_los_cierres[j]*(volumen_gloval[j]*0.005)
  stop_profit[j] = quantile(aux_Ibex_data$high-aux_Ibex_data$open,c(0.3))
  stop_loss[j] = quantile(aux_Ibex_data$open-aux_Ibex_data$low,c(0.8))

  df_pd_aux = data.frame(price_departure$X,price_departure[j+1])
  a = colnames(df_pd_aux)
  colnames(df_pd_aux) = c("X.1",a[2])
  
  join = merge(aux_Ibex_data,df_pd_aux, by=c("X.1"))
  colnames(join) = c("X.1","X","close","high","low","open","ticker","vol","pd" )
  aux_Ibex_data = join
  aux_Ibex_data = na.omit(aux_Ibex_data)
  
  beneficio_acumado = rep(0,length(aux_Ibex_data$open))
  beneficio_por_mov = rep(0,length(aux_Ibex_data$open))
  HSM = rep(0,length(aux_Ibex_data$open))
  HIM = rep(0,length(aux_Ibex_data$open))
  contador = 0
  importe_operacion = rep(0,length(aux_Ibex_data$open))
  invertido_operacion = rep(0,length(aux_Ibex_data$open))

  for (i in 1: length(aux_Ibex_data$open)){

    
    if(length(aux_Ibex_data$open) >= 30 ) {
      if (aux_Ibex_data[i,9] >= 0.75){
        contador=contador+1
        HSM[contador] = aux_Ibex_data[i,4]-aux_Ibex_data[i,6]
        HIM[contador] = aux_Ibex_data[i,6]-aux_Ibex_data[i,5]      

        n_acciones_compro = dinero_max[j] / aux_Ibex_data$open[i]
        invertido = round(dinero_max[j],3)
        gasto_compra = round(invertido*0.0003,3)
        invertido_operacion[i]  =invertido

        if(aux_Ibex_data$low[i] < (aux_Ibex_data$open[i] - stop_loss[j])){
          precio_salir = (aux_Ibex_data$open[i] - stop_loss[j]) * n_acciones_compro
        }else if(aux_Ibex_data$high[i] > (aux_Ibex_data$open[i] + stop_profit[j])){
          precio_salir = (aux_Ibex_data$open[i] + stop_profit[j]) * n_acciones_compro
        }else{
          precio_salir = aux_Ibex_data$close[i] * n_acciones_compro 
        }
        precio_salir=round(precio_salir,3)
        gasto_venta = round(precio_salir*0.0003,3)

        beneficio_sinform = precio_salir-invertido-gasto_venta-gasto_compra
        beneficio = round(beneficio_sinform,3)

        if(contador == 1){
          beneficio_acumado[1] = beneficio
        }
        if(contador > 1){
          beneficio_acumado[contador] = beneficio_acumado[contador-1]+beneficio
        }
        
        beneficio_por_mov[contador] = beneficio
        importe_operacion[contador] = gasto_venta+gasto_compra
        
      }
    }
    
  }

  beneficio_por_mov = beneficio_por_mov[1:contador]
  beneficio_acumado = beneficio_acumado[1:contador]

  
  #B medio por operacion
  b_med_oper = sum(beneficio_por_mov )/ length(beneficio_por_mov)
  
  #B medio por euro invertido
  b_med_euro = sum(beneficio_por_mov)  /sum(invertido_operacion)
  
  #Beneficio acumulado
  beneficio_acumado_final = beneficio_acumado[length(beneficio_acumado)]
  
  #% dias positivos
  dias_positivos = length(which(beneficio_por_mov>=0)) / length(beneficio_por_mov)
  
  #dias negativos
  dias_negativos = length(which(beneficio_por_mov<0)) / length(beneficio_por_mov)

  #Horquilla superior media
  Horquilla_superior_media_sinform=sum(HSM)/contador
  Horquilla_superior_media=round(Horquilla_superior_media_sinform,3)
  
  #Horquilla inferior media
  Horquilla_inferior_media_sinform = sum(HIM) / contador
  Horquilla_inferior_media = round(Horquilla_inferior_media_sinform,3)
  
  #Numero de Operaciones
  numero_operaciones = contador
  
  #Importe medio operacion
  importe_operacion_1 = sum(importe_operacion) / contador
  
  b_med_euro_new = round(b_med_euro,5)
  beneficio_acumado_new = round(beneficio_acumado,5)
  beneficio_por_mov_new = round(beneficio_por_mov,5)
  b_med_oper_new = round(b_med_oper,5)
  beneficio_acumado_final_new = round(beneficio_acumado_final,5)
  dias_positivos_new = round(dias_positivos,5)
  dias_negativos_new = round(dias_negativos,5)
  Horquilla_superior_media_new = round(Horquilla_superior_media,5)
  Horquilla_inferior_media_new = round(Horquilla_inferior_media,5)
  numero_operaciones_new = round(numero_operaciones,5)
  stop_profit_new = round(stop_profit[j],5) 
  stop_loss_new = round(stop_loss[j],5)
  importe_operacion_new = round(importe_operacion_1,5)
  
  
  if(length(aux_Ibex_data$open) >= 30 ) {
    Tabla_resultados[1,j] = importe_operacion_new #"Importe medio por operacion",
    Tabla_resultados[2,j] = b_med_oper_new #"B medio por operacion",
    Tabla_resultados[3,j] = b_med_euro_new #"B medio por euro invertido",
    Tabla_resultados[4,j] = beneficio_acumado_final_new #"Beneficio acumulado",
    Tabla_resultados[5,j] = dias_positivos_new #"% dias positivos",
    Tabla_resultados[6,j] = dias_negativos_new #"% dias negativos",
    Tabla_resultados[7,j] = Horquilla_superior_media_new #"Horquilla superior media",
    Tabla_resultados[8,j] = Horquilla_inferior_media_new #"Horquilla inferior media",
    Tabla_resultados[9,j] = stop_profit_new#"Stop profit objetivo",
    Tabla_resultados[10,j] = stop_loss_new#"Stop loss",
    Tabla_resultados[11,j] = numero_operaciones_new#"Numero de operaciones")
    
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
    Tabla_resultados[8,j] = NA
    Tabla_resultados[9,j] = NA
    Tabla_resultados[10,j] = NA
    Tabla_resultados[11,j] = NA
    
  }

}
