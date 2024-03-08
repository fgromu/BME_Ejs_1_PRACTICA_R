
rm(list = ls()) #limpiar
#install.package("dplyr")
#install.package("svDialogs")
library(dplyr)
library(svDialogs)
#setwd("C:/Users/Lenovo/OneDrive/Escritorio/BNE/dat")

Ibex_data <- read.table("Ibex_data.csv",header=T,sep=",",stringsAsFactors = F,dec=".") #cargar
price_departure <- read.table("price_departures.csv",header=T,sep=",",stringsAsFactors = F,dec=".") #cargar

lista_empresas_id = unique(Ibex_data$X)

n_preguntas_c = ZEL
n_preguntas_c <- dlgInput(paste("De la siguiente lista de empresas, ?Cual desea graficar?,
                                     A3TV, ABE, ABG, ABG.P_0, ABG.P_1, ACS, ACX, ACX_0, AENA, 
                                     AGS, ALT, AMS, ANA, ANA_0, BBVA, BKIA, BKIA_0, BKT, BME, 
                                     BTO_0, BTO_1, CABK, CAR, CIE, CIN, CLNX, COL, COL_0, CRI, 
                                     DIA, EBRO_0, EBRO_1, ELE, ELE_0, ENC, ENG, EVA, FAD, FCC, 
                                     FER, GRF, IAG, IBE, IBLA, IBR, IDR, ITX, JAZ, LOR, MAP, 
                                     MAP_0, MAS, MEL, MRL, MTS, NHH_0, NHH_1, NTGY, OHL, 
                                     POP, PRS_1, REE, REE_0, REP, SAB, SAN, SCYR_0, SCYR_1, 
                                     SGC, SGRE, SGRE_0, TEF, TEM, TL5, TRE, UNF, VIS, VIS_0, ZEL.,
                                     Por defecto es ZEL"), Sys.info()["user"])$res


if(is.na(n_preguntas_c) == T |is.null(n_preguntas_c) == T ){n_preguntas_c = "ZEL"}
if(!length(n_preguntas)){n_preguntas = ZEL}
if(any(lista_empresas_id == n_preguntas_c) == F){n_preguntas_c = "ZEL"}

winDialog(type = "ok", message=paste("Ha elegido graficar: ",n_preguntas_c))     
j = which(n_preguntas_c == lista_empresas_id)

aux_nombre_empresa = lista_empresas_id[j]
aux_Ibex_data = filter(Ibex_data, Ibex_data$X == lista_empresas_id[j])#hago un df auxiliar para ir viendo la inversion por empresa
df_pd_aux = data.frame(price_departure$X,price_departure[j+1])
a = colnames(df_pd_aux)
colnames(df_pd_aux) = c("X.1",a[2])

join = merge(aux_Ibex_data,df_pd_aux, by=c("X.1"))
colnames(join) = c("X.1","X","close","high","low","open","ticker","vol","pd" )
aux_Ibex_data = join
aux_Ibex_data = na.omit(aux_Ibex_data)

beneficio_acumado = rep(0,length(aux_Ibex_data$open))
beneficio_por_mov = rep(0,length(aux_Ibex_data$open))
contador = 0

for (i in 1: length(aux_Ibex_data$open)){

  if(length(aux_Ibex_data$open) >= 30 ) {
    if (aux_Ibex_data[i,9] >= 0.75){
      contador = contador+1
      n_acciones_compro = round(30000/aux_Ibex_data$open[i],3)
      invierto = round(30000,3)
      gasto_compra = round(invierto*0.0003,3)

      if(aux_Ibex_data$low[i] < (aux_Ibex_data$open[i] - 0.1)){
        precio_salir=(aux_Ibex_data$open[i] - 0.1)*n_acciones_compro
      }else if(aux_Ibex_data$high[i] > (aux_Ibex_data$open[i] + 0.03)){
        precio_salir = (aux_Ibex_data$open[i] + 0.03) * n_acciones_compro
      }else{
        precio_salir = aux_Ibex_data$close[i] * n_acciones_compro 
      }

            precio_salir = round(precio_salir,3)#NUEVO
      gasto_venta = round(precio_salir*0.0003,3)
      beneficio_sinform = precio_salir-invierto-gasto_venta-gasto_compra
      beneficio = round(beneficio_sinform,3)

      if(contador == 1){
        beneficio_acumado[1] = beneficio
      }
      if(contador > 1){
        beneficio_acumado[contador] = beneficio_acumado[contador-1]+beneficio
      }
      beneficio_por_mov[contador] = beneficio
      }
  }
  
}

beneficio_por_mov = beneficio_por_mov[1:contador]
beneficio_acumado = beneficio_acumado[1:contador]

nombre_a_plotear_querido = n_preguntas_c
plot(beneficio_acumado,type = "l",main=nombre_a_plotear_querido,xlab = "Interaciones",ylab = "Beneficio")

