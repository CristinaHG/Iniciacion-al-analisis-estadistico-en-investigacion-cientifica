# Iniciación al análisis estadístico en investigación científica con RStudio - Modulo 2

# Ejercicio 1
# apartado a)
distancia<-c(350, 596,129, 330, 674,210, 390,455, 195, 547)
tiempo<-c(12,31,14,21,25,16,18,22,19,39)
drones<- cbind(distancia,tiempo)

# apartado b)
distancia_km<-distancia/1000
drones<- cbind(drones,distancia_km)

# apartado c)
tiempo_h<-tiempo/60
drones<- cbind(drones,tiempo_h)

# apartado d)
vel<-drones[,3]/drones[,4]
drones<- cbind(drones,vel)

# más veloz es el dron 1
which(vel==max(vel))
# menos veloz es el dron 3
which(vel==min(vel))

# apartado e)
tipo_dron<-ifelse(drones[,7]<1, "LENTO","RAPIDO")
drones<- cbind(drones,tipo_dron)

# apartado f)
drones_rapidos<-as.data.frame(drones[which(drones[,8]=="RAPIDO"),])

# apartado g)

factorCont<-function(velKm_h,dist_km){
  sqrt(as.numeric(velKm_h))+3*as.numeric(dist_km)+2.5
}

for(i in 1:nrow(drones_rapidos)){
  fac_cont[i]<-(factorCont(drones_rapidos[[7]][i],drones_rapidos[[5]][i]))
}

drones_rapidos<-cbind(drones_rapidos,fac_cont)


