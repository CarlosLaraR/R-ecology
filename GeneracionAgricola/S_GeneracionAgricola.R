

#### Datos de entrada ####

area<-10 #Ha
pac<- 250 * area # €/Ha de subvención

cultivar<-c("lechoso","castellano","fardon")
variedad<-c("tradicional","tradicional","comercial")
rendimiento<-c(1.8,2,2.2)
precio<-c(800,725,700)
siembra<-c("temprana","intermedia","taria")
rabia<-c("alta","media","baja")
hierbas<-c("baja","media","alta")


#Tipo de semilla
semilla.df<-data.frame(cultivar,variedad,rendimiento,
                       precio,siembra,rabia,hierbas)


#Costes por Ha
costes<-read.table("costes_ha.txt",header=T, sep="\t")

regadio<- 50 #€/Ha

semilla<-40 #€/Ha


#### Producción ####

#Incremento producción convencional
convencional<-0.08 #8%

#Secano conservacion
produccion.secano<-semilla.df$rendimiento*area #Toneladas
produccion.regadio<-(semilla.df$rendimiento*1.05)*area
produccion.secano.convencional<-produccion.secano+(produccion.secano*0.08) #Toneladas
produccion.regadio.convencional<-produccion.regadio+(produccion.regadio*0.08)

ingreso.secano<-produccion.secano*semilla.df$precio+pac
ingreso.regadio<-produccion.regadio*semilla.df$precio+pac
ingreso.secano.convencional<-produccion.secano.convencional*semilla.df$precio+pac
ingreso.regadio.convencional<-produccion.regadio.convencional*semilla.df$precio+pac


#### Beneficio ####
costes.secano<- sum(costes$Conservacion)*area
costes.regadio<- sum(costes$Conservacion)*area + (regadio*area)
costes.secano  <- c(rep(costes.secano,2),costes.secano+semilla*area) #coste semilla comercial
costes.regadio  <- c(rep(costes.regadio,2),costes.regadio+semilla*area) #Coste semilla comercial


costes.secano.convencional <- sum(costes$Convencional)*area
costes.regadio.convencional <- sum(costes$Convencional)*area+ (regadio*area)
costes.secano.convencional  <- c(rep(costes.secano.convencional,2),
                                 costes.secano.convencional+semilla*area) #coste semilla comercial
costes.regadio.convencional  <- c(rep(costes.regadio.convencional,2),
                                  costes.regadio.convencional+semilla*area) #Coste semilla comercial


beneficio.secano <-  ingreso.secano- costes.secano
beneficio.regadio<-  ingreso.regadio- costes.regadio


beneficio.secano.convencional<-ingreso.secano.convencional-costes.secano.convencional
beneficio.regadio.convencional<-ingreso.regadio.convencional-costes.regadio.convencional

#### Tabla resultados ####

costes.total<-rbind(costes.secano,
              costes.secano.convencional,
              costes.regadio,
              costes.regadio.convencional)
colnames(costes.total)<- semilla.df$cultiva


produccion.total<-rbind(produccion.secano,
                    produccion.secano.convencional,
                    produccion.regadio,
                    produccion.regadio.convencional)
colnames(produccion.total)<- semilla.df$cultiva




beneficio<-rbind(beneficio.secano,
                      beneficio.secano.convencional,
                      beneficio.regadio,
                      beneficio.regadio.convencional)

colnames(beneficio)<- semilla.df$cultivar
              
#### Eventos ####

df.secano<-data.frame(clima.anual=NULL,
                      plaga.anual=NULL,
                      helada.anual=NULL,
                      politica.anual=NULL,
                      produccion=NULL,
                      costes=NULL,
                      beneficio=NULL,
                      año=NULL)


df.secano.convencional<-data.frame(clima.anual=NULL,
                                   plaga.anual=NULL,
                                   helada.anual=NULL,
                                   politica.anual=NULL,
                                   produccion=NULL,
                                   costes=NULL,
                                   beneficio=NULL,
                                   año=NULL)

df.regadio<-data.frame(clima.anual=NULL,
                       plaga.anual=NULL,
                       helada.anual=NULL,
                       politica.anual=NULL,
                       produccion=NULL,
                       costes=NULL,
                       beneficio=NULL,
                       año=NULL)


df.regadio.convencional<-data.frame(clima.anual=NULL,
                                    plaga.anual=NULL,
                                    helada.anual=NULL,
                                    politica.anual=NULL,
                                    produccion=NULL,
                                    costes=NULL,
                                    beneficio=NULL,
                                    año=NULL)

for(i in seq(1:10)) {


# Vector de palabras
clima <- c("humedo", "normal", "seco")
# Vector de probabilidades asociadas a las palabras
probabilidades <- c(0.2, 0.5, 0.3)
# Extraer una palabra basada en las probabilidades
clima.anual <- sample(clima, size = 1, prob = probabilidades)


####  Clima seco

if (clima.anual == "seco"){ #Penaliza produccion 
  
  produccion.secano.seco<-produccion.secano*0.7 #25% pérdida
  produccion.regadio.seco<-produccion.regadio*0.9 #15% pérdida
  produccion.secano.convencional.seco<-produccion.secano.convencional*0.6
  produccion.regadio.convencional.seco<-produccion.regadio.convencional*0.8
  
  plaga.anual <- sample(c("plaga.si","plaga.no"), size = 1, prob = c(0.75,0.25))
  
  if (plaga.anual == "plaga.si"){ #Penaliza produccion 10% convencional
  
    produccion.secano.seco<-produccion.secano.seco
    produccion.regadio.seco<-produccion.regadio.seco
    produccion.secano.convencional.seco<-produccion.secano.convencional.seco*0.8
    produccion.regadio.convencional.seco<-produccion.regadio.convencional.seco*0.8
    
    costes.secano.seco<-costes.secano 
    costes.regadio.seco  <- costes.regadio
    costes.secano.convencional.seco  <-  costes.secano.convencional*1.05
    costes.regadio.convencional.seco  <-  costes.regadio.convencional *1.05
    
  }
  
  helada.anual <- sample(c("helada.si","helada.no"), size = 1, prob = c(0.4,0.6))
  
  if (helada.anual == "helada.si"){ #Penaliza produccion fardon 25% 
    
    produccion.secano.seco [3]<-produccion.secano.seco  [3] * 0.65
    produccion.regadio.seco[3]<-produccion.regadio.seco [3] * 0.65
    produccion.secano.convencional.seco[3]<-produccion.secano.convencional.seco [3] * 0.75
    produccion.regadio.convencional.seco[3]<-produccion.regadio.convencional.seco [3] * 0.75
    
  }
  
  politica.anual <- sample(c("politica.normal","politica.crisis"), size = 1, prob = c(0.7,0.3))
  
  if (politica.anual == "politica.crisis"){ #Incremento costes insumos 75% 
    
    costes.secano.seco<-costes.secano.seco * 1.25 #25% incremento 
    costes.regadio.seco  <- costes.regadio.seco* 1.25#25% incremento 
    costes.secano.convencional.seco  <-  costes.secano.convencional* 1.75
    costes.regadio.convencional.seco  <-  costes.regadio.convencional *1.75
    
  }
  
  
  ####Tabla Resultados
  ingreso.secano.seco<-produccion.secano.seco*semilla.df$precio+pac
  ingreso.regadio.seco<<-produccion.regadio.seco*semilla.df$precio+pac
  ingreso.secano.convencional.seco<<-produccion.secano.convencional.seco*semilla.df$precio+pac
  ingreso.regadio.convencional.seco<<-produccion.regadio.convencional.seco*semilla.df$precio+pac
  
  
  # Beneficio 

  beneficio.secano.seco <-  ingreso.secano.seco- costes.secano.seco
  beneficio.regadio.seco<-  ingreso.regadio.seco- costes.regadio.seco
  
  beneficio.secano.convencional.seco<-ingreso.secano.convencional.seco-costes.secano.convencional.seco
  beneficio.regadio.convencional.seco<-ingreso.regadio.convencional.seco-costes.regadio.convencional.seco
  
  #Tabla resultados
  costes.total.seco<-rbind(costes.secano.seco,
                      costes.secano.convencional.seco,
                      costes.regadio.seco,
                      costes.regadio.convencional.seco)
  colnames(costes.total.seco)<- semilla.df$cultiva
  
  
  produccion.total.seco<-rbind(produccion.secano.seco,
                          produccion.secano.convencional.seco,
                          produccion.regadio.seco,
                          produccion.regadio.convencional.seco)
  colnames(produccion.total.seco)<- semilla.df$cultiva
  
  
  
  
  beneficio.seco<-rbind(beneficio.secano.seco,
                   beneficio.secano.convencional.seco,
                   beneficio.regadio.seco,
                   beneficio.regadio.convencional.seco)
  
  colnames(beneficio.seco)<- semilla.df$cultivar
  

  df.secano<-rbind(df.secano, 
                   data.frame(año=rep(i,3),
                              clima.anual,
                              plaga.anual,
                              helada.anual,
                              politica.anual,
                              produccion=produccion.total.seco[1,],
                              costes=costes.total.seco[1,],
                              beneficio=beneficio.seco[1,]))
  
  
  df.secano.convencional<-rbind(df.secano.convencional, 
                                data.frame(año=rep(i,3),
                                           clima.anual,
                                           plaga.anual,
                                           helada.anual,
                                           politica.anual,
                                           produccion=produccion.total.seco[2,],
                                           costes=costes.total.seco[2,],
                                           beneficio=beneficio.seco[2,]))
    
  df.regadio<-rbind(df.regadio, 
                    data.frame(año=rep(i,3),
                               clima.anual,
                               plaga.anual,
                               helada.anual,
                               politica.anual,
                               produccion=produccion.total.seco[3,],
                               costes=costes.total.seco[3,],
                               beneficio=beneficio.seco[3,]))
  
  df.regadio.convencional<-rbind(df.regadio.convencional, 
                                data.frame(año=rep(i,3),
                                           clima.anual,
                                           plaga.anual,
                                           helada.anual,
                                           politica.anual,
                                           produccion=produccion.total.seco[4,],
                                           costes=costes.total.seco[4,],
                                           beneficio=beneficio.seco[4,]))
  
  }

#### Clima humedo

if (clima.anual == "humedo"){ #Penaliza produccion 
  
  produccion.secano.humedo<-produccion.secano*1.25 #25% incremento
  produccion.regadio.humedo<-produccion.regadio *1.1 #10% incremento
  produccion.secano.convencional.humedo<-produccion.secano.convencional*1.25 #25% incremento
  produccion.regadio.convencional.humedo<-produccion.regadio.convencional*1.1 #105 incremento
  
  plaga.anual <- sample(c("plaga.si","plaga.no"), size = 1, prob = c(0.75,0.25))
  
  if (plaga.anual == "plaga.si"){ #Penaliza produccion variedades tradicionales y riego
    
    produccion.secano.humedo [1:2]<-c(produccion.secano.humedo[1]*0.85, produccion.secano.humedo[2]*0.95)
    produccion.regadio.humedo[1:2]<-c(produccion.regadio.humedo[1]*0.8,produccion.regadio.humedo[2]*0.9)
    produccion.secano.convencional.humedo[1:2]<-c(produccion.secano.convencional.humedo[1]*0.8,produccion.secano.convencional.humedo[2]*0.9)
    produccion.regadio.convencional.humedo[1:2]<-c(produccion.regadio.convencional.humedo[1]*0.75,produccion.regadio.convencional.humedo[2]*0.85)
    
    costes.secano.humedo<-costes.secano *1.05
    costes.regadio.humedo  <- costes.regadio*1.05
    costes.secano.convencional.humedo  <-  costes.secano.convencional*1.05
    costes.regadio.convencional.humedo  <-  costes.regadio.convencional *1.05
    
  }
  
  helada.anual <- sample(c("helada.si","helada.no"), size = 1, prob = c(0.4,0.6))
  
  if (helada.anual == "helada.si"){ #Penaliza produccion fardon 25% 
    
    produccion.secano.humedo [3]<-produccion.secano.humedo  [3] * 0.65
    produccion.regadio.humedo[3]<-produccion.regadio.humedo [3] * 0.65
    produccion.secano.convencional.humedo[3]<-produccion.secano.convencional.humedo [3] * 0.75
    produccion.regadio.convencional.humedo[3]<-produccion.regadio.convencional.humedo [3] * 0.75
    
  }
  
  politica.anual <- sample(c("politica.normal","politica.crisis"), size = 1, prob = c(0.7,0.3))
  
  if (politica.anual == "politica.crisis"){ #Incremento costes insumos 75% 
    
    costes.secano.humedo<-costes.secano.humedo * 1.25 #25% incremento 
    costes.regadio.humedo  <- costes.regadio.humedo* 1.25#25% incremento 
    costes.secano.convencional.humedo  <-  costes.secano.convencional* 1.75
    costes.regadio.convencional.humedo  <-  costes.regadio.convencional *1.75
    
  }
  
  
  ####Tabla Resultados
  ingreso.secano.humedo<-produccion.secano.humedo*semilla.df$precio+pac
  ingreso.regadio.humedo<<-produccion.regadio.humedo*semilla.df$precio+pac
  ingreso.secano.convencional.humedo<<-produccion.secano.convencional.humedo*semilla.df$precio+pac
  ingreso.regadio.convencional.humedo<<-produccion.regadio.convencional.humedo*semilla.df$precio+pac
  
  
  # Beneficio 
  
  beneficio.secano.humedo <-  ingreso.secano.humedo- costes.secano.humedo
  beneficio.regadio.humedo<-  ingreso.regadio.humedo- costes.regadio.humedo
  
  beneficio.secano.convencional.humedo<-ingreso.secano.convencional.humedo-costes.secano.convencional.humedo
  beneficio.regadio.convencional.humedo<-ingreso.regadio.convencional.humedo-costes.regadio.convencional.humedo
  
  #Tabla resultados
  costes.total.humedo<-rbind(costes.secano.humedo,
                           costes.secano.convencional.humedo,
                           costes.regadio.humedo,
                           costes.regadio.convencional.humedo)
  colnames(costes.total.humedo)<- semilla.df$cultiva
  
  
  produccion.total.humedo<-rbind(produccion.secano.humedo,
                               produccion.secano.convencional.humedo,
                               produccion.regadio.humedo,
                               produccion.regadio.convencional.humedo)
  colnames(produccion.total.humedo)<- semilla.df$cultiva
  
  
  
  
  beneficio.humedo<-rbind(beneficio.secano.humedo,
                        beneficio.secano.convencional.humedo,
                        beneficio.regadio.humedo,
                        beneficio.regadio.convencional.humedo)
  
  colnames(beneficio.humedo)<- semilla.df$cultivar
  
  
  df.secano<-rbind(df.secano, 
                   data.frame(año=rep(i,3),
                              clima.anual,
                              plaga.anual,
                              helada.anual,
                              politica.anual,
                              produccion=produccion.total.humedo[1,],
                              costes=costes.total.humedo[1,],
                              beneficio=beneficio.humedo[1,]))
  
  
  df.secano.convencional<-rbind(df.secano.convencional, 
                                data.frame(año=rep(i,3),
                                           clima.anual,
                                           plaga.anual,
                                           helada.anual,
                                           politica.anual,
                                           produccion=produccion.total.humedo[2,],
                                           costes=costes.total.humedo[2,],
                                           beneficio=beneficio.humedo[2,]))
  
  df.regadio<-rbind(df.regadio, 
                    data.frame(año=rep(i,3),
                               clima.anual,
                               plaga.anual,
                               helada.anual,
                               politica.anual,
                               produccion=produccion.total.humedo[3,],
                               costes=costes.total.humedo[3,],
                               beneficio=beneficio.humedo[3,]))
  
  df.regadio.convencional<-rbind(df.regadio.convencional, 
                                 data.frame(año=rep(i,3),
                                            clima.anual,
                                            plaga.anual,
                                            helada.anual,
                                            politica.anual,
                                            produccion=produccion.total.humedo[4,],
                                            costes=costes.total.humedo[4,],
                                            beneficio=beneficio.humedo[4,]))
  
  
}



#### Clima normal

if (clima.anual == "normal"){ #Penaliza produccion 
  
  produccion.secano.normal<-produccion.secano
  produccion.regadio.normal<-produccion.regadio
  produccion.secano.convencional.normal<-produccion.secano.convencional
  produccion.regadio.convencional.normal<-produccion.regadio.convencional
  
  plaga.anual <- sample(c("plaga.normal","plaga.normal"), size = 1, prob = c(0.75,0.25))
  
  
  if (plaga.anual == "plaga.normal"){ #Penaliza produccion 10% convencional
    
    produccion.secano.normal<-produccion.secano.normal
    produccion.regadio.normal<-produccion.regadio.normal
    produccion.secano.convencional.normal<-produccion.secano.convencional.normal
    produccion.regadio.convencional.normal<-produccion.regadio.convencional.normal
    
    costes.secano.normal<-costes.secano 
    costes.regadio.normal  <- costes.regadio
    costes.secano.convencional.normal  <-  costes.secano.convencional
    costes.regadio.convencional.normal  <-  costes.regadio.convencional
  }
  
  
  helada.anual <- sample(c("helada.si","helada.no"), size = 1, prob = c(0.4,0.6))
  
  if (helada.anual == "helada.si"){ #Penaliza produccion fardon 25% 
    
    produccion.secano.normal [3]<-produccion.secano.normal  [3] * 0.65
    produccion.regadio.normal[3]<-produccion.regadio.normal [3] * 0.65
    produccion.secano.convencional.normal[3]<-produccion.secano.convencional.normal [3] * 0.75
    produccion.regadio.convencional.normal[3]<-produccion.regadio.convencional.normal [3] * 0.75
    
  }
  
  politica.anual <- sample(c("politica.normal","politica.crisis"), size = 1, prob = c(0.7,0.3))
  
  if (politica.anual == "politica.crisis"){ #Incremento costes insumos 75% 
    
    costes.secano.normal<-costes.secano.normal * 1.25 #25% incremento 
    costes.regadio.normal  <- costes.regadio.normal* 1.25#25% incremento 
    costes.secano.convencional.normal  <-  costes.secano.convencional* 1.75
    costes.regadio.convencional.normal  <-  costes.regadio.convencional *1.75
    
  }
  
  
  ####Tabla Resultados
  ingreso.secano.normal<-produccion.secano.normal*semilla.df$precio+pac
  ingreso.regadio.normal<<-produccion.regadio.normal*semilla.df$precio+pac
  ingreso.secano.convencional.normal<<-produccion.secano.convencional.normal*semilla.df$precio+pac
  ingreso.regadio.convencional.normal<<-produccion.regadio.convencional.normal*semilla.df$precio+pac
  
  
  # Beneficio 
  
  beneficio.secano.normal <-  ingreso.secano.normal- costes.secano.normal
  beneficio.regadio.normal<-  ingreso.regadio.normal- costes.regadio.normal
  
  beneficio.secano.convencional.normal<-ingreso.secano.convencional.normal-costes.secano.convencional.normal
  beneficio.regadio.convencional.normal<-ingreso.regadio.convencional.normal-costes.regadio.convencional.normal
  
  #Tabla resultados
  costes.total.normal<-rbind(costes.secano.normal,
                             costes.secano.convencional.normal,
                             costes.regadio.normal,
                             costes.regadio.convencional.normal)
  colnames(costes.total.normal)<- semilla.df$cultiva
  
  
  produccion.total.normal<-rbind(produccion.secano.normal,
                                 produccion.secano.convencional.normal,
                                 produccion.regadio.normal,
                                 produccion.regadio.convencional.normal)
  colnames(produccion.total.normal)<- semilla.df$cultiva
  
  
  
  
  beneficio.normal<-rbind(beneficio.secano.normal,
                          beneficio.secano.convencional.normal,
                          beneficio.regadio.normal,
                          beneficio.regadio.convencional.normal)
  
  colnames(beneficio.normal)<- semilla.df$cultivar
  
  
  df.secano<-rbind(df.secano, 
                   data.frame(año=rep(i,3),
                              clima.anual,
                              plaga.anual,
                              helada.anual,
                              politica.anual,
                              produccion=produccion.total.normal[1,],
                              costes=costes.total.normal[1,],
                              beneficio=beneficio.normal[1,]))
  
  
  df.secano.convencional<-rbind(df.secano.convencional, 
                                data.frame(año=rep(i,3),
                                           clima.anual,
                                           plaga.anual,
                                           helada.anual,
                                           politica.anual,
                                           produccion=produccion.total.normal[2,],
                                           costes=costes.total.normal[2,],
                                           beneficio=beneficio.normal[2,]))
  
  df.regadio<-rbind(df.regadio, 
                    data.frame(año=rep(i,3),
                               clima.anual,
                               plaga.anual,
                               helada.anual,
                               politica.anual,
                               produccion=produccion.total.normal[3,],
                               costes=costes.total.normal[3,],
                               beneficio=beneficio.normal[3,]))
  
  df.regadio.convencional<-rbind(df.regadio.convencional, 
                                 data.frame(año=rep(i,3),
                                            clima.anual,
                                            plaga.anual,
                                            helada.anual,
                                            politica.anual,
                                            produccion=produccion.total.normal[4,],
                                            costes=costes.total.normal[4,],
                                            beneficio=beneficio.normal[4,]))
  
  
}

} #Fin bucle for (i)



write.table(df.secano, "secano_conservacion.txt",
            row.names=T, sep="\t")


write.table(df.regadio, "regadio_conservacion.txt",
            row.names=T, sep="\t")

write.table(df.secano.convencional, "secano_convencional.txt",
            row.names=T, sep="\t")

write.table(df.regadio.convencional, "regadio_convencional.txt",
            row.names=T, sep="\t")
