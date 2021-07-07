install.packages("TransP")
#TransP Implementación de algoritmos de problemas de transporte#
library(TransP)
#Autor Somenath Sit#
 
             
            #RUTA1    RUTA2    RUTA3   RUTA4   RUTA5   RUTA6   RUTA7
#BODEGA A    2.115    2.462    1.134   3.226   1.330   9.101   3.668
#BODEGA B    2.029    1.916    1.141   2.933   1.492   6.456   3.756
#BODEGA C    2.573    2.030    1.155   3.002   1.235   6.374   4.015
#DEMAND    419.000 7770.000 3130.000 432.000 876.000 273.000 602.000

              #RUTA8    RUTA9   RUTA10   RUTA11  RUTA12   RUTA13   RUTA14
#BODEGA A     5.509    1.172    1.455    1.429   3.836    1.114    3.678
#BODEGA B     3.283    1.306    1.388    2.078   2.604    1.163    2.618
#BODEGA C     3.035    1.319    1.450    2.140   2.701    1.188    2.964
#DEMAND    2490.000 1735.000 2125.000 3035.000 847.000 5040.000 1541.000

          #RUTA F OFERTA
#BODEGA A      0  25193
#BODEGA B      0  20427
#BODEGA C      0  22470
#DEMAND    37775  68090


costo_matrix=data.frame(RUTA1=c(2.115,2.029,2.573,419),RUTA2=c(2.462,1.916,2.030,7770),RUTA3=c(1.134,1.141,1.155,3130),
                         RUTA4=c(3.226,2.933,3.002,432),RUTA5=c(1.330,1.492,1.235,876),RUTA6=c(9.101,6.456,6.374,273),
                         RUTA7=c(3.668,3.756,4.015,602),RUTA8=c(5.509,3.283,3.035,2490),RUTA9=c(1.172,1.306,1.319,1735),
                         RUTA10=c(1.455,1.388,1.45,2125),RUTA11=c(1.429,2.078,2.14,3035),RUTA12=c(3.836,2.604,2.701,847),
                         RUTA13=c(1.114,1.163,1.188,5040),RUTA14=c(3.678,2.618,2.964,1541),RUTAF=c(0,0,0,37775), SUPPLY=c(25193,20427,22470,68090)
                        ,row.names=c("BODEGA A ","BODEGA B","BODEGA C","DEMAND"))


costo_matrix #Una matriz de costos donde la última columna debe ser la oferta y la última fila debe ser la demanda. La matriz de entrada no debe tener valores perdidos (NA), de lo contrario, la función arrojará un error#
mincost(costo_matrix)#Esta función implementa el algoritmo de costo mínimo para resolver el problema de transporte y obtener una matriz de asignación optimizada#


mincost=function(ex_matrix){
  
  if(sum(is.na(ex_matrix))>0)
    stop("Your matrix has NA values")
  
  Demand=as.vector(ex_matrix[nrow(ex_matrix),-ncol(ex_matrix)])
  Supply=as.vector(ex_matrix[-nrow(ex_matrix),ncol(ex_matrix)])
  High_Values=max(ex_matrix) + 999999999
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  ex_matrix=Alloc_Matrix
  Alloc_Matrix[,]=0
  Total_Cost=0
  Total_alloc=0
  
  while(sum(Supply) != 0 & sum(Demand) != 0)
  {
    tc=which.min(apply(ex_matrix,MARGIN=2,min))  #column of minimum value
    tr=which.min(apply(ex_matrix,MARGIN=1,min))  #row of minimum value
    
    min_curr=min(Demand[tc],Supply[tr])
    
    Demand[tc]=Demand[tc] - min_curr
    Supply[tr]=Supply[tr] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    
    if(Demand[tc]==0)
    {
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else if(Demand[tc]==Supply[tr])
    {
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else{
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
    }
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost
  
  #Si la oferta y demanda no son iguales
  if(sum(Demand) != 0)
    output$Dummy_demand=sum(Demand)
  else if(sum(Supply) != 0)
    output$Dummy_supply=sum(Supply)
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  return(output)
}

ex_matrix=data.frame(RUTA1=c(2.115,2.029,2.573,419),RUTA2=c(2.462,1.916,2.030,7770),RUTA3=c(1.134,1.141,1.155,3130),
                     RUTA4=c(3.226,2.933,3.002,432),RUTA5=c(1.330,1.492,1.235,876),RUTA6=c(9.101,6.456,6.374,273),
                     RUTA7=c(3.668,3.756,4.015,602),RUTA8=c(5.509,3.283,3.035,2490),RUTA9=c(1.172,1.306,1.319,1735),
                     RUTA10=c(1.455,1.388,1.45,2125),RUTA11=c(1.429,2.078,2.14,3035),RUTA12=c(3.836,2.604,2.701,847),
                     RUTA13=c(1.114,1.163,1.188,5040),RUTA14=c(3.678,2.618,2.964,1541),RUTAF=c(0,0,0,37775), SUPPLY=c(25193,20427,22470,68090)
                     ,row.names=c("BODEGA A ","BODEGA B","BODEGA C","DEMAND"))

ex_matrix #Una matriz de costos donde la última columna debe ser la oferta y la última fila debe ser la demanda. La matriz de entrada no debe tener valores perdidos (NA), de lo contrario, la función arrojará un error#
nwc(ex_matrix)#Esta función implementa el algoritmo North-West Corner para resolver el problema de transporte mediante una matriz de costos optimizada y un costo total optimizado#

nwc=function(ex_matrix){
  
  if(sum(is.na(ex_matrix))>0)
    stop("Your matrix has NA values")
  
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  Alloc_Matrix[,]=0
  tr=1
  tc=1
  Total_Cost=0
  Total_alloc=0
  colnames(ex_matrix)[ncol(ex_matrix)]="Supply"
  while(sum(ex_matrix[nrow(ex_matrix),]) != 0 & sum(ex_matrix[,ncol(ex_matrix)]) != 0)
  {
    min_curr=min(ex_matrix[tr,ncol(ex_matrix)],ex_matrix[nrow(ex_matrix),tc])
    ex_matrix[tr,ncol(ex_matrix)]=ex_matrix[tr,ncol(ex_matrix)] - min_curr
    ex_matrix[nrow(ex_matrix),tc]=ex_matrix[nrow(ex_matrix),tc] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(ex_matrix[nrow(ex_matrix),tc]==0)
    {
      tc=tc+1
    }else if(ex_matrix[tr,ncol(ex_matrix)]==ex_matrix[nrow(ex_matrix),tc])
    {
      tr=tr+1
      tc=tc+1
    }else{
      tr=tr+1
    }
    ex_matrix[nrow(ex_matrix),ncol(ex_matrix)]=sum(ex_matrix$Supply[-nrow(ex_matrix)])
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost
  
  #Si la oferta y demanda no son iguales
  if(sum(ex_matrix[nrow(ex_matrix),]) != 0)
    output$Dummy_demand=sum(ex_matrix[nrow(ex_matrix),])
  else if(sum(ex_matrix[,ncol(ex_matrix)]) != 0)
    output$Dummy_supply=sum(ex_matrix[,ncol(ex_matrix)])
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  
  return(output)
}

