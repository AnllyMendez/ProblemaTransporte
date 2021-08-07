install.packages("lpSolve")
#Resuelve problemas de programación lineal y entera#
library(lpSolve) 

#Matriz de costos para asignar cada trabajador a cada puesto#
cost<-(as.matrix(Basecostos)) 
cost

#Función para calcular el costo mínimo de asignación#
lp.assign(cost)

#Asignación de cada trabajador a cada puesto#
lp.assign(cost)$solution


?barplot
par(mfrow=c(1,1),bg="white",oma=c(0,3,0,0),mar=c(2,2,2,2))
R1<-cbind(cost)
R1

#Gráfico de costos de asignación#
barplot(R1, beside=T,horiz=T,
        main="Costos de Asignación de trabajador a puesto")

