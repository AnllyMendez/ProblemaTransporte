?barplot
par(mfrow=c(1,1),bg="white",oma=c(0,3,0,0),mar=c(2,2,2,2))

#Costos de transporte de cada bodega a ruta
R1<-c(2.115,2.029,2.573)
R2<-c(2.462,1.916,2.030)
R3<-c(1.134,1.141,1.155)
R4<-c(3.226,2.933,3.002)
R5<-c(1.330,1.492,1.235)
R6<-c(9.101,6.456,6.374)
R7<-c(3.668,3.756,4.015)
R8<-c(5.509,3.283,3.035)
R9<-c(1.172,1.306,1.319)
R10<-c(1.455,1.388,1.45)
R11<-c(1.429,2.078,2.14)
R12<-c(3.836,2.604,2.701)
R13<-c(1.114,1.163,1.188)
R14<-c(3.678,2.618,2.964)

RS.CON.Z<-cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14)
RS.CON.Z

rownames(RS.CON.Z)<-c("B1","B2","B3")

barplot(RS.CON.Z, beside=T, horiz=T,
        col=c("blue","gray","yellow"),
        main="Costos de Transporte (Cantidades en Quetzales)")

legend(4,14,rownames(RS.CON.Z),fill=c("blue","gray","yellow"),bg="white")

par(mfrow=c(2,2))

#Unidades a enviar actualidad
R1<-c(0,0,419)
R2<-c(7770,0,0)
R3<-c(0,3130,0)
R4<-c(432,0,0)
R5<-c(0,876,0)
R6<-c(273,0,0)
R7<-c(0,0,602)
R8<-c(2490,0,0)
R9<-c(0,1735,0)
R10<-c(2125,0,0)
R11<-c(0,0,3035)
R12<-c(847,0,0)
R13<-c(0,5040,0)
R14<-c(1541,0,0)


RS.CON.w<-cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14)
RS.CON.w

barplot(RS.CON.w, beside=T, hori=T,legend=c("B1","B2","B3"),
        
        col=c("blue","yellow","red4"),
        main="Unidades a enviar (Actualidad)")

#Unidades a enviar por método de costo mínimo
R1<-c(0,0,419)
R2<-c(0,0,7770)
R3<-c(0,3130,0)
R4<-c(0,0,432)
R5<-c(0,0,876)
R6<-c(0,0,273)
R7<-c(0,0,602)
R8<-c(0,0,2490)
R9<-c(0,0,1735)
R10<-c(0,0,2125)
R11<-c(0,0,3035)
R12<-c(0,0,847)
R13<-c(0,4715,325)
R14<-c(0,0,1541)
R15<-c(25193,12582,0)


RS.CON.w<-cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15)
RS.CON.w

barplot(RS.CON.w, beside=T, hori=T,legend=c("B1","B2","B3"),
        
        col=c("blue","yellow","red4"),
        main="Unidades a enviar (Método costo mínimo)")


#Unidades a enviar por método de esquina noroeste
R1<-c(419,0,0)
R2<-c(7770,0,0)
R3<-c(3130,0,0)
R4<-c(432,0,0)
R5<-c(876,0,0)
R6<-c(273,0,0)
R7<-c(602,0,0)
R8<-c(2490,0,0)
R9<-c(1735,0,0)
R10<-c(2125,0,0)
R11<-c(3035,0,0)
R12<-c(847,0,0)
R13<-c(1459,3581,0)
R14<-c(0,1541,0)
R15<-c(0,15305,22470)


RS.CON.w<-cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15)
RS.CON.w

barplot(RS.CON.w, beside=T, hori=T,legend=c("B1","B2","B3"),
        
        col=c("blue","yellow","red4"),
        main="Unidades a enviar (Método esquina noroeste)")



#Unidades a enviar por método de multiplicadores
R1<-c(0,419,0)
R2<-c(0,7770,0)
R3<-c(3130,0,0)
R4<-c(0,432,0)
R5<-c(0,0,876)
R6<-c(0,0,273)
R7<-c(602,0,0)
R8<-c(0,0,2490)
R9<-c(1735,0,0)
R10<-c(0,2125,0)
R11<-c(3035,0,0)
R12<-c(0,847,0)
R13<-c(5040,0,0)
R14<-c(0,1541,0)
R15<-c(11651,7293,18831)


RS.CON.w<-cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15)
RS.CON.w

barplot(RS.CON.w, beside=T, hori=T,legend=c("B1","B2","B3"),
        
        col=c("blue","yellow","red4"),
        main="Unidades a enviar (Método multiplicadores)")


