# Kevin Garc�a - 1533173
# Alejandro Vargas - 1525953
#Alejandro Soto -
#Natalia Buitron -
# Laboratorio 3 - Aplicada III
# Datos Caso 8 Naranjas
library("FactoMineR")
library("Factoshiny")
library("factoextra")
library("missMDA")
library("ade4")
data("orange") #Carga de base de datos
datos<-as.matrix(orange) #Volvemos el data frame una matriz
#Se reescala con la desviaci�n no corregida:
njm=colMeans(datos,na.rm=TRUE); njs=colSums(datos,na.rm=TRUE)
nj=njs/njm
X=scale(datos)*sqrt(nj/(nj-1)) #Matriz estandarizada con NA's
matrizimputada<-imputeMFA(as.data.frame(X),group = c(3,5),type = rep("s",2)) #Imputaci�n matriz estandariza
#Estadisticas descrptivas
summary(orange)
sd(orange$Typicity,na.rm = T)

#AFM a la matriz dividida en las 3 primeras columnas y las 5 ultimas
#Con imputaci�n por medio del metodo regularizado:
matrizoriginalR<-imputeMFA(as.data.frame(datos),group = c(3,5),type = rep("s",2)) 
matrizoriginalR$completeObs #Matriz original imputada regularizado
res.mfa <- MFA(as.data.frame(matrizoriginalR$completeObs),group=c(3,5),type=c("s","s"),
               name.group=c("Percepci�n previa","Percepci�n posterior"))

MFAshiny(res.mfa)

#Con imputaci�n por medio del metodo EM:
matrizoriginalEM<-imputeMFA(as.data.frame(datos),group = c(3,5),type = rep("s",2),method = "EM") 
matrizoriginalEM$completeObs #Matriz original imputada EM
res.mfa1 <- MFA(as.data.frame(matrizoriginalEM$completeObs),group=c(3,5),type=c("s","s"),
               name.group=c("Percepci�n previa","Percepci�n posterior"))

MFAshiny(res.mfa1)

#Matriz de correlaciones
cor(matrizoriginalEM$completeObs)

#Porcentaje de inercia explicado
res.mfa1$eig  #Una forma
get_eigenvalue(res.mfa1) #otra forma
#Gr�fica del porcentaje de inercia explicado
x11()
fviz_eig(res.mfa)

#Resultados para individuos
res.mfa1$ind #Una forma
get_mfa_ind(res.mfa) #Otra forma

x11()
fviz_mfa_ind(res.mfa1)


#Resultados para variables
res.mfa1$quanti.var #Una forma
get_mfa_var(res.mfa1) #Otra forma

x11()
fviz_mfa_var(res.mfa1)

#Gr�fica variables e individuos juntos
x11()
s.label(res.mfa1$ind$coord,box=FALSE)
s.label(res.mfa1$quanti.var$coord,add.plot=TRUE)

#Resultados para los grupos, coeficientes lg y rv
x11()
fviz_mfa_group(res.mfa1,xlim=c(0,1),ylim=c(0,1))

res.mfa1$group

# Contribuci�n de los individuos a la primera dimensi�n
x11()
fviz_contrib(res.mfa1, "ind", axes = 1)
# Contribuci�n de los individuos a la segunda dimensi�n
x11()
fviz_contrib(res.mfa1, "ind", axes = 2)

# Contribuci�n de las variables a la primera dimensi�n
x11()
fviz_contrib(res.mfa1, "quanti.var", axes = 1)
# Contribuci�n de las variables a la segunda dimensi�n
x11()
fviz_contrib(res.mfa1, "quanti.var", axes = 2)

# Contribuci�n de los grupos a la primera dimensi�n
x11()
fviz_contrib(res.mfa1, "group", axes = 1)
# Contribuci�n de los grupos a la segunda dimensi�n
x11()
fviz_contrib(res.mfa1, "group", axes = 2)

# Cosenos cuadrados de los individuos a la primera dimensi�n
x11()
fviz_cos2(res.mfa1, "ind", axes = 1)
# Cosenos cuadrados de los individuos a la segunda dimensi�n
x11()
fviz_cos2(res.mfa1, "ind", axes = 2)
# Cosenos cuadrados de las variables a la primera dimensi�n
x11()
fviz_cos2(res.mfa1, "quanti.var", axes = 1)
# Cosenos cuadrados de las variables a la segunda dimensi�n
x11()
fviz_cos2(res.mfa1, "quanti.var", axes = 2)


### Representaci�n superpuesta
x11()
fviz_mfa_ind(res.mfa1, partial = "all") 


#Gr�fica de ejes parciales
x11()
fviz_mfa_axes(res.mfa1)

#Construcci�n indice
#Indice de los jugos para el primer grupo (percepci�n previa)
I=res.mfa1$quanti.var$coord[1:3,1]%*%t(matrizoriginalEM$completeObs[,-c(4,5,6,7,8)])
Ie=(I-min(I))/(max(I)-min(I))*100 #Reescalado de indices

