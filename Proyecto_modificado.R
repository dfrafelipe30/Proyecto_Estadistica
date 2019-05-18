library("dplyr")
library("zoo")
library("fitdistrplus")
library("ggplot2")
#Base de datos de hurtos personas
Hurto_a_personas_2019 <- read.csv("../Proyecto_Estadistica/Hurto_a_personas_2019.csv",encoding = "UTF-8")
#View(Hurto_a_personas_2019)

#Cargando base de datos de incautaciones de armas de fuego
Incautacion_armas_de_fuego_2019 <- read.csv("../Proyecto_Estadistica/Incautaci_n_armas_de_fuego_2019.csv",encoding = "UTF-8")
#View(Incautacion_armas_de_fuego_2019)

#Quitando los valores de NA
Hurto_a_personas <- na.omit(Hurto_a_personas_2019)
Incautacion_armas <- na.omit(Incautacion_armas_de_fuego_2019)
#View(Hurto_a_personas)

#Agregando la columna en la tabla hurtos para luego agrupar los datos.
unique(Hurto_a_personas$Arma.empleada)
Hurto_a_personas[, "arma_fuego"] <- ifelse(Hurto_a_personas$Arma.empleada == "ARMA DE FUEGO", 1, 0)
#View(Hurto_a_personas)

#Arreglando la columna sexo y edad  para poder agrupar los datos.
unique(Hurto_a_personas$Sexo)
Hurto_a_personas[,"sexo"] <- ifelse(Hurto_a_personas$Sexo == "MASCULINO","M","H")


#Base de datos de la poblacion de habitantes por departamentos.
Poblacion <- read.csv("../Proyecto_Estadistica/departamentos.csv",sep = ";")
#View(Poblacion)

#Arreglamos la cantidad de habitantes en cundinamarca ya que la base de datos de 
# Hurtos no continen a Bogota, luego le agregamos los habitante de Bogota a Cundinamarca.
Poblacion$Poblacion[Poblacion$Departamento == "Cundinamarca"] <- 
Poblacion$Poblacion[Poblacion$Departamento == "Cundinamarca"] + Poblacion$Poblacion[Poblacion$Departamento == "Bogotá"]


#Creacion de una base de datos de apoyo para obtener la proporcion de robos por 
#departamentos
base_h_f <- group_by(Hurto_a_personas,Departamento,arma_fuego)%>%count()
base_h_f[,"departamento"] <- tolower(base_h_f$Departamento)
Poblacion[,"departamento"] <- tolower(Poblacion$Departamento)
##View(Poblacion)
Hurto_Final <- left_join(base_h_f,Poblacion,by = "departamento")
#Vamos a tomar la proporcion de 10000 habitantes.
Hurto_Final[,"Proporcion"] <- (Hurto_Final$n/Hurto_Final$Poblacion)*10000 #Cada 10000 personas, atracan a tantas


#Creacion de la base de datos final para las armas incautadas
Incautacion_Final <- group_by(Incautacion_armas,DEPARTAMENTO)%>%count()
Incautacion_Final[,"departamento"] <- tolower(Incautacion_Final$DEPARTAMENTO)
#View(Incautacion_Final)

#Base de datos para agrupar los robos y armas incautadas por departamentos.
#En esta base de datos agrupamos todos los datos necesarios para reaizar nuestro
#estudio acerca de la relacion entre el numero de robos y el numero de armas }
#incautadas.
base_final <- left_join(Hurto_Final,Incautacion_Final, by = "departamento")
base_filtrada <- base_final%>%filter(arma_fuego == 1)
##View(base_filtrada)

attach(base_filtrada)

#Grafico acerca de los Robos por departamentos, donde nos muestra cual es 
#el departamento mas peligroso.
g <- ggplot(Hurto_a_personas,aes(Departamento)) + geom_bar()
g <- g + coord_flip()

#ANALISS DESCRIPTIVO

#Analisis descriptivo del numero de robos.
promedio_de_robos <- sum(n.x)/32
summary(n.x)
sd(n.x)
#Analisis descriptivo de armas de fuego
promedio_de_armas <- sum(n.y)/32
summary(n.y)
sd(n.y)
hist(n.y,xlab = "Numero de armas incautadas",ylab = "Frecuencia",
     main = "Frecuencia armas incautadas")
#Analisis descriptivo del sexo.
g <- ggplot(Hurto_a_personas,aes(sexo)) + geom_bar()
g <- g + coord_flip()
#Analisis descriptivo de la edad.
Hurto_a_personas[,"edad"] <- abs(Hurto_a_personas$Edad)
summary(Hurto_a_personas$edad)
hist(Hurto_a_personas$edad,xlab = "Edad",ylab = "Frecuencia",
     main = "Histograma de Edad")


#PARAMETROS

#Parametro de la media de robos.
sigma <- sd(n.x)
n <- length(n.x)
intervalo_inferior <- promedio_de_robos - (qnorm(0.95) * (sigma/sqrt(n))) 
intervalo_superior <- promedio_de_robos + (qnorm(0.95) * (sigma/sqrt(n)))
#Varianza del numero de armas incautadas
S <- (sum((n.y - mean(n.y))**2))/(n - 1)
intervalo_inferior_S <- ((n - 1)*S)/qchisq(0.95,df = n- 1)
invtervalo_superior_S <- ((n - 1)*S)/qchisq(0.05,df = n- 1)


#PRUEBAS DE HIPOTESIS

#Ho: p1 - p2 = 0
#Ha: p1 - p2 != 0
#E_P = p1 - p2 / sqrt(p1(1 - p1)/n1  +  p2(1 - p2)/n2)
#RR  = {x : |x| > z(alpha/2)}

n1 <-  base_filtrada$Poblacion[base_filtrada$Departamento.x == "CUNDINAMARCA"]
n2 <- sum(base_filtrada$Poblacion)

p1 <- base_filtrada$Proporcion[base_filtrada$Departamento.x == "CUNDINAMARCA"]
p2 <- sum(n.x)/n2

E_P <- (p1 - p2) / sqrt(((p1*(1 - p1))/n1)  +  ((p2*(1 - p2))/n2))

#MODELO LINEAL

plot(base_filtrada$n.y,base_filtrada$n.x,xlab = "Numero de armas incautadas",ylab = "Numero de robos")
cor(n.y,n.x)
modelo <- lm(n.y~n.x)
abline(a = modelo$coefficients[1],b = modelo$coefficients[2])

#PRUEBAS DE BONDAD DE AJUSTE


#Para el numero de armas
fit.exp <- fitdist(n.y,distr = "exp",method = "mle")
summary(fit.exp)
plot(fit.exp)
x = n.y
bj1 <- hist(x,breaks = 9,include.lowest = FALSE,right = FALSE)
bj_cdf1 <- pexp(bj1$breaks,rate =  0.005294741)
pj1 <- rollapply(bj_cdf1,2,function(x) x[2] - x[1])

E_P_1 <- sum((bj1$counts - (length(x)* pj1))**2/(length(x)*pj1)) 
qchisq(0.95,7)

#Para la edad
fit.norm <- fitdist(Hurto_a_personas$edad,distr = "norm",method = "mle")
summary(fit.norm)
plot(fit.norm)
x1 = Hurto_a_personas$edad
bj2 <- hist(x1,breaks = 18,include.lowest = FALSE,right = FALSE)
bj_cdf2 <- pnorm(bj2$breaks,mean = 34.98,sd = 13.15)
pj2 <- rollapply(bj_cdf2,2,function(x) x[2] - x[1])

E_P_2 <- sum((bj2$counts - (length(x1)* pj2))**2/(length(x1)*pj2)) 
qchisq(0.95,17)


