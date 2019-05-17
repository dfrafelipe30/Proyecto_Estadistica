library("dplyr")
library("ggplot2")
##Base de datos de hurtos personas
Hurto_a_personas_2019 <- read.csv("C:/Users/prestamour/Downloads/Hurto_a_personas_2019.csv",encoding = "UTF-8")
View(Hurto_a_personas_2019)

##Cargando base de datos de incautaciones de armas de fuego
Incautacion_armas_de_fuego_2019 <- read.csv("C:/Users/prestamour/Downloads/Incautaci_n_armas_de_fuego_2019.csv",encoding = "UTF-8")
View(Incautacion_armas_de_fuego_2019)

##Quitando los valores de NA
Hurto_a_personas <- na.omit(Hurto_a_personas_2019)
Incautacion_armas <- na.omit(Incautacion_armas_de_fuego_2019)
View(Hurto_a_personas)

##Agregando la columna en la tabla hurtos
unique(Hurto_a_personas$Arma.empleada)
Hurto_a_personas[, "arma_fuego"] <- ifelse(Hurto_a_personas$Arma.empleada == "ARMA DE FUEGO", 1, 0)
View(Hurto_a_personas)

#Arreglo del sexo y edad
unique(Hurto_a_personas$Sexo)
Hurto_a_personas[,"sexo"] <- ifelse(Hurto_a_personas$Sexo == "MASCULINO","M","H")


##Buscar base de datos de la cantidad de habitantes.
Poblacion <- read.csv("c:/Users/prestamour/Downloads/departamentos.csv",sep = ";")
View(Poblacion)

##Modificacion de Poblacion
Poblacion$Poblacion[Poblacion$Departamento == "Cundinamarca"] <- 
Poblacion$Poblacion[Poblacion$Departamento == "Cundinamarca"] + Poblacion$Poblacion[Poblacion$Departamento == "Bogotá"]


##Creacion de la nueva base de datos
base_h_f <- group_by(Hurto_a_personas,Departamento,arma_fuego,sexo)%>%count()
base_h_f[,"departamento"] <- tolower(base_h_f$Departamento)
Poblacion[,"departamento"] <- tolower(Poblacion$Departamento)
View(Poblacion)
Hurto_Final <- left_join(base_h_f,Poblacion,by = "departamento")
View(Hurto_Final)
Hurto_Final[,"Proporcion"] <- (Hurto_Final$n/Hurto_Final$Poblacion)*10000 #Cada 10000 personas, atracan a tantas
View(Hurto_Final)

#Creacion de la base de datos final para las armas incautadas
Incautacion_Final <- group_by(Incautacion_armas,DEPARTAMENTO)%>%count()
View(Incautacion_Final)

##Analisis descriptivo
promedio <- sum(Hurto_Final$n)/(sum(Hurto_Final$Poblacion)/2)
sum(Hurto_Final$Poblacion)/2

base_final <- left_join(Hurto_Final,Incautacion_Final, by = "departamento")

base_filtrada <- base_final%>%filter(arma_fuego == 1)
View(base_filtrada)

attach(base_filtrada)
p <- ggplot(x = factor(Departamento.x),y = n.x) + geom_boxplot()

plot(base_filtrada$n.y,base_filtrada$n.x,xlab = "Numero de armas incautadas",ylab = "Numero de robos")
cor(n.y,n.x)
modelo <- lm(n.y~n.x)
abline(a = modelo$coefficients[1],b = modelo$coefficients[2])
plot(x = factor(Departamento.x),n.x,xlab = "Departamentos",ylab = "Numero de robos",type = "o")
with(base_filtrada,text(Departamento.x,labels = row.names(Departamento.x)))
g <- ggplot(Hurto_a_personas,aes(Sexo)) + geom_bar()
g <- g + coord_flip()

