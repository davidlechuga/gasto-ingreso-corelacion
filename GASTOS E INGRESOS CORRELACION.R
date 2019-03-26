##LECHUGA HUERTA DAVID ####
###    ""declarando objetos"" 
## Repositorio en GITHUB##
## el codigo se ha revisado por la CGyE ####
##### CADENA DE NUMEROS QUE REPRENTAN LOS 
#####  INGRESOS "X"      Y,       GASTOS  "Y"
ingresos=c(6,8,10,12,14,16,18,20);
gastos=c(6,8,8,12,12,14,17,16);
n<-c(8)   
n            ## 8 familias sus gastos e ingresos
#####
####
####
########   TABLA DE XY 
#####
#####
ingresosyegresos=data.frame(ingresos,gastos)
ingresosyegresos
####
#####
####   suma de x  y su tabla
INGRESOS=data.frame(ingresos)
INGRESOS
sum(ingresos)
########  suma de y,   y su tabla
GASTOS=data.frame(gastos)
GASTOS
sum(GASTOS)
####
#####
######
####  MULTIPLICACION "XY"   ####
####
INGRESOS*GASTOS
View(INGRESOS*GASTOS)
sum(INGRESOS*GASTOS)
####
###   INGRESOS AL CUADRADO   "x^2"
#####
INGRESOS^2
sum(INGRESOS^2)
### 
###### GASTOS AL CUADRADO  "y^2"
####
GASTOS^2
sum(GASTOS^2)
####
####  CREAR NUBE DE PUNTOS
#####   y dibujar la linea de la recta
####    una vez obtenido "y=a+bx"
####
plot(ingresos,gastos)
abline(lm(gastos~ingresos, data=ingresosyegresos))
####
####
##### ENCONTRAR LA ECUACION DE LA REGRESION LINEAL
####
####              ---------  Y=a+bx-------
#####    
##### ----- PRIMERO HAY QUE ENCONTRAR LA PENDIENTE DE "b"-----
######   ----- b=(n(sum(xy))-sum(x)*sum(y))/(n(sum(x^2))-sum(x)^2)  ------
####
####
(n*sum(INGRESOS*GASTOS)-sum(INGRESOS)*sum(GASTOS))/(n*sum(INGRESOS^2)-sum(INGRESOS)^2)
b=(n*sum(INGRESOS*GASTOS)-sum(INGRESOS)*sum(GASTOS))/(n*sum(INGRESOS^2)-sum(INGRESOS)^2)
b
#####
######   ---SEGUNDO ENCONTRAR "a" o valor de la ordenada al origen
#####
####
###### ----- a=(sum(y)/n)-(b*sum(x)/n)
####
####
sum(GASTOS)/n
b*sum(INGRESOS)/n
(sum(GASTOS)/n)-(b*sum(INGRESOS)/n)
a=(sum(GASTOS)/n)-(b*sum(INGRESOS)/n)
a
####
####
####   VERIFICAR VALORES MEDIANTE COMANDO RAPIDO
####
######
lm(gastos~ingresos, data= ingresosyegresos)
###### POR CADA 1000 PESOS QUE INGRESA
######  LAS FAMILIAS EN PROMEDIO GASTARAN 792 PESOS
#########
#########  -----  "y=a+bx"  ------
#####
########    
a
b
a+b*19
####   x= 19 pesos de ingresos
####   El gasto esperado de una familia cuando tiene
####   19 mil pesos de ingresos es de 16mil375 pesos.
#####
####
######
#####  SE HIZO LA ECUACION DE REGRESION LINEAL PARA DIBUJAR LA RECTA.
######  AHORA SE ENCONTRARÁ EL COEFICIENTE DE CORELACIÓN DE PEARSON
#####
#####
##### r= sum(xy)-(sum(x)*sum(y))/(n)/(sqrt((sum(x^2)-(sum(x)^2)/(n)))*(sum(y^2))-(sum(y)^2/(n)))
###
####
sum(INGRESOS*GASTOS)-(sum(INGRESOS)*sum(GASTOS))/(n)
#####
####
sum(INGRESOS^2)-(sum(INGRESOS)^2)/(n)
sum(GASTOS^2)-sum(GASTOS)^2/(n)

(sum(INGRESOS^2)-(sum(INGRESOS)^2)/(n))*(sum(GASTOS^2)-sum(GASTOS)^2/(n))

## aplicar raiz cuadrada
sqrt((sum(INGRESOS^2)-(sum(INGRESOS)^2)/(n))*(sum(GASTOS^2)-sum(GASTOS)^2/(n))
)
### operacion y resultado final
#####
###
#####  
(sum(INGRESOS*GASTOS)-(sum(INGRESOS)*sum(GASTOS))/(n))/sqrt((sum(INGRESOS^2)-(sum(INGRESOS)^2)/(n))*(sum(GASTOS^2)-sum(GASTOS)^2/(n))
)
#### el resultado significa que 
#### se obtuvo una correlacion positiva intensa
#####
