### Instalación de los paquetes necesarios
install.packages('arules', dep=T)
install.packages("arulesViz")

library(arulesViz)
library(arules)
library(datasets)

### Lectura de los datos
data(Groceries)
inspect(Groceries[1:3])

# PAUSA HASTA PRESIONAR ENTER
readline(prompt="GRÁFICO 1/5: Presiona [Enter] en la consola para continuar...")

### Podemos ver la frecuencia de los primeros 10 ítems - Valores absolutos:
itemFrequencyPlot(Groceries, topN=10, type='absolute')

# PAUSA HASTA PRESIONAR ENTER
readline(prompt="GRÁFICO 2/5: Presiona [Enter] en la consola para continuar...")

### Podemos ver la frecuencia de los primeros 10 ítems - Valores relativos: 
itemFrequencyPlot(Groceries, topN=10, type='relative')

### También podemos obtener una visión general de los datos:
summary(Groceries)

### Ahora vamos a ver las reglas:
### Primero, establecemos Soporte = 0,001 y Confianza = 0,7
set.seed(1912)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.7, minlen=2))
summary(rules)

### Veamos las primeras 5 reglas ordenadas por confianza:
options(digits=2)
inspect(sort(rules[1:5], by="confidence"))

### Si quiero saber qué se compró con cerveza, por ejemplo (¿quién compra cerveza también compra qué productos?)
set.seed(1912)
rules <- apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.1,minlen=2), appearance = list(default='rhs',lhs='bottled beer'), control = list(verbose=F))
inspect(sort(rules, by='confidence', decreasing=T))

# PAUSA HASTA PRESIONAR ENTER
readline(prompt="GRÁFICO 3/5: Presiona [Enter] en la consola para continuar...")

# Diagrama de reglas de asociación
# a) Diagrma de dispersión (support, confidence, lift)
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

# PAUSA HASTA PRESIONAR ENTER
readline(prompt="GRÁFICO 4/5: Presiona [Enter] en la consola para continuar...")

#b) Diagrama  en forma de red
plot(rules, method = "graph", engine = "htmlwidget")

# PAUSA HASTA PRESIONAR ENTER
readline(prompt="GRÁFICO 5/5: Presiona [Enter] en la consola para continuar...")

#c) Filtrar reglas antes de graficar
#Por ejemplo, reglas con alto lift:
highLiftRules <- subset(rules, lift > 1)
plot(highLiftRules, method = "graph")

