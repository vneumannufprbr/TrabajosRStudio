### Práctica 1
### Coloca las recompensas en una matriz
R <- matrix(c(-1, -1, -1, -1,   0,  -1,
              +             -1, -1, -1,  0,  -1, 100,
              +             -1, -1, -1,  0,  -1,  -1, 
              +             -1,  0,  0, -1,   0,  -1,
              +              0, -1, -1,  0,  -1, 100,
              +             -1,  0, -1, -1,   0, 100), nrow=6, ncol=6, byrow=TRUE)


### Carga y ejecuta el código fuente de un algoritmo de Q-Learning
source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/qlearn.R") 
results <- q.learn(R,10000,alpha=0.1,gamma=0.8,tgt.state=6)

### Mostrar el resultado
round(results)


### Práctica 2
### Coloca las recompensas en una matriz
R <- matrix(c(10,0,-1,0,-1, -1, -1, -1, -1,10,0,6,-1,-10, -1, -1, -1, -1, -1,0,6, -1,
              -1,-10, -1, -1, -1,10, -1, -1,0,-10, -1,0, -1, -1, -1,0, -1,0,-10,-10,
              -1,0, -1, -1, -1, -1, -1,-10,-10, -1, -1,4, -1, -1, -1,0, -1, -1,0,0, 
              -1, -1, -1, -1, -1,-10, -1,0,0,4, -1, -1, -1, -1, -1,-10, -1,0,4), nrow=9, ncol=9, byrow=TRUE)

### Carga y ejecuta el código fuente de un algoritmo de Q-Learning
#source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/qlearn.R") 
results <- q.learn(R,10000,alpha=0.1,gamma=0.8,tgt.state=1)

### Mostrar el resultado
round(results)

