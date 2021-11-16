#Tarea 1.31 
#Agrego el link de la tarea: https://drive.google.com/file/d/1DZrDWLFzxRtQ2k4nVHpHSN5CvjASmHa7/view?fbclid=IwAR0thpF_n7sKHjz7vSnRoi4PysFY-r5Afoxx5-J-MvUMWUCjTEDVLmjz-64 por si quieren mirar las instrucciones
#del  pdf de las instrucciones para poder entender el objetivo.
#Notar que se da solución al inciso 1.31 y 1.35



dn <- function(x){
  n <- length(x)
  x <- sort(x)
  j <- c(1:n)/n
  j_1 <- c(0:(n-1))/n
  Fo <- pnorm(x, mean = 0, sd = 1)
  dn = sqrt(n)*max((j-Fo),(Fo - j_1))
  return (dn)
}
d.sim <- function(m){
  d.sim <- vector(mode = "numeric", length = m)
  for (k in 1:m) {
    u <- runif(100)
    x <- qnorm(u)
    d.sim[k] <- dn(x)
  }
  return(d.sim)
} 
pv.summary.ks <- function(v, n.muestra, n.sim, d.sim){
  p.values <- vector(mode = "numeric", length = n.sim)
  for (i in 1:n.sim) {
    x.obs <- rt(n = n.muestra, df = v)
    d.obs <- dn(x.obs)
    p.values[i] <- mean(d.sim >= d.obs)
  }
  pv.summary.ks = summary(p.values)
  return (pv.summary.ks)
}
table.ks <- function(n.param, n.muestra, n.sim, d.sim){
  df.ks <- data.frame("minimo" = numeric(),"PrimerCuanti" = numeric(),"mediana" = numeric(),"prom" = numeric(),"TercerCuanti" = numeric(),"maximo" = numeric())
  for (v in 1:n.param) {
    df.ks[v,] <- pv.summary.ks(v = v, n.muestra = n.muestra, n.sim = n.sim, d.sim = d.sim)
  }
  return(df.ks)
}

#Datos
d.sim <- d.sim(m = 100000)
n.muestra1 <- 50
n.muestra2 <- 1000
n.sim <- 10000

#Tablas
Tabla1.ks <- table.ks(n.param = 30,n.muestra = n.muestra1,n.sim,d.sim)
Tabla2.ks <- table.ks(n.param = 30,n.muestra = n.muestra2,n.sim,d.sim)
View(Tabla1.ks)
#Grafica
plot(x = c(1:30),y = Tabla1.ks$prom, ylim = c(0,0.6), main = "Test KS por parametro",xlab = "Parametro t-studen", ylab = "P-values")
points(x = c(1:30), y = Tabla2.ks$prom, col = "blue")

#Tarea 1.35 

an <- function(x){
  n <- length(x)
  x <- sort(x)
  x_decre <- sort(x, decreasing = T)
  j <- c(1:n)
  an = -n^2 - sum((2*j - 1)*(log(pnorm(x))+log(1- pnorm(x_decre))))
  return(an)
}
a.sim <- function(m){
  a.sim <- vector(mode = "numeric", length = m)
  for (k in 1:m) {
    u <- runif(100)
    x <- qnorm(u)
    a.sim[k] <- an(x)
  }
  return(a.sim)
} 
pv.summary.a <- function(v, n.muestra, n.sim, a.sim){
  p.values <- vector(mode = "numeric", length = n.sim)
  for (i in 1:n.sim) {
    x.obs <- rt(n = n.muestra, df = v)
    a.obs <- an(x.obs)
    p.values[i] <- mean(a.sim >= a.obs)
  }
  pv.summary.a = summary(p.values)
  return (pv.summary.a)
}
table.a <- function(n.param, n.muestra, n.sim, a.sim){
  df.a <- data.frame("minimo" = numeric(),"PrimerCuanti" = numeric(),"mediana" = numeric(),"prom" = numeric(),"TercerCuanti" = numeric(),"maximo" = numeric())
  for (v in 1:n.param) {
    df.a[v,] <- pv.summary.a(v = v, n.muestra = n.muestra, n.sim = n.sim, a.sim = a.sim)
  }
  return(df.a)
}

#Datos
a.sim <- a.sim(m = 100000)
n.muestra1 <- 50
n.muestra2 <- 1000
n.sim <- 10000

#Tablas
Tabla1.a <- table.a(n.param = 30,n.muestra = n.muestra1,n.sim,a.sim)
Tabla2.a <- table.a(n.param = 30,n.muestra = n.muestra2,n.sim,a.sim)
View(Tabla1.a)
#Grafica
plot(x = c(1:30),y = Tabla1.a$prom, ylim = c(0,0.8), main = "Test Anderson por parametro", xlab = "Parametro t-studen", ylab = "P-values")
points(x = c(1:30), y = Tabla2.a$prom, col = "red")
plot(x = c(1:30), y = Tabla2.a$prom, col = "red")


#Comparacion de pruebas
plot(c(1:30), y = Tabla1.ks$prom, ylim = c(0,0.8), col = "blue",main = "Comparacion de las pruebas con muestras de tamaño 50", xlab = "Parametro t-studen", ylab = "P-values")
points(x = c(1:30), y = Tabla1.a$prom, col = "red")

plot(c(1:30), y = Tabla2.ks$prom, ylim = c(0,0.8), col = "blue",main = "Comparacion de las pruebas con muestras de tamaaño 1000", xlab = "Parametro t-studen", ylab = "P-values")
points(x = c(1:30), y = Tabla2.a$prom, col = "red")

#Conclusion
"
La prueba Anderson es muy sensible ante muestras pequeñas.
"