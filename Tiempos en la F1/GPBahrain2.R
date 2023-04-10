# FUNCIONES GENERALES--------------------------------------------

# Funcion para graficar los contornos de verosilitud relativa de (mu, sigma)
plotRelative<-function(l, aG, bG, xL, xR, yL, yR, levels, n, xlab = expression(paste(beta[0])),
                       ylab = expression(paste(beta[1])), main="Contornos de verosimilitud relativa"){
  x_vec = seq(from = xL, to = xR, length.out = n)
  y_vec = seq(from = yL, to = yR, length.out = n)
  R<-function(a, b){
    return(exp(l(a, b) - l(aG, bG)))
  }
  Rmat = matrix(nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      Rmat[i, j] = R(x_vec[i], y_vec[j])
    }
  }
  contour(x_vec,y_vec,Rmat,level=levels,xlab=xlab,ylab=ylab,main=main)
}

# Funcion para grafica PP
pp_plot = function(X, ag, bg, confidence){
  Y = pnorm(sort(X), ag, bg)
  X = seq(1/(n+1), 1, length.out = n)
  # puntos de la muestra
  plot(X, Y,
       main = "Gr?fica PP",
       xlab = "Probabilidades emp?ricas",
       ylab = "Probabilidades te?ricas",
       pch = 19,
       cex = 0.5)
  # identidad
  abline(a = 0, b = 1, col = "red", lwd = 2)
  # bandas de confianza
  points(X, qbeta((1 - confidence)/2, 1:n, n + 1 - 1:n),
         type = "l",
         lty = 2)
  points(X, qbeta((1 + confidence)/2, 1:n, n + 1 - 1:n),
         type = "l",
         lty = 2)
}

# DATOS DEL PROBLEMA --------------------------------------------------------

# Tamano de la muestra observada
n <- 120

# Numero de vueltas
X <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
       5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
       9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
       11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
       13,13,13,13,13,13,13,13,13,13,13,13,13,13)

# Tiempos en segundos
Y <- c(97.853,97.880,98.649,99.092,99.002,100.817,98.993,100.412,99.694,100.169,100.111,100.722,100.548,101.229,100.697,101.077,101.019,102.298,
       98.414,98.566,98.923,98.741,98.892,101.127,99.642,100.097,99.772,100.158,100.209,101.163,101.126,100.393,100.958,101.189,101.931,101.947,
       98.712,98.940,99.594,99.218,99.707,101.003,100.338,100.658,99.605,100.474,100.558,102.447,101.708,101.461,100.611,101.995,101.897,102.945,
       98.951,99.092,99.588,99.264,99.682,101.287,100.555,100.823,99.934,100.433,100.782,102.512,101.561,100.620,100.754,101.912,101.827,102.065,
       99.123,99.392,99.682,99.806,101.581,100.788,101.195,101.873,100.236,100.981,100.992,103.969,103.500,101.339,101.182,103.228,102.350,102.678,
       99.256,99.410,100.097,99.754,101.073,101.370,100.215,101.347,101.094,105.949,102.505,101.281,101.096,102.821,102.462,103.492,
       99.629,102.050,103.029,100.035,103.736,104.244,100.409,104.005,104.080,101.185,101.352,102.959,101.803,105.315)

# Vector de 1's
Aux <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
         1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

# Coeficiente de correlacion de Pearson
cor(X, Y, method = "pearson")

PX <- sum(X) / n
PY <- sum(Y) / n

SXX <- sum(X^2) - n * (PX * PX)
SYY <- sum(Y^2) - n * (PY * PY)
SXY <- sum(X * Y) - n * PX * PY

RSS <- SYY - ((SXY * SXY) / SXX)

# Coeficiente de determinaci?n
R2 <- 1 - (RSS / SYY)

# Matriz de diseno
XX <- cbind(Aux, X)

# Matriz transpuesta
XXt <- rbind(Aux, X)

# Estimadores de maxima verosimilitud
b1g <- (n * sum(X*Y) - sum(X)*sum(Y)) / det(XXt %*% XX)
b0g <- (sum(Y) / n) - b1g * (sum(X) / n)
bg <- rbind(b0g, b1g)
sigmag <- (t(Y - XX %*% bg) %*% (Y - XX %*% bg)) / n

# Residuos observados
R <- Y - b0g - (b1g * X)

# Parametros asociados a los residuos con distribucion Normal
t1 <- sum(R)
t2 <- sum(R^2)
mg <- t1 / n
H <- t2 - (t1^2 / n)
sg <- sqrt(H / n)

# FUNCIONES EMVR's, VEROSIMILITUD, LOGVEROSIMILITUD (+RELATIVAS) -------------------------------

# Densidad (log) con todos los parametros
d1 <- function(b0,b1,sig){
  return(log(((1/(2*pi*sig))^(n/2)))+((-1/(2*sig))*sum((Y-b0-b1*X)^2)))
}

# Densidad (log) con sin b_0
d2 <- function(b1,sig){
  return(log(((1/(2*pi*sig))^(n/2)))+((-1/(2*sig))*sum((Y-b1*X)^2)))
}

# Emvr de beta1
emvrb1 <- function(beta0){
  return(((sum(X * Y) - beta0 * sum(X)) / (sum(X^2))))
}

# Emvr de beta0
emvrb0 <- function(beta1){
  return(((sum(Y) - beta1 * sum(X)) / n))
}

# Inversa del emvr de beta0
Iemvrb0 <- function(beta1){
  return((sum(Y) - n * beta1) / sum(X))
}

# Logverosimilitud perfil de beta_0
lpb0 <- function(beta0){
  return((-n/2)*log((sum((Y - beta0 - emvrb1(beta0)*X)^2)) / n) - n/2)
}

# Logverosimilitud perfil de beta_1
lpb1 <- function(beta1){
  return((-n/2)*log((sum((Y - emvrb0(beta1) - beta1*X)^2)) / n) - n/2)
}

# Variables auxiliares para la logverosimilitud perfil de sigma
a1 <- t(Y - (XX %*% bg))
a2 <- Y - (XX %*% bg)

# Logverosimilitud perfil de sigma
lps <- function(sigma){
  return((-n/2)*log(sigma) - ((1/(2*sigma))*(a1 %*% a2)))
}

# Verosimilitud perfil relativa de beta_0
Rpb0 <- function(beta0){
  return(exp(lpb0(beta0) - lpb0(b0g)))
}

# Verosimilitud perfil relativa de beta_1
Rpb1 <- function(beta1){
  return(exp(lpb1(beta1) - lpb1(b1g)))
}

# Verosimilitud perfil relativa de sigma
Rps <- function(sigma){
  return(exp(lps(sigma) - lps(sigmag)))
}

# Logverosimilitud de (beta0, beta1)
Blogver <- function(beta0, beta1){
  return((-n/2) * log((t(Y - XX %*% c(beta0, beta1)) %*% (Y - XX %*% c(beta0, beta1))) / n) - n/2)
}

# Logverosimilitud relativa de (beta0, beta1)
rlogver <- function(beta0, beta1){
  return(Blogver(beta0, beta1) - Blogver(b0g, b1g))
}

# AIC --------------------------------

# AIC completa
AIC1 <- (-2*d1(b0g,b1g,sigmag))+6

# AIC incompleta
AIC2 <- (-2*d2(b1g,sigmag))+4

# INTERVALOS DE CONFIANZA SIGMA CUADRADA (VARIANZA)------------------------------------------------------------------------------

# Funcion para obtener el la probabilidad de un nivel 
ints <- function(x){
  return(log(Rps(x)) - log(0.245))
}

i1 = uniroot(ints, c(1, 1.8))$root
i2 = uniroot(ints, c(1.8, 3))$root

p <- pchisq((n * sigmag)/ i1, 118, lower.tail = TRUE) - pchisq((n * sigmag) / i2, 118, lower.tail = TRUE)

# Tras probar con distintos valores de de c y ver la probabilidad correspondiente
# en a funci?n anterior, se lleg? que las c's son 0.245, 0.205, 0.137 y 0.031.

# Extremos del intervalo de verosimilitud de nivel c = 0.245
ints90 <- function(sigma){
  return(log(Rps(sigma)) - log(0.245))
}
uno90 = uniroot(ints90, c(0.0001, sigmag))$root
dos90 = uniroot(ints90, c(sigmag, 15*sigmag))$root

# Extremos del intervalo de verosimilitud de nivel c = 0.205
ints92 <- function(sigma){
  return(log(Rps(sigma)) - log(0.205))
}
uno92 = uniroot(ints92, c(0.0001, sigmag))$root
dos92 = uniroot(ints92, c(sigmag, 15*sigmag))$root

# Extremos del intervalo de verosimilitud de nivel c = 0.137
ints95 <- function(sigma){
  return(log(Rps(sigma)) - log(0.137))
}
uno95 = uniroot(ints95, c(0.0001, sigmag))$root
dos95 = uniroot(ints95, c(sigmag, 15*sigmag))$root

# Extremos del intervalo de verosimilitud de nivel c = 0.031
ints99 <- function(sigma){
  return(log(Rps(sigma)) - log(0.031))
}
uno99 = uniroot(ints99, c(0.0001, sigmag))$root
dos99 = uniroot(ints99, c(sigmag, 15*sigmag))$root

# INTERVALOS DE CONFIANZA BETA 0 -------------------------------------

# Extremos del intervalo de verosimilitud de nivel c
extgb0 <- function(c){
  ints <- function(beta){
    return(log(Rpb0(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b0g / 10, b0g))$root
  dos = uniroot(ints, c(b0g, b0g * 10))$root
  
  int <- c(uno, dos)
  
  return(int)
}

# Funcion para obtener c para 90%
gb090 <- function(c){
  ints <- function(beta){
    return(log(Rpb0(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b0g / 10, b0g))$root
  dos = uniroot(ints, c(b0g, b0g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b0g - uno) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  piv2 <- sqrt((n-2)/n) * (b0g - dos) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.90)
}

# Funcion para obtener c para 92%
gb092 <- function(c){
  ints <- function(beta){
    return(log(Rpb0(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b0g / 10, b0g))$root
  dos = uniroot(ints, c(b0g, b0g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b0g - uno) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  piv2 <- sqrt((n-2)/n) * (b0g - dos) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.92)
}

# Funcion para obtener c para 95%
gb095 <- function(c){
  ints <- function(beta){
    return(log(Rpb0(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b0g / 10, b0g))$root
  dos = uniroot(ints, c(b0g, b0g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b0g - uno) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  piv2 <- sqrt((n-2)/n) * (b0g - dos) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.95)
}

# Funcion para obtener c para 99%
gb099 <- function(c){
  ints <- function(beta){
    return(log(Rpb0(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b0g / 10, b0g))$root
  dos = uniroot(ints, c(b0g, b0g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b0g - uno) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  piv2 <- sqrt((n-2)/n) * (b0g - dos) * sqrt(det(XXt %*% XX) / (sigmag * sum(X^2)))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.99)
}

# Obtenemos las c's
c090 = uniroot(gb090, c(0.0001, 0.8))$root
c092 = uniroot(gb092, c(0.0001, 0.8))$root
c095 = uniroot(gb095, c(0.0001, 0.8))$root
c099 = uniroot(gb099, c(0.0001, 0.8))$root

# Obtenemos los intervalos correspondientes
ext090 = extgb0(c090)
ext092 = extgb0(c092)
ext095 = extgb0(c095)
ext099 = extgb0(c099)

# INTERVALOS DE CONFIANZA BETA 1 ------------------------------------

# Extremos del intervalo de verosimilitud de nivel c
extgb1 <- function(c){
  ints <- function(beta){
    return(log(Rpb1(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b1g / 10, b1g))$root
  dos = uniroot(ints, c(b1g, b1g * 10))$root
  
  int <- c(uno, dos)
  
  return(int)
}

# Funcion para obtener c para 90%
gb190 <- function(c){
  ints <- function(beta){
    return(log(Rpb1(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b1g / 10, b1g))$root
  dos = uniroot(ints, c(b1g, b1g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b1g - uno) * sqrt(det(XXt %*% XX) / (n * sigmag))
  piv2 <- sqrt((n-2)/n) * (b1g - dos) * sqrt(det(XXt %*% XX) / (n * sigmag))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.90)
}

# Funcion para obtener c para 92%
gb192 <- function(c){
  ints <- function(beta){
    return(log(Rpb1(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b1g / 10, b1g))$root
  dos = uniroot(ints, c(b1g, b1g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b1g - uno) * sqrt(det(XXt %*% XX) / (n * sigmag))
  piv2 <- sqrt((n-2)/n) * (b1g - dos) * sqrt(det(XXt %*% XX) / (n * sigmag))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.92)
}

# Funcion para obtener c para 95%
gb195 <- function(c){
  ints <- function(beta){
    return(log(Rpb1(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b1g / 10, b1g))$root
  dos = uniroot(ints, c(b1g, b1g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b1g - uno) * sqrt(det(XXt %*% XX) / (n * sigmag))
  piv2 <- sqrt((n-2)/n) * (b1g - dos) * sqrt(det(XXt %*% XX) / (n * sigmag))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.95)
}

# Funcion para obtener c para 99%
gb199 <- function(c){
  ints <- function(beta){
    return(log(Rpb1(beta)) - log(c))
  }
  
  uno = uniroot(ints, c(b1g / 10, b1g))$root
  dos = uniroot(ints, c(b1g, b1g * 10))$root
  
  piv1 <- sqrt((n-2)/n) * (b1g - uno) * sqrt(det(XXt %*% XX) / (n * sigmag))
  piv2 <- sqrt((n-2)/n) * (b1g - dos) * sqrt(det(XXt %*% XX) / (n * sigmag))
  
  return(pt(piv1, n-2, lower.tail = TRUE) - pt(piv2, n-2, lower.tail = TRUE) - 0.99)
}

# Obtenemos las c's
c190 = uniroot(gb190, c(0.0001, 0.8))$root
c192 = uniroot(gb192, c(0.0001, 0.8))$root
c195 = uniroot(gb195, c(0.0001, 0.8))$root
c199 = uniroot(gb199, c(0.0001, 0.8))$root

# Obtenemos los intervalos correspondientes
ext190 = extgb1(c190)
ext192 = extgb1(c192)
ext195 = extgb1(c195)
ext199 = extgb1(c199)

# GRAFICAS -----------------------------------------------------------

# Grafica de puntos, linea de regresion y bandas
bconf1 <- function(x){
  v <- matrix(nrow = 1, ncol = 2)
  v[,1] = 1
  v[,2] = x
  return(v %*% bg + qt(0.975, n - 2, lower.tail = T) * sqrt(sigmag * (v %*% solve(XXt %*% XX) %*% t(v))) * sqrt((n-2) / n))
}

bconf2 <- function(x){
  v <- matrix(nrow = 1, ncol = 2)
  v[,1] = 1
  v[,2] = x
  return(v %*% bg - qt(0.975, n - 2, lower.tail = T) * sqrt(sigmag * (v %*% solve(XXt %*% XX) %*% t(v))) * sqrt((n-2) / n))
}

bpred1 <- function(x){
  v <- matrix(nrow = 1, ncol = 2)
  v[,1] = 1
  v[,2] = x
  return(v %*% bg + qt(0.975, n - 2, lower.tail = T) * sqrt(sigmag * (1 + (v %*% solve(XXt %*% XX) %*% t(v)))) * sqrt((n-2) / n))
}

bpred2 <- function(x){
  v <- matrix(nrow = 1, ncol = 2)
  v[,1] = 1
  v[,2] = x
  return(v %*% bg - qt(0.975, n - 2, lower.tail = T) * sqrt(sigmag * (1 + (v %*% solve(XXt %*% XX) %*% t(v)))) * sqrt((n-2) / n))
}

# Grafica de puntos
y1 <- c()
x1 = seq(1, 13, length.out = 1000)
for(i in 1:1000){
  y1[i] = bconf1(x1[i])
}

y2 <- c()
x2 = seq(1, 13, length.out = 1000)
for(i in 1:1000){
  y2[i] = bconf2(x2[i])
}

y3 <- c()
x3 = seq(1, 13, length.out = 1000)
for(i in 1:1000){
  y3[i] = bpred1(x3[i])
}

y4 <- c()
x4 = seq(1, 13, length.out = 1000)
for(i in 1:1000){
  y4[i] = bpred2(x4[i])
}

# Recta regresion estimada
lineaF <- function(x){
  return ( b0g + b1g * x)
}
plot(X, Y, pch = 16, main = "Tiempos por vuelta", xlab = "Vuelta", ylab = "Segundos")
plot.function( x = function(t)lineaF(t), from = 1, to = 13 , add = TRUE)
lines(x1, y1, type = "l", col = "deepskyblue2", lwd = 2)
lines(x2, y2, type = "l", col = "deepskyblue2", lwd = 2)
lines(x3, y3, type = "l", col = "snow4", lwd = 2)
lines(x4, y4, type = "l", col = "snow4", lwd = 2)

# Grafica PP de residuos
pp_plot(R, mg, sg, 0.95)

# Grafica de verosimilitud perfil de sigma
plot.function(x = function(t) Rps(t),
              from = 1, #Rango
              to = 2.8, lwd = 1.9,
              col = "black",
              main = expression(paste("Verosimilitud perfil de ", sigma[1]^2)),
              ylab = expression(paste(Rp(sigma[1]^2))),
              xlab = expression(paste(sigma[1]^2)))
abline(v = sigmag, lty = 2, col = "red")
segments(x0 = uno90, y0 = 0.245, x1 = dos90, y1 = 0.245, col = "steelblue2", lwd = 2.5)
segments(x0 = uno92, y0 = 0.205, x1 = dos92, y1 = 0.205, col = "steelblue2", lwd = 2.5)
segments(x0 = uno95, y0 = 0.137, x1 = dos95, y1 = 0.137, col = "steelblue2", lwd = 2.5)
segments(x0 = uno99, y0 = 0.031, x1 = dos99, y1 = 0.031, col = "steelblue2", lwd = 2.5)
text(uno90-0.05, 0.245+0.02, expression(c == 0.245), cex = 0.75)
text(uno92-0.05, 0.205+0.02, expression(c == 0.205), cex = 0.75)
text(uno95-0.05, 0.137+0.02, expression(c == 0.137), cex = 0.75)
text(uno99-0.1, 0.031+0.02, expression(c == 0.031), cex = 0.75)
legend("topright", legend = expression(paste(hat(sigma)[1]^2)),
       lwd = 2, lty = 2, col = "red")

# Grafica de verosimilitud perfil de beta_0
y <- c()
x = seq(98.7, 100.6, length.out = 1000)
for(i in 1:1000){
  y[i] = Rpb0(x[i])
}
plot(x, y, type = "l", main = expression(paste("Verosimilitud perfil de ", beta[0])),
     ylab = expression(paste(Rp(beta[0]))),
     xlab = expression(paste(beta[0])))
segments(x0 = ext090[1], y0 = c090, x1 = ext090[2], y1 = c090, col = "steelblue2", lwd = 2.5)
segments(x0 = ext092[1], y0 = c092, x1 = ext092[2], y1 = c092, col = "steelblue2", lwd = 2.5)
segments(x0 = ext095[1], y0 = c095, x1 = ext095[2], y1 = c095, col = "steelblue2", lwd = 2.5)
segments(x0 = ext099[1], y0 = c099, x1 = ext099[2], y1 = c099, col = "steelblue2", lwd = 2.5)
abline(v = b0g, lty = 2, col = "red")
legend("topright", legend = expression(paste(hat(beta)[0])),
       lwd = 2, lty = 2, col = "red")
text(ext090[1]-0.08, c090+0.02, expression(c == 0.2512), cex = 0.75)
text(ext092[1]-0.08, c092+0.015, expression(c == 0.2090), cex = 0.75)
text(ext095[1]-0.08, c095+0.01, expression(c == 0.1406), cex = 0.75)
text(ext099[1]-0.08, c099+0.02, expression(c == 0.0337), cex = 0.75)



# Grafica de verosimilitud perfil de beta_1
y <- c()
x = seq(0.05, 0.33, length.out = 1000)
for(i in 1:1000){
  y[i] = Rpb1(x[i])
}
plot(x, y, type = "l", main = expression(paste("Verosimilitud perfil de ", beta[1])),
     ylab = expression(paste(Rp(beta[1]))),
     xlab = expression(paste(beta[1])))
segments(x0 = ext190[1], y0 = c190, x1 = ext190[2], y1 = c190, col = "steelblue2", lwd = 2.5)
segments(x0 = ext192[1], y0 = c192, x1 = ext192[2], y1 = c192, col = "steelblue2", lwd = 2.5)
segments(x0 = ext195[1], y0 = c195, x1 = ext195[2], y1 = c195, col = "steelblue2", lwd = 2.5)
segments(x0 = ext199[1], y0 = c199, x1 = ext199[2], y1 = c199, col = "steelblue2", lwd = 2.5)
abline(v = b1g, lty = 2, col = "red")
legend("topright", legend = expression(paste(hat(beta)[1])),
       lwd = 2, lty = 2, col = "red")
text(ext190[1]-0.01, c190+0.029, expression(c == 0.2512), cex = 0.75)
text(ext192[1]-0.01, c192+0.024, expression(c == 0.2090), cex = 0.75)
text(ext195[1]-0.01, c195+0.021, expression(c == 0.1406), cex = 0.75)
text(ext199[1]-0.01, c199+0.02, expression(c == 0.0337), cex = 0.75)

q11 = XX[1,] %*% bg + qt(0.975, n - 2, T) * sqrt(sigmag * (XX[1,] %*% solve(XXt %*% XX) %*% XX[1,])) * sqrt((n-2) / n)
q12 = XX[1,] %*% bg - qt(0.975, n - 2, T) * sqrt(sigmag * (XX[1,] %*% solve(XXt %*% XX) %*% XX[1,])) * sqrt((n-2) / n)

q21 = XX[1,] %*% bg + qt(0.975, n - 2, T) * sqrt(sigmag * (1 + (XX[1,] %*% solve(XXt %*% XX) %*% XX[1,]))) * sqrt((n-2) / n)
q22 = XX[1,] %*% bg - qt(0.975, n - 2, T) * sqrt(sigmag * (1 + (XX[1,] %*% solve(XXt %*% XX) %*% XX[1,]))) * sqrt((n-2) / n)


# Contornos de la funcion de verosimilitud de (mu, sigma)
#plotRelative(rlogver, b0g, b1g, 98.7, 100.6, 0.05, 0.33, c(0.01, 0.05, 0.10), 1000)
#plot.function(x = function(t) emvrb1(t),
#              from = 98.3, #Rango
#              to = 100.8, lwd = 1.5,
#              col = "blue",
#              add = T)
#plot.function(x = function(t) Iemvrb0(t),
#              from = 98.3, #Rango
#              to = 100.8, lwd = 1.5,
#              col = "blue",
#              add = T)
#points(b0g, b1g, type = "p", pch = 8, col = "red")
#segments(x0 = ext095[1], y0 = 0, x1 = ext095[1], y1 = 1, col = "grey", lwd = 1.5, lty = 2)
#segments(x0 = ext095[2], y0 = 0, x1 = ext095[2], y1 = 1, col = "grey", lwd = 1.5, lty = 2)
#segments(x0 = 92, y0 = ext195[1], x1 = 105, y1 = ext195[1], col = "grey", lwd = 1.5, lty = 2)
#segments(x0 = 92, y0 = ext195[2], x1 = 105, y1 = ext195[2], col = "grey", lwd = 1.5, lty = 2)
#legend("topright", legend=expression(paste("(", hat(beta)[0],"," ,hat(beta)[1],")")),
#       pch = 8, col = "red")