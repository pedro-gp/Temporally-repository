x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan(x,y)
plot3d(x,y,z,col = rainbow(1000), type = 'p')


for(i in 1:m){
  phi[i] = rbeta(n = m,shape1 = 2,shape2 = 2)
  y[i] = rbinom(n = m,size = 10,prob = phi[i])
}

phi = rbeta(n = m,shape1 = 2,shape2 = 2)
y = rbinom(n=m,size = 10,prob = phi)

# Rendimiento promedio de la población
mu = 500
x_hat = 518
n = 25
s = 40
df = n - 1


