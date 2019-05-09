# Bibliotēka ir praktiski domāta grafa iekrāsošanai. (drapecol() funkcija)
# install.packages("shape")
library(shape)

# Precizitātes un soļa lieluma definīcijas
precision <- 0.01
step_size <- 0.001

# Funkcijas definēšana un atvasināšana, lai iegūtu gradienta komponentes.
my_function <- function(x, y) (x-2*y+x^2+2*x*y+4*y^2)
x_deriv <- D(body(my_function), 'x')
y_deriv <- D(body(my_function), 'y')

# Trīs pētāmo punktu definēšana.
x_initial <- c(-1.5, -2, 2.4)
y_initial <- c(-1, 2, 2.7)
z_initial <- mapply(my_function, x_initial, y_initial)

# Grafika izveidošana (tuvināšanas trajektorijas analīzei)
x_plot <- y_plot <- seq(-3, 3, length= 60)
z_plot <- outer(x_plot, y_plot, my_function)
func_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)", col = drapecol(z_plot, col = heat.colors(100)),
                    theta = 65, phi = 30, main = "f(x1, x2)", ticktype = "detailed")
points(trans3d(x_initial, y_initial, z_initial, pmat=func_graph), pch = 21, col="blue", bg="blue")


for (i in seq(1, length(x_initial)))
counter <- 1
x <- c(x_initial[i])
y <- c(y_initial[i])
z <- c(z_initial[i])
while (TRUE) {
  x_grad <- 0
  y_grad <- 0
  if (x_grad < precision & y_grad < precision) {
    break()
  }
}