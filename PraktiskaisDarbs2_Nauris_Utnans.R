# Precizitātes un soļa lieluma definīcijas
precision <- c(0.01, 0.1)
step_size <- c(0.01, 0.05, 0.1, 0.15, 0.22, 0.23)

# Lai pārbaudītu visas precizitātes/soļa garuma kombinācijas, vajag realizēt dekarta reizinājumu
prec_step_matrix <- matrix(nrow = 0, ncol = 2)
for (i in seq(1, length(precision))) {
  for (j in seq(1, length(step_size))) {
    prec_step_matrix <- rbind(prec_step_matrix, c(precision[i], step_size[j]))
  }
}

# Funkcijas definēšana un atvasināšana, lai iegūtu gradienta komponentes.
my_expr <- expression(x-2*y+x^2+2*x*y+4*y^2)
my_function <- function(x, y) {}
body(my_function) <- my_expr
x_deriv <- D(my_expr, 'x')
y_deriv <- D(my_expr, 'y')

# Trīs pētāmo punktu definēšana.
x_initial <- c(-1.5, -2, 2.4)
y_initial <- c(-1, 2, 2.3)
z_initial <- mapply(my_function, x_initial, y_initial)

# Grafika izveidošana (tuvināšanās trajektorijas analīzei)
x_plot <- y_plot <- seq(-2.5, 2.5, length= 50)
z_plot <- outer(x_plot, y_plot, my_function)
func_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)",
                    theta = 65, phi = 25, main = "Sakuma punkti", ticktype = "detailed")
points(trans3d(x_initial, y_initial, z_initial, pmat=func_graph), pch = 21, col="black", bg="orange")

sol_ledger <- list()
graphs <- list()

# Cikls katra soļa/precizitātes pāra apstrādei
for (j in seq(1, nrow(prec_step_matrix))) {
  offset <- length(x_initial) * (j - 1)
  # Cikls katra sākuma punkta apstrādei
  for (i in seq(1, length(x_initial))) {
  x <- x_initial[i]
  y <- y_initial[i]
  # Te tiek definēta soļu matrica
  step_matrix <- matrix(data = c(x, y, my_function(x, y)), nrow = 1, ncol = 3)
    # Ciklā atkārtoti tiek aprēķināta gradienta x un y komponente. 
    while (TRUE) {
      x_grad <- eval(x_deriv)
      y_grad <- eval(y_deriv)
      if ((abs(x_grad) <= prec_step_matrix[j, 1] & abs(y_grad) <= prec_step_matrix[j, 1])) {
        break()
      }
      # Ja precizitāte joprojām nav sasniegta, tekošās x un y koordinātas tiek uzlabotas.
      # Formula x' = x' - gradients(x) * t (minimizācijas gadījumā)
      x <- x - x_grad * prec_step_matrix[j, 2]
      y <- y - y_grad * prec_step_matrix[j, 2]
      step_matrix <- rbind(step_matrix, c(x, y, my_function(x, y)))
    }
    # Katra atrisinājumu matrica tiek glabāta kopējā sarakstā.
    sol_ledger[[offset + i]] <- step_matrix
    
    # Šajā daļā tiek konstruēti grafiki katram x1 x2 šākuma punktu pārim. Grafikos arī tiek pievienoti 
    # visi aprēķinātie punkti.
    graphs[[offset + i]] <- curr_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)", theta = 65, phi = 25, 
                        main = paste("Optimizacija sakuma punktiem:\n x1 = ", x_initial[i], " , x2 = ", y_initial[i],
                                     "\nt = ", prec_step_matrix[j, 2], " , e = ", prec_step_matrix[j, 1]), 
                        sub = paste("Solu skaits: ", nrow(step_matrix) - 1), ticktype = "detailed")
    points(trans3d(step_matrix[,1], step_matrix[,2], step_matrix[,3], pmat = curr_graph),
           pch = 21, col ="black",bg = "orange")
  }
}



# Cikls diagrammu izveidei pie dažādām precizitātēm
for (k in seq(1, length(precision))) {
  offset_ledger <- length(x_initial) * length(step_size) * (k - 1)
  
  my_data <- matrix(nrow = length(step_size), ncol = length(x_initial))
  rownames(my_data) <- step_size
  
  point_names = vector()
  for (i in seq(1, length(x_initial))) {
    point_names <- c(point_names, paste("(", x_initial[i], "; ", y_initial[i], ")"))
  }
  colnames(my_data) <- point_names
  
  # Atbilstošo datu ievade datu objektā
  for (i in seq(1, length(step_size))) {
    offset <- length(x_initial) * (i - 1)
    for (j in seq(1, length(x_initial))) {
      my_data[i, j] <- nrow(sol_ledger[[offset_ledger + offset + j]]) - 1
    }
  }
  
  # Stabiņu diagrammas izveide
  opar <- par(oma = c(0,0,0,4))
  curr_bar_plot <- barplot(my_data, main=paste("Iteracijas VS sola lielums pie e = ", precision[k]),
                       beside = TRUE, xlab = "Punkti", ylab = "Iteracijas", border = "black")
  par(opar)
  
  opar <- par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
  legend(x = "right", legend = rownames(my_data),fill = gray.colors(length(step_size)), 
         y.intersp = 2, title = "t", bty = "n")
  par(opar)
}

