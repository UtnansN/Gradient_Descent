# Funkcijas, precizitātes un soļa lieluma definīcijas
my_expr <- expression(x-2*y+x^2+2*x*y+4*y^2)
precision <- c(0.01, 0.1)
step_size <- c(0.01, 0.05, 0.1, 0.15, 0.22, 0.23)
# Iterācijas VS novirze dažādu soļu izpētes modelis
# step_size <- seq(0.002, 0.23, by = 0.002)

# Optimums tiek ierakstīts šeit, lai varētu veidot diagrammas ar optimuma / iterāciju salīdzinājumu
func_opt <- c(-1, 0.5)

# Te var izvēlēties izlaist diagrammu konstruēšanu, ja tos ir jākonstruē pārāk daudz
skip_3d_plots <- FALSE
skip_barcharts <- FALSE
skip_iteration_deviation_plot <- FALSE

# Lai pārbaudītu visas precizitātes/soļa garuma kombinācijas, vajag realizēt dekarta reizinājumu
prec_step_matrix <- matrix(nrow = 0, ncol = 2)
for (i in seq(1, length(precision))) {
  for (j in seq(1, length(step_size))) {
    prec_step_matrix <- rbind(prec_step_matrix, c(precision[i], step_size[j]))
  }
}

# Funkcijas definēšana un atvasināšana, lai iegūtu gradienta komponentes.
my_function <- function(x, y) {}
body(my_function) <- my_expr
x_deriv <- D(my_expr, 'x')
y_deriv <- D(my_expr, 'y')

# Trīs pētāmo punktu definēšana.
x_initial <- c(-1.5, -2, 2.4)
y_initial <- c(-1, 2, 2.3)
z_initial <- mapply(my_function, x_initial, y_initial)

# Grafika izveidošana (tuvināšanās trajektorijas analīzei)
if (!skip_3d_plots) {
  x_plot <- y_plot <- seq(-3, 3, length= 30)
  z_plot <- outer(x_plot, y_plot, my_function)
  func_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)",
                      theta = 65, phi = 25, main = "Sakuma punkti", ticktype = "detailed")
  points(trans3d(x_initial, y_initial, z_initial, pmat=func_graph), pch = 21, col="black", bg="orange")
}

sol_ledger <- list()

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
    # Katra atrisinājumu matrica tiek glabāta kopējā sarakstā. Soļu matrica tiek ievietota datu freimā un pie tās pielikti
    # metadati parametru apskatei.
    sol_ledger[[offset + i]] <- data.frame(step_matrix)
    colnames(sol_ledger[[offset + i]]) <- c("x1", "x2", "f(x1, x2)")
    comment(sol_ledger[[offset + i]]) <- paste("x1 = ", x_initial[i], ", x2 = ", y_initial[i],
                                               ", t = ", prec_step_matrix[j, 2], ", e = ", prec_step_matrix[j, 1])
    
    # Šajā daļā tiek konstruētas 3d diagrammas katram x1 x2 sākuma punktu pārim. Grafikos arī tiek pievienoti 
    # visi aprēķinātie punkti.
    if (!skip_3d_plots) {
    curr_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)", theta = 65, phi = 25, 
                        main = paste("Optimizacija sakuma punktiem:\n x1 = ", x_initial[i], " , x2 = ", y_initial[i],
                                     "\nt = ", prec_step_matrix[j, 2], " , e = ", prec_step_matrix[j, 1]), 
                        sub = paste("Solu skaits: ", nrow(step_matrix) - 1), ticktype = "detailed")
    
    points(trans3d(step_matrix[1,1], step_matrix[1,2], 
                   step_matrix[1,3], pmat = curr_graph), pch = 21, col ="black",bg = "red")
    
    points(trans3d(step_matrix[-1,1], step_matrix[-1,2], step_matrix[-1,3], pmat = curr_graph),
           pch = 21, col ="black",bg = "orange")
    
    points(trans3d(step_matrix[nrow(step_matrix),1], step_matrix[nrow(step_matrix),2], 
                   step_matrix[nrow(step_matrix),3], pmat = curr_graph), pch = 21, col ="black",bg = "blue")
    remove(curr_graph)
    }
  }
}
remove(x, y, x_grad, y_grad)

# Cikls iterāciju un novirzes diagrammu izveidošanai
if (!skip_iteration_deviation_plot) {
  # Cikls katrai precizitatei
  for (i in seq(1, length(precision))) {
    offset_ledger <- length(x_initial) * length(step_size) * (i - 1)
    # Cikls katram koordināšu pārim
    for (j in seq(1, length(x_initial))) {
      x_opt_offset <- vector(length = length(step_size))
      y_opt_offset <- vector(length = length(step_size))
      iter_ct <- vector(length = length(step_size))
      # Cikls lai iegūtu katru vērtību, lai to ievietotu datu vektoros
      for (k in seq(1, length(step_size))) {
        offset_step <- length(x_initial) * (k - 1)
        work_ledger <- sol_ledger[[offset_ledger + offset_step + j]]
        
        x_opt_offset[[k]] <- abs(work_ledger[nrow(work_ledger),1] - func_opt[1])
        y_opt_offset[[k]] <- abs(work_ledger[nrow(work_ledger),2] - func_opt[2])
        iter_ct[[k]] <- nrow(work_ledger) - 1
      }
      plot(x_opt_offset, iter_ct, xlab = "absoluta kluda", ylab = "iteraciju skaits", col = "black", bg = "blue",
           pch = 21, main = paste("Iteracijas VS novirze\ne = ", 
                                  precision[i], " , x1 = ", x_initial[j], " , x2 = ", y_initial[j]))
      points(y_opt_offset, iter_ct, col = "black", bg = "red", pch = 21)
      legend("topleft", legend = c("x1", "x2"), fill = c("blue", "red"), bty = "n")
    }
  }
  remove(x_opt_offset, y_opt_offset, offset_step)
}

# Cikls diagrammu izveidei pie dažādām precizitātēm
if (!skip_barcharts) {
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
    
    # Stabiņu diagrammas izveide. Par() funkcija tiek izmantota, lai pielāgotu sānu malas, lai varētu normāli formatēt
    # soļa lielumu sarakstu.
    opar <- par(oma = c(0,0,0,4))
    curr_bar_plot <- barplot(my_data, main=paste("Iteracijas VS sola lielums pie e = ", precision[k]),
                         beside = TRUE, xlab = "Punkti", ylab = "Iteracijas", border = "black")
    par(opar)
    
    opar <- par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
    legend(x = "right", legend = rownames(my_data),fill = gray.colors(length(step_size)), 
           y.intersp = 2, title = "t", bty = "n")
    par(opar)
    remove(opar)
  }
  remove(k, offset_ledger, point_names)
}
remove(i, j, offset, x_plot, y_plot, z_plot, skip_3d_plots, skip_barcharts, skip_iteration_deviation_plot)