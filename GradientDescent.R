# Function, precision and step size definitions
my_expr <- expression(x-2*y+x^2+2*x*y+4*y^2)
precision <- c(0.01, 0.1)
step_size <- c(0.025, 0.05, 0.1, 0.15)

# Choose to skip plots here
skip_3d_plots <- FALSE
skip_barcharts <- FALSE

prec_step_matrix <- matrix(nrow = 0, ncol = 2)
for (i in seq(1, length(precision))) {
  for (j in seq(1, length(step_size))) {
    prec_step_matrix <- rbind(prec_step_matrix, c(precision[i], step_size[j]))
  }
}

# Derivatives
my_function <- function(x, y) {}
body(my_function) <- my_expr
x_deriv <- D(my_expr, 'x')
y_deriv <- D(my_expr, 'y')

# Three starting point definitions
x_initial <- c(-1.5, -2, 2.4)
y_initial <- c(-1, 2, 2.3)
z_initial <- mapply(my_function, x_initial, y_initial)

if (!skip_3d_plots) {
  x_plot <- y_plot <- seq(-3, 3, length= 30)
  z_plot <- outer(x_plot, y_plot, my_function)
  func_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)",
                      theta = 65, phi = 25, main = "Start points", ticktype = "detailed")
  points(trans3d(x_initial, y_initial, z_initial, pmat=func_graph), pch = 21, col="black", bg="orange")
  remove(func_graph)
}

sol_ledger <- list()

# Cycle for every step/precision pair
for (j in seq(1, nrow(prec_step_matrix))) {
  offset <- length(x_initial) * (j - 1)
  # Cycle for each starting point
  for (i in seq(1, length(x_initial))) {
  x <- x_initial[i]
  y <- y_initial[i]
  step_matrix <- matrix(data = c(x, y, my_function(x, y)), nrow = 1, ncol = 3)
    while (TRUE) {
      x_grad <- eval(x_deriv)
      y_grad <- eval(y_deriv)
      if ((abs(x_grad) <= prec_step_matrix[j, 1] & abs(y_grad) <= prec_step_matrix[j, 1])) {
        break()
      }
      x <- x - x_grad * prec_step_matrix[j, 2]
      y <- y - y_grad * prec_step_matrix[j, 2]
      step_matrix <- rbind(step_matrix, c(x, y, my_function(x, y)))
    }
    sol_ledger[[offset + i]] <- data.frame(step_matrix)
    colnames(sol_ledger[[offset + i]]) <- c("x1", "x2", "f(x1, x2)")
    comment(sol_ledger[[offset + i]]) <- paste("x1 = ", x_initial[i], ", x2 = ", y_initial[i],
                                               ", t = ", prec_step_matrix[j, 2], ", e = ", prec_step_matrix[j, 1])

    if (!skip_3d_plots) {
    curr_graph <- persp(x_plot, y_plot, z_plot, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)", theta = 65, phi = 25, 
                        main = paste("Optimization with starting point:\n x1 = ", x_initial[i], " , x2 = ", y_initial[i],
                                     "\nt = ", prec_step_matrix[j, 2], " , e = ", prec_step_matrix[j, 1]), 
                        sub = paste("Step count: ", nrow(step_matrix) - 1), ticktype = "detailed")
    
    points(trans3d(step_matrix[1,1], step_matrix[1,2], 
                   step_matrix[1,3], pmat = curr_graph), pch = 21, col ="black",bg = "red")
    
    points(trans3d(step_matrix[-1,1], step_matrix[-1,2], step_matrix[-1,3], pmat = curr_graph),
           pch = 21, col ="black",bg = "orange")
    
    points(trans3d(step_matrix[nrow(step_matrix),1], step_matrix[nrow(step_matrix),2], 
                   step_matrix[nrow(step_matrix),3], pmat = curr_graph), pch = 21, col ="black",bg = "blue")
    remove(curr_graph)
    }
    remove(step_matrix)
  }
}
if (!skip_3d_plots) {
  remove(x_plot, y_plot, z_plot)
}
remove(x, y, x_grad, y_grad)

# Cycle for each precision diagram
if (!skip_barcharts) {
  for (k in seq(1, length(precision))) {
    offset_ledger <- length(x_initial) * length(step_size) * (k - 1)
    
    my_data <- matrix(nrow = length(step_size), ncol = length(x_initial))
    rownames(my_data) <- step_size
    
    point_names <- vector()
    for (i in seq(1, length(x_initial))) {
      point_names <- c(point_names, paste("(", x_initial[i], "; ", y_initial[i], ")"))
    }
    colnames(my_data) <- point_names

    for (i in seq(1, length(step_size))) {
      offset <- length(x_initial) * (i - 1)
      for (j in seq(1, length(x_initial))) {
        my_data[i, j] <- nrow(sol_ledger[[offset_ledger + offset + j]]) - 1
      }
    }

    # Barchart generation
    opar <- par(oma = c(0,0,0,4))
    curr_bar_plot <- barplot(my_data, main=paste("Iterations VS step size at e = ", precision[k]),
                         beside = TRUE, xlab = "Points", ylab = "Iterations", border = "black")
    par(opar)
    
    opar <- par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
    legend(x = "right", legend = rownames(my_data),fill = gray.colors(length(step_size)), 
           y.intersp = 2, title = "t", bty = "n")
    par(opar)
    remove(opar)
  }
  remove(k, offset_ledger, point_names, curr_bar_plot, my_data)
}
remove(i, j, offset, skip_3d_plots, skip_barcharts, prec_step_matrix)