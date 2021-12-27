# function that generates matrix with all 2^n possible unique variations of 0s and 1s for n digits
n_points <- function(n) {
  coords = list()
  for (i in 1:n) {
    coords = c(coords, rep(c(rep(0, (2^n)/(2^i)), rep(1, (2^n)/(2^i))), 2^(i-1)))
  }  
  matrix(as.numeric(coords), ncol = n)
}

# generates 4x2 matrix with coordinates of square and optional x and y offsets
# offsets 0 for x and y results in square with points
# P1 = (-0.5|-0.5), P2 = (-0.5|0.5), P3 = (0.5|0.5), P4 = (0.5|-0.5)
get_square_offset <- function(x_offset = 0, y_offset = 0) {
  base_matrix <- matrix(c(-0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5, -0.5), nrow = 4, ncol = 2)
  base_matrix[,1] <- base_matrix[,1]+x_offset
  base_matrix[,2] <- base_matrix[,2]+y_offset
  base_matrix
}

# draws n squares with custom offsets
draw_n_squares <- function(n = 4, offsets = matrix(c(-1, -1, 1, 1, -1, 1, 1, -1), nrow = 4, ncol = 2)) {
  
  # matrix for all squares
  all_squares <- matrix(nrow = 4*n, ncol = 2)
  
  # adding points for n = 4 squares to all_squares matrix
  for (i in 1:n) {
    all_squares[(4*i-3):(4*i), ] <- get_square_offset(offsets[i, 1], offsets[i, 2])
  }
  
  # plot points
  plot(all_squares, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")
  
  # plot lines for each square
  for (square_number in 1:n) {
    beginning_square_index <- 4*square_number-3
    ending_square_index <- 4*square_number
    for (point_number in 0:3) {
      segments(all_squares[beginning_square_index+point_number, 1],
               all_squares[beginning_square_index+point_number, 2],
               all_squares[beginning_square_index+((point_number+1)%%4), 1],
               all_squares[beginning_square_index+((point_number+1)%%4), 2])
    }
  }
  
  # plot lines inbetween squares
  for (i in 1:(4*(n-1))) {
    segments(all_squares[i, 1],
             all_squares[i, 2],
             all_squares[i+4, 1],
             all_squares[i+4, 2],)
  }
  
  # connect last and first square
  for (i in 1:4) {
    segments(all_squares[(4*(n-1)+i), 1],
             all_squares[(4*(n-1)+i), 2],
             all_squares[i, 1],
             all_squares[i, 2])
  }
}

# draws 4-cube
draw_hypercube <- function() {
  draw_n_squares(4, matrix(c(0, 1.5, 2, 0.5, 0, 1, 0.5, -0.5), nrow = 4, ncol = 2))
}
