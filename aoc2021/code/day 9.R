input <- read.fwf("data/day9_input1.txt", c(rep(1,100)), header = FALSE) |>
  as.matrix()

#' Part one Let's use R's vectorized operations to save time
#' If a point is lower than the 4 points around itthen it's a local minima
find_local_minima <- function(mat){
  rows <- nrow(mat)
  cols <- ncol(mat)

  combined <-
  rbind(input[1:(rows-1),] < input[2:rows,], TRUE) &
  rbind(TRUE, input[2:rows,] < input[1:(rows-1),]) &
  cbind(input[,1:(cols-1)] < input[,2:cols], cols) &
  cbind(cols, input[,2:cols] < input[,1:(cols-1)])


}

(input[find_local_minima(input)]+1) |> sum()


#' Part 2
#' So each basin would be surrounding the minima
#' the walls are at 9s
#' Adapted the logic from @TeaStats on twitter after using a image package to create rater groups
find_basins <- function(mat){
  rows <- nrow(mat)
  cols <- ncol(mat)

  minima <- find_local_minima(mat)
  mat[] <- ifelse(mat < max(mat), NA, Inf)
  mat[minima] <- 1:sum(minima)

  na_to_finite_neighbor <- function(x,y){
    ifelse(
      (!is.na(x) | is.infinite(x)), x,
        ifelse(
          !is.infinite(y), y, x  )
      )

  }

  while (anyNA(mat)) {
    mat <- mat |>
      na_to_finite_neighbor(cbind(mat, NA)[,-1] )  |>#right
      na_to_finite_neighbor(cbind(NA, mat[,-cols]))  |>#left
      na_to_finite_neighbor(rbind(NA, mat[-rows,]))  |> #up
      na_to_finite_neighbor(rbind(mat, NA)[-1,] )    #down


  }


  return(
    mat[is.finite(mat)] |>
      table()|>
      sort(decreasing = TRUE)
    )

}

`find_basins(input) |>
  head(3) |>
  prod()
