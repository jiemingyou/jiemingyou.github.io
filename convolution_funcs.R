plot_raster <- function(raster) {
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot.new()
  as.raster(raster) |>
    rasterImage(xleft = 0, xright = 1, ytop = 0, ybottom = 1)
}


edge_extension <- function(mat, pad) {
  n <- nrow(mat)
  m <- ncol(mat)
  
  top   <- c(rep(mat[1,1], pad), mat[1,], rep(mat[1,m], pad))
  bot   <- c(rep(mat[n,1], pad), mat[n,], rep(mat[n,m], pad))
  left  <- matrix(rep(mat[1:n, 1], pad), ncol=pad)
  right <- matrix(rep(mat[1:n, m], pad), ncol=pad)
  mid   <- cbind(left, mat[1:n,], right)
  
  new_mat <- rbind(
    matrix(rep(top, pad), nrow=pad, byrow=TRUE),
    mid,
    matrix(rep(bot, pad), nrow=pad, byrow=TRUE)
  )
  
  return(new_mat)
}

rolling_kernel <- function(f, M) {
  
  x <- ncol(f)
  y <- nrow(f)
  m <- ncol(M)
  n <- nrow(M)
  g <- f # g(x,y)
  
  # Kernel dimensions exceed the source
  if (m > x ||  n  > y) {
    stop("Kernel length exceeds the source")
  }
  
  g_extended <- edge_extension(f, (m-1))
  
  for (i in 1:x) {
    for (j in 1:y) {
      f_kernel <- g_extended[i:(i+(m-1)), j:(j+(n-1))]
      g_kernel <- f_kernel * M
      g[i, j] <- sum(g_kernel)
    }
  }
  
  return(g)
}


rgb_kernel <- function(raster, kernel) {
  channels <- dim(raster)[3]
  output <- raster
  
  for (ch in 1:channels) {
    output[,,ch] <- rolling_kernel(raster[,,ch], kernel)
  }
  
  # Scaling
  output_reshape <- matrix(output, nrow = ncol(raster))
  output_scaled  <- rank(output_reshape) / length(output_reshape)
  dim(output_scaled) <- c(dim(raster))
  
  return(output_scaled)
}


gaussian_kernel <- function(width, sigma = 1) {
  if (width%%2 != 1) { stop("Even width parameter") }
  
  # Boundaries
  b  <- (width-1)/2
  a <- -b
  
  # Setting up the linspace
  ax <- seq(a, b, length = width)
  
  # Draw from gaussian CDF
  u <- dnorm(ax, sd = sigma)
  
  # Outer product uu^T and normalization
  M <- u %o% u
  M <- M / sum(M)
  
  return(M)
}