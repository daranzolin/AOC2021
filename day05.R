coords <- readLines("inputs/input05.txt")

# Part 1
coords <- strsplit(coords, "[,>-]")
coords <- lapply(coords, function(x) setNames(as.numeric(x[c(1, 2, 4, 5)]), c("x1", "y1", "x2", "y2")) + 1)

max_x <- max(sapply(coords, function(x) x[c(1,3)]))
max_y <- max(sapply(coords, function(x) x[c(2, 4)]))
mat <- matrix(0, nrow = max_x, ncol = max_y)

add_layer <- function(mat, coords) {
  if (all(table(coords) == 2)) return(mat)
  if (coords["x1"] == coords["x2"] | coords["y1"] == coords["y2"]) {
    xfill <- coords[1]:coords[3]
    yfill <- coords[2]:coords[4]
    mat[xfill, yfill] <- mat[xfill, yfill] + 1
    return(mat)
  }
  mat
}

p1mat <- mat
for (i in seq_along(coords)) {
  p1mat <- add_layer(p1mat, coords[[i]])
}

sum(p1mat >= 2)

# Part 2
add_layer2 <- function(mat, coords) {
  xfill <- coords[1]:coords[3]
  yfill <- coords[2]:coords[4]
  if (coords["x1"] == coords["x2"] | coords["y1"] == coords["y2"]) {
    mat[xfill, yfill] <- mat[xfill, yfill] + 1
    return(mat)
  }
  diags <- purrr::map2(xfill, yfill, c)
  for (i in seq_along(diags)) {
    mat[diags[[i]][1], diags[[i]][2]] <- mat[diags[[i]][1], diags[[i]][2]] + 1
  }
  mat
}

p2mat <- mat
for (i in seq_along(coords)) {
  p2mat <- add_layer2(p2mat, coords[[i]])
}

sum(p2mat >= 2)
