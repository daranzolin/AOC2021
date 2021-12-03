####################################################
###################### DAY 3 ######################
###################################################
bits <- readLines("inputs/input03.txt")

# Part 1
bool_to_binary_to_decimal <- function(x, negate = FALSE) {
  if (negate) {
    out <- paste(as.numeric(!x), collapse = "")
  } else {
    out <- paste(as.numeric(x), collapse = "")
  }
  strtoi(out, base = 2)
}

bits_mat <- matrix(as.numeric(unlist(strsplit(bits, ""))), ncol = 12, byrow = TRUE)
bool <- apply(bits_mat, MARGIN = 2, function(x) {
  counts <- table(x)
  counts[2] > counts[1]
})

gamma <- bool_to_binary_to_decimal(bool)
epsilon <- bool_to_binary_to_decimal(bool, negate = TRUE)
gamma * epsilon

# Part 2
count_col <- function(mat, ind) {
  table(mat[,ind])
}

subset_mat <- function(mat, ind, which) {
  mat[mat[,ind] == which,, drop = FALSE]
}

oxymat <- bits_mat
for (i in 1:ncol(oxymat)) {
  col_counts <- count_col(oxymat, i)
  if (length(col_counts) == 1) {
    to_subset <- as.numeric(names(col_counts[1]))
  } else {
    to_subset <- ifelse(col_counts[2] >= col_counts[1], 1, 0)
  }
  oxymat <- subset_mat(oxymat, i, to_subset)
}
oxygen_rating <- strtoi(paste(as.vector(oxymat), collapse = ""), base = 2)

co2mat <- bits_mat
for (i in 1:ncol(co2mat)) {
  col_counts <- count_col(co2mat, i)
  if (length(col_counts) == 1) {
    to_subset <- as.numeric(names(col_counts[1]))
  } else {
    to_subset <- ifelse(col_counts[2] >= col_counts[1], 0, 1)
  }
  co2mat <- subset_mat(co2mat, i, to_subset)
}
co2_rating <- strtoi(paste(as.vector(co2mat), collapse = ""), base = 2)

oxygen_rating*co2_rating
