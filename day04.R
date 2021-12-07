input_lines <- readLines("inputs/input04.txt")

# Part 1
numbers <- as.numeric(strsplit(input_lines[1], ",")[[1]])
boards <- lapply(strsplit(input_lines[3:length(input_lines)], " "), function(x) x[x != ""])
boards <- Filter(function(x) length(x) > 0, boards)
boards <- split(boards, ceiling(seq_along(boards)/5))
boards <- lapply(boards, function(x) matrix(as.numeric(unlist(x)), nrow = 5, byrow = TRUE))

n_boards <- length(boards)
bool_mat <- matrix(FALSE, nrow = 5, ncol = 5)
bool_boards <- vector("list", length = n_boards)
for (i in seq_along(bool_boards)) bool_boards[[i]] <- bool_mat

get_num_checks_vec <- function(number) {
  num_checks <- vector(length = n_boards)
  for (i in seq_along(boards)) {
    x <- which(boards[[i]] == number)
    num_checks[i] <- ifelse(length(x) == 0, NA, x)
  }
  num_checks
}

update_bool_boards <- function(bb, vec) {
  for (i in seq_along(vec)) {
    bb[[i]][vec[i]] <- TRUE
  }
  bb
}

check_for_winner <- function(bb) {
  for (i in seq_along(bb)) {
    for (j in 1:5) {
      check_rows <- all(bb[[i]][j,])
      check_cols <- all(bb[[i]][,j])
      if (check_rows | check_cols) return(i)
    }
  }
  0
}

bb <- bool_boards
winning_board <- 0
last_number <- 0
for (i in seq_along(numbers)) {
    v <- get_num_checks_vec(numbers[i])
    bb <- update_bool_boards(bb, v)
    winning_board <- check_for_winner(bb)
    last_number <- numbers[i]
    if (winning_board != 0) break
}

last_number * sum(boards[[winning_board]][!bb[[winning_board]]])

# Part 2
# check_for_winners <- function(bb) {
#   winning_boards <- c()
#   for (i in seq_along(bb)) {
#     for (j in 1:5) {
#       check_rows <- all(bb[[i]][j,])
#       check_cols <- all(bb[[i]][,j])
#       if (check_rows | check_cols) winning_boards <- append(winning_boards, i)
#     }
#   }
#   list(winning_boards)
# }
#
# bb <- bool_boards
# winning_board <- 0
# winning_boards_lst <- list()
# for (i in seq_along(numbers)) {
#   v <- get_num_checks_vec(numbers[i])
#   bb <- update_bool_boards(bb, v)
#   winning_boards_lst <- append(winning_boards_lst, check_for_winners(bb))
# }
#
# last_board_to_win <- as.numeric(names(sort(table(unlist(winning_boards_lst)))[1]))
# num_seq <- sapply(winning_boards_lst, function(x) last_board_to_win %in% x)
# last_board_won_after <- min(which(num_seq == TRUE)) - 1
#
# bb <- bool_boards
# winning_board <- 0
# last_number <- 0
# for (i in 1:last_board_won_after) {
#   v <- get_num_checks_vec(numbers[i])
#   bb <- update_bool_boards(bb, v)
#   winning_board <- check_for_winner(bb)
#   last_number <- numbers[i]
# }
#
# last_number * sum(boards[[last_board_to_win]][!bb[[last_board_to_win]]])

