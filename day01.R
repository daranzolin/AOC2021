####################################################
###################### DAY 1 ######################
###################################################
lines <- as.numeric(readLines("inputs/input01.txt"))

# Part 1
sum(dplyr::lead(lines) > lines, na.rm = TRUE)

# Part 2
threesums <- slider::slide_dbl(lines, sum, .after = 2, .step = 1)
sum(dplyr::lead(threesums) > threesums, na.rm = TRUE)
