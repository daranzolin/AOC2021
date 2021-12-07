steps <- as.numeric(strsplit(readLines("inputs/input07.txt"), ",")[[1]])

# Part 1
sum(abs(steps - median(steps)))
