ages <- scan("inputs/input06.txt", sep = ",")

# Part 1
for (i in 1:80) {
  zeroes <- sum(ages == 0)
  ages[ages == 0] <- 7
  ages <- ages - 1
  ages <- append(ages, rep(8, zeroes))
}
length(ages)

# Part 2
