####################################################
###################### DAY 2 ######################
###################################################
steps <- readLines("inputs/input02.txt")

# Part 1
extract_dir_nums <- function(direction) {
  dirs <- steps[grep(direction, steps)]
  sum(as.numeric(gsub(direction, "", dirs)))
}

extract_dir_nums("forward") * (extract_dir_nums("down") - extract_dir_nums("up"))

# Part 2
library(dplyr)
df <- data.frame(
  step = steps,
  step_val = readr::parse_number(steps)
) %>%
  mutate(
    aim = cumsum(
      case_when(
        grepl("down", step) ~ step_val,
        grepl("up", step) ~ -step_val,
        TRUE ~ 0
      )
    ),
    depth = ifelse(grepl("forward", step), step_val * aim, 0)
  )

sum(df$depth) * extract_dir_nums("forward")
