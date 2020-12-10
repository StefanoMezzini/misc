# dice ----
d2 <- function(modifier = 0) {
  sample(1:2, 1) + modifier
}

d4 <- function(modifier = 0) {
  sample(1:4, 1) + modifier
}

d6 <- function(modifier = 0) {
  sample(1:6, 1) + modifier
}

d8 <- function(modifier = 0) {
  sample(1:8, 1) + modifier
}

d10 <- function(modifier = 0) {
  sample(1:10, 1) + modifier
}

d12 <- function(modifier = 0) {
  sample(1:12, 1) + modifier
}

d20 <- function(modifier = 0) {
  x <- sample(1:20, 1)
  
  if(x == 20) warning('Critical success!')
  if(x == 1) warning('Critical fail!')
  x + modifier
}

d100 <- function(modifier = 0) {
  sample(1:100, 1) + modifier
}

dcustom <- function(n, modifier = 0) {
  sample(1:n, 1) + modifier
}

# advantage ----
adv <- function(dice, modifier = 0) {
  x <- sort(c(suppressWarnings(dice()),
              suppressWarnings(dice())),
            decreasing = TRUE)
  
  if(dice == d20) {
    if(max(x) == 20) warning('Critical success!')
    if(max(x) == 1) warning('Critical fail!')
  }
  
  x + modifier
}

# disadvantage ----
dis <- function(dice, modifier = 0) {
  x <- sort(c(dice(), dice()), decreasing = FALSE)
  
  if(dice == d20) {
    if(min(x) == 20) warning('Critical success!')
    if(min(x) == 1) warning('Critical fail!')
  }
  
  x + modifier
}

# high variance dice
high.v <- function(sides = 20, modifier = 0) {
  sample()
}

p.highv <- c(3, 2, 2, 1, 1, 1, 1,  1,  1,  2,  2,  3) / 20
