# Work out the payouts for signal games
# First Nature chooses A or B
# Then P1 chooses L or R
# Then P2 chooses U or D
# P1 strategy is c(X, Y) meaning X if A, Y if B
# P2 strategy is c(X, Y) meaning X if L, Y if R

require(tidyverse)


set.seed(4)


# Building the tree

# What Nature does
n <- data.frame(n=c("A","B"))

# What P1 does 
p1 <- data.frame(p1=c("L","R"))

# What P2 does 
p2 <- data.frame(p2=c("U","D"))

tree <- crossing(n, p1, p2) |>
  mutate(pay1 = floor(runif(8)*5)) |>
  mutate(pay2 = floor(runif(8)*5))

# Determining Strategies

# What Nature does
n <- data.frame(n=c("A","B"))

# What P1 does if A
p1a <- data.frame(p1a=c("L","R"))
# What P1 does if B
p1b <- data.frame(p1b=c("L","R"))

# What P2 does if L
p2a <- data.frame(p2a=c("U","D"))
# What P1 does if R
p2b <- data.frame(p2b=c("U","D"))

strats <- crossing(n, p1a, p1b, p2a, p2b)

# Working out payouts

strats <- strats |> mutate(pay1 = 0, pay2 = 0)
for (i in 1:nrow(strats)){
  this_n <- strats$n[i]

  if (this_n == "A"){
    this_p1 <- strats$p1a[i]
  }
  else{
    this_p1 <- strats$p1b[i]
  }
  
  if (this_p1 == "L"){
    this_p2 <- strats$p2a[i]
  }
  else{
    this_p2 <- strats$p2b[i]
  }  
  tree_part <- tree |>
    filter(n == this_n, p1 == this_p1, p2 == this_p2)
  strats$pay1[i] <- tree_part$pay1[1]
  strats$pay2[i] <- tree_part$pay2[1]
}

# Now build table without A

a_prob <- 0.6
exp_strats <- crossing(p1a, p1b, p2a, p2b) |> mutate(pay1 = 0, pay2 = 0, e1 = 0, e2 = 0)
for (i in 1:nrow(exp_strats)){
  a_pay_1 <- filter(strats, 
                  n == "A",
                  p1a == exp_strats$p1a[i],
                  p1b == exp_strats$p1b[i],
                  p2a == exp_strats$p2a[i],
                  p2b == exp_strats$p2b[i])$pay1
  b_pay_1 <- filter(strats, 
                  n == "B",
                  p1a == exp_strats$p1a[i],
                  p1b == exp_strats$p1b[i],
                  p2a == exp_strats$p2a[i],
                  p2b == exp_strats$p2b[i])$pay1
  a_pay_2 <- filter(strats, 
                    n == "A",
                    p1a == exp_strats$p1a[i],
                    p1b == exp_strats$p1b[i],
                    p2a == exp_strats$p2a[i],
                    p2b == exp_strats$p2b[i])$pay2
  b_pay_2 <- filter(strats, 
                    n == "B",
                    p1a == exp_strats$p1a[i],
                    p1b == exp_strats$p1b[i],
                    p2a == exp_strats$p2a[i],
                    p2b == exp_strats$p2b[i])$pay2
  exp_strats$pay1[i] <- a_pay_1 * a_prob + b_pay_1 * (1-a_prob)
  exp_strats$pay2[i] <- a_pay_2 * a_prob + b_pay_2 * (1-a_prob)
}

best_response_p1 <- exp_strats |>
  group_by(p2a, p2b) |>
  summarise(best_1 = max(pay1), .groups = "drop")
  
best_response_p2 <- exp_strats |>
  group_by(p1a, p1b) |>
  summarise(best_2 = max(pay2), .groups = "drop")

exp_strats <- exp_strats |>
  left_join(best_response_p1, by = c("p2a", "p2b")) |>
  left_join(best_response_p2, by = c("p1a", "p1b")) |>
  mutate(eqm = case_when(
    pay1 == best_1 & pay2 == best_2 ~ 1,
    TRUE ~ 0
  ))

 exp_wide <- exp_strats |>
   mutate(P1 = paste(p1a, p1b, sep="")) |>
   mutate(P2 = paste(p2a, p2b, sep="")) |>
   mutate(payout = paste(pay1, pay2, sep=", ")) |>
   pivot_wider(id_cols = P1, names_from = P2, values_from = payout)

# if (sum(exp_strats$eqm) > 4){
#  print(s)
#  print(sum(exp_strats$eqm))
# }
