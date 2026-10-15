source("tech_trees.R")

techs <- create_techs()
dependencies <- create_tech_tree()

generations <- 100

known_techs <- techs$id[techs$tier == 1]
sampled_techs <- vector()

for (i in 1:generations) {
  known_payoffs <- techs$payoff[known_techs]
  sampled_tech <- sample(known_techs, 1, prob=known_payoffs)
  sampled_techs <- c(sampled_techs, sampled_tech)
}
