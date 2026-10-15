source("tech_trees.R")

techs <- create_techs()
dependencies <- create_tech_tree()

generations <- 100
p_innovate <- 0.1

known_techs <- techs$id[techs$tier == 1]
sampled_techs <- vector()

for (i in 1:generations) {
  known_payoffs <- techs$payoff[known_techs]
  sampled_tech <- sample(known_techs, 1, prob=known_payoffs)
  
  possible_innovations <- vector()
  for (t in techs$id) {
    if (!(t %in% known_techs)) {
      if (dependencies[sampled_tech, t] == 1) {
        if (all(dependencies[t,setdiff(1:n_techs, known_techs)] > -1)) {
          if (runif(1, 0, 1) < p_innovate) {
            known_techs <- c(known_techs, t)
          }
        }
      }
    }
  }
  sampled_techs <- c(sampled_techs, sampled_tech)
}
