source("tech_trees.R")

techs <- create_techs()
dependencies <- create_tech_tree()

generations <- 100  # how many goes
p_innovate <- 0.1  # probaiblity a tech that can be innovated, is innovated

known_techs <- techs$id[techs$tier == 1]  # vector of known techs by ID
sampled_techs <- vector()  # vector for which tech is exploited on each step

# innovation loop
for (i in 1:generations) {
  # pick a tech to exploit
  known_payoffs <- techs$payoff[known_techs]
  sampled_tech <- sample(known_techs, 1, prob=known_payoffs)
  
  # discover innovations
  possible_innovations <- vector()
  for (t in techs$id) {
    if (!(t %in% known_techs)) {  # cannot be already known
      if (dependencies[sampled_tech, t] %in% c(1, 2)) {  # must depend on exploited tech
        if (all(dependencies[t,setdiff(1:n_techs, known_techs)] > -1)) {  # all dependencies must be known
          if (runif(1, 0, 1) < p_innovate) {
            known_techs <- c(known_techs, t)
          }
        }
      }
    }
  }
  sampled_techs <- c(sampled_techs, sampled_tech)
}
