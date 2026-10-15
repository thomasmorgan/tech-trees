source("tech_trees.R")

techs <- create_techs()
dependencies <- create_tech_tree()

known_techs <- techs$id[techs$tier == 1]
