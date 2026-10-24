
# function to generate number of techs in each tier
num_techs_per_tier <- function(tier) {
  if (tree_growth_rate < 0.01) { return(starting_techs) }
  return(round((tier + (starting_techs)^(1/tree_growth_rate) - 1)^tree_growth_rate))
}

create_techs <- function() {
  # initialize data frame
  techs <- data.frame(
    id   = integer(), # unique ID for each tech
    tier = integer(), # what tier its in
    keystone = integer(), # is it a keystone tech
    xpos = numeric(), # xcor for plotting
    ypos = numeric()  # ycor for plotting
  )
  
  # create techs
  for (t in 1:max_tier) {
    n_techs_tier <- num_techs_per_tier(t)
    ids <- c((nrow(techs)+1):(nrow(techs)+n_techs_tier))
    new_rows <- data.frame(
      id = ids,
      tier = rep(t, n_techs_tier),
      keystone = round(runif(n_techs_tier, 0, 1) < p_keystone_tech)*1,
      xpos = ids - mean(ids),
      ypos = rep(t, n_techs_tier)
    )
    techs <- rbind(techs, new_rows)
  }
  
  # total number of techs in tree
  n_techs <<- nrow(techs)
  
  # add some jitter
  techs$xpos <- techs$xpos + rnorm(n_techs, 0, 0.1)
  techs$ypos <- techs$ypos + rnorm(n_techs, 0, 0.1)

  return(techs)
}

create_tech_tree <- function() {
  techs <- create_techs()
  dependencies <- create_dependency_matrix(techs)
  if (check_dependencies(dependencies)) {
    print("Tree is good.")
  } else {
    print("Warning, tree contains loops!")
  }
  
  # horizontally reorder techs to make tree nicer looking
  for (i in 2:max_tier) {  # go through all tiers, starting with tier 3
    this_tier_ids <- techs[techs$tier == i,]$id  # first get the ids of the techs in this tier
    parents <- parent_xpos <- vector()
    for (t in this_tier_ids) {
      parent <- techs[techs$id == match(1, dependencies[,t])[1],]
      parents <- c(parents, parent$id)  # then get the techs from the prior tier these techs depend on
      parent_xpos <- c(parent_xpos, parent$xpos)  # and get their xpos' too
    }
    ord <- order(parent_xpos, this_tier_ids)  # get order of parents' xposs, breaking by offspring id
    ordered_ids <- this_tier_ids[ord]   # order the ids according to this
    techs$xpos[match(ordered_ids, techs$id)] <- seq_along(ordered_ids) - mean(seq_along(ordered_ids))  # insert into db
  }
  
  # calculate total number of dependencies
  techs$direct_outgoing_dependencies <- rowSums((dependencies > 0) * (dependencies < 3))
  techs$direct_incoming_dependencies <- rowSums((dependencies < 0) * (dependencies > -3))
  techs$all_outgoing_dependencies <- rowSums(dependencies > 0)
  techs$all_incoming_dependencies <- rowSums(dependencies < 0)
  
  # calculate how hard each tech is to discover
  tier_1_techs <- techs$id[techs$tier == 1]
  techs$innovations_required <- rowSums(
    dependencies[, -(tier_1_techs), drop = FALSE] < 0
  ) + 1L
  # Foundational techs are already known.
  techs$innovations_required[tier_1_techs] <- 0L
  
  # assign payoffs
  techs$payoff <- rexp(nrow(techs), 1/(payoff_growth_rate^techs$innovations_required))
  techs$payoff <- techs$payoff/mean(techs$payoff)
  
  tree <- list(
    techs=techs,
    dependencies=dependencies
  )
  return(tree)
}
  
create_dependency_matrix <- function(techs) {
  # initialize dependencies:
  # "dependencies" is a n-by-n matrix that scores the relationship between every pair of techs
  dependencies <- matrix(0, nrow=n_techs, ncol=n_techs)
  
  # dependency code:
  # 0 = none
  # 1 = basic from-to
  # -1 = basic to-from
  # 2 = extra to-from
  # -2 = extra from-to
  # 3 = indirect to-from
  # -3 = indirect from-to
  
  # add basic dependencies
  # each tech has one basic dependecy from a random chosen tech in the prior tier
  for (i in 1:n_techs) {
    tech <- techs[i,]
    if (tech$tier > 1) {
      possible_parents <- techs$id[techs$tier == (tech$tier - 1)]
      parent <- possible_parents[sample.int(length(possible_parents), 1)]
      dependencies[parent, tech$id] <- 1
      dependencies[tech$id, parent] <- -1
    }
  }
  
  # identify indirect dependencies
  # e.g. A -> B and B -> C, implies A -> C
  for (to in 1:n_techs) {
    if (techs$tier[techs$id == to] > 1) {
      for (from in 1:(i-1)) {
        if (dependencies[from, to] > 0) {
          earlier_techs <- c(1:n_techs)[dependencies[,from] > 0]
          for (et in earlier_techs) {
            if (dependencies[et, to] == 0) {
              dependencies[et,to] <- 3
            }
            if (dependencies[to, et] == 0) {
              dependencies[to,et] <- -3
            }
          }
        }
      }
    }
  }
  
  # add some extra dependencies
  # these can occur between any possible techs
  # as long as they don't already have a dependency of any kind
  # and as long as they are not too many tiers apart
  pairs <- expand.grid(
    from = 1:n_techs,
    to   = 1:n_techs
  )
  pairs$parent_tier <- techs$tier[match(pairs$from, techs$id)]
  pairs$child_tier  <- techs$tier[match(pairs$to, techs$id)]
  pairs$tier_delta <- pairs$child_tier - pairs$parent_tier
  pairs <- pairs[pairs$tier_delta %in% extra_dependency_tier_gaps,]
  pairs <- pairs[sample(nrow(pairs)), ]
  
  # NEW Method
  if (new_dependency_method) {
    for (parent in 1:nrow(techs)) {
      possible_children <- pairs$to[
        pairs$from == parent & # only your kids
        pairs$to != parent & # can't be yourself
        dependencies[parent,pairs$to] == 0 & # no existing connection
        pairs$parent_tier != 1 & # cannot be from tier 1
        pairs$child_tier != 1 # cannot be to tier 1
      ]
      n_children <- rpois(1, p_extra_dependency)
      n_children <- min(n_children, length(possible_children))
      children <- sample(possible_children, n_children, replace=F)
      
      for (child in children) {
        dependencies[parent, child] <- 2
        dependencies[child, parent] <- -2
    
        ancestors <- which(dependencies[parent, ] < 0)
        descendants <- which(dependencies[child, ] > 0)
    
        for (ancestor in ancestors) {
          for (descendant in descendants) {
            if (dependencies[ancestor, descendant] == 0) {
              dependencies[ancestor, descendant] <- 3
              dependencies[descendant, ancestor] <- -3
            }
          }
        }
    
        for (descendant in descendants) {
          if (dependencies[parent, descendant] %in% c(0,2)) {
            dependencies[parent, descendant] <- 3
            dependencies[descendant, parent] <- -3
          }
        }
    
        for (ancestor in ancestors) {
          if (dependencies[ancestor, child] %in% c(0, 2)) {
            dependencies[ancestor, child] <- 3
            dependencies[child, ancestor] <- -3
          }
        }
      }
    }
    
  } else {

    # OLD DEPENDENCY METHOD
  
    for (i in 1:nrow(pairs)) {
      pair <- pairs[i,]
      parent <- pair$from
      child <- pair$to
      tier_delta <- pair$tier_delta
  
  
      if ((parent != child) &
          (dependencies[parent, child] == 0) &
          (pair$parent_tier > 1) &
          (pair$child_tier > 1)) {
        p_link <- p_extra_dependency
        if (techs$keystone[techs$id == parent] == 1) { p_link <- p_link * keystone_multiplier }
        if (runif(1,0,1) < p_link) {
          dependencies[parent, child] <- 2
          dependencies[child, parent] <- -2
  
          ancestors <- which(dependencies[parent, ] < 0)
          descendants <- which(dependencies[child, ] > 0)
  
          for (ancestor in ancestors) {
            for (descendant in descendants) {
              if (dependencies[ancestor, descendant] == 0) {
                dependencies[ancestor, descendant] <- 3
                dependencies[descendant, ancestor] <- -3
              }
            }
          }
  
          for (descendant in descendants) {
            if (dependencies[parent, descendant] %in% c(0,2)) {
              dependencies[parent, descendant] <- 3
              dependencies[descendant, parent] <- -3
            }
          }
  
          for (ancestor in ancestors) {
            if (dependencies[ancestor, child] %in% c(0, 2)) {
              dependencies[ancestor, child] <- 3
              dependencies[child, ancestor] <- -3
            }
          }
        }
      }
    }
  }

  return(dependencies)
}

check_dependencies <- function(dependencies, foundational = 1:5) {
  n <- nrow(dependencies)
  
  # Check the matrix itself.
  if (!is.matrix(dependencies) ||
      n != ncol(dependencies) ||
      anyNA(dependencies) ||
      !all(dependencies %in% -3:3)) {
    stop("dependencies must be a square matrix containing codes -3 to 3.")
  }
  
  if (!all(dependencies == -t(dependencies))) {
    stop("dependencies must equal -t(dependencies).")
  }
  
  if (anyNA(foundational) ||
      !all(foundational %in% seq_len(n))) {
    stop("foundational must contain valid technology indices.")
  }
  
  # TRUE means the row technology is a prerequisite of the column.
  edges <- matrix(
    dependencies %in% c(1, 2, 3),
    nrow = n,
    ncol = n
  )
  
  # Every non-foundational technology needs a prerequisite.
  n_prerequisites <- colSums(edges)
  missing_prerequisites <- setdiff(
    which(n_prerequisites == 0),
    foundational
  )
  
  # Remove technologies whose prerequisites have already been removed.
  remaining <- rep(TRUE, n)
  ready <- which(n_prerequisites == 0)
  
  while (length(ready) > 0) {
    remaining[ready] <- FALSE
    
    n_prerequisites <- n_prerequisites -
      colSums(edges[ready, , drop = FALSE])
    
    ready <- which(remaining & n_prerequisites == 0)
  }
  
  has_loop <- any(remaining)
  
  list(
    valid = !has_loop && length(missing_prerequisites) == 0,
    has_loop = has_loop,
    missing_prerequisites = missing_prerequisites,
    blocked_by_loops = which(remaining)
  )
  
  return(!has_loop && length(missing_prerequisites) == 0)
}

plot_tech_tree <- function(tree, techs_to_paint=c(1:nrow(tree$techs)),by_tier=TRUE) {
  techs <- tree$techs
  dependencies <- tree$dependencies
  
  payoff_ratio <- max(techs$payoff) / min(techs$payoff)
  plotting_scale_parameter <- log(10)/log(payoff_ratio)
  
  dot_sizes <- techs$payoff^plotting_scale_parameter
  dot_sizes <- dot_sizes/max(dot_sizes)*5
  
  dot_cols <- round(1 + 255*(dot_sizes/max(dot_sizes)))
  pal <- hcl.colors(256, "viridis")
  
  if (by_tier) {
    ys <- techs$ypos
  } else {
    ys <- techs$innovations_required
  }
  
  plot(ys~techs$xpos, bty="n", xlab="", xaxt="n", ylab="tier", pch=16, cex=dot_sizes[techs_to_paint], col="white")
  points(ys[techs_to_paint]~techs$xpos[techs_to_paint], bty="n", xlab="", xaxt="n", ylab="tier", pch=16, cex=dot_sizes[techs_to_paint], col = pal[dot_cols[techs_to_paint]])
  
  if (max_tier < 8) {
    for (i in 1:nrow(techs)) {
      text(techs$xpos[i], ys[i], techs$id[i], pos=1, offset=dot_sizes[i]*0.2 + 1/max_tier)
    }
  }
  for (i in techs_to_paint) {
    for (j in techs_to_paint) {
      if (dependencies[i,j] == 1) {
        from <- techs[techs$id == i,]
        to <- techs[techs$id == j,]
        lines(c(ys[i], ys[j])~c(from$xpos, to$xpos), lwd=(to$all_dependencies/n_techs * 10 + 0.5))
      }
      if (dependencies[i,j] == 2) {
        from <- techs[techs$id == i,]
        to <- techs[techs$id == j,]
        arrows(from$xpos, ys[i],
               to$xpos, ys[j],
               length = 0.1,
               col="firebrick",
               angle = 25,
               code=2,
               lwd=(to$all_dependencies/n_techs * 5 + 0.5))
      }
    }
  }
}

generate_trees <- function(n_trees = 1000) {
  trees <- vector("list", n_trees)
  for (i in seq_len(n_trees)) {
    techs <- create_techs()
    dependencies <- create_tech_tree()
    trees[[i]] <- list(techs = techs, dependencies = dependencies)
    cat(paste0("\rGenerated ", i, "/", n_trees, " trees."))
    flush.console()
  }
  cat("\n")
  
  saveRDS(trees, file = paste0(n_trees, "_trees.rds"))
}

