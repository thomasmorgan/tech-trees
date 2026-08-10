# parameters for tree
max_tier <- 10
num_techs_per_tier <- function(tier) { return(round((tier/1.5)^1.5)) }
n_techs <- sum(num_techs_per_tier(c(1:max_tier)))
p_extra_dependency <- 0.02/n_techs
p_keystone_tech <- 0.02
keystone_techs <- sample(c(1:n_techs), round(p_keystone_tech * n_techs))
keystone_multiplier <- 200

# initialize data frame
techs <- data.frame(
  id = 1,
  tier = 1,
  xpos = 0,
  ypos = 1
)

# create techs
for (t in 1:max_tier) {
  n_techs_tier <- num_techs_per_tier(t)
  
  ids <- c((nrow(techs)+1):(nrow(techs)+n_techs_tier))
  
  new_rows <- data.frame(
    id = ids,
    tier = rep(t, n_techs_tier),
    xpos = ids - mean(ids),
    ypos = rep(t, n_techs_tier)
  )

  techs <- rbind(techs, new_rows)
}

# initialize dependencies:
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

# horizontally reorder techs to make tree nicer looking
for (i in 3:max_tier) {  # go through all tiers, starting with tier 3
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

old_ds <- dependencies

# add some arbitrary dependencies
pairs <- expand.grid(
  from = 1:n_techs,
  to   = 1:n_techs
)
pairs <- pairs[sample(nrow(pairs)), ]

for (i in 1:nrow(pairs)) {
  pair <- pairs[i,]
  parent <- pair$from
  child <- pair$to
  
  # print(paste0("*** Considering: ", parent, " -> ", child))
  # if (parent == child) {
  #   print(paste0("Cannot add it as they are the same"))
  # }
  # if (dependencies[parent, child] != 0) {
  #   print(paste0("Cannot add it as they have a relationship: ", dependencies[parent, child]))
  # }
  
  
  if ((parent != child) & (dependencies[parent, child] == 0)) {
    p_link <- p_extra_dependency
    if (parent %in% keystone_techs) { p_link <- p_link * keystone_multiplier }
    if (runif(1,0,1) < p_link) {
      # print(paste0("Adding dependency from ", parent, " to ", child, ". ", dependencies[parent, child]))
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

# calculate total number of dependencies
techs$direct_dependencies <- rowSums((dependencies > 0) * (dependencies < 3))
techs$all_dependencies <- rowSums(dependencies > 0)


# add some jitter
techs$xpos <- techs$xpos + rnorm(n_techs, 0, 0.1)
techs$ypos <- techs$ypos + rnorm(n_techs, 0, 0.1)

# plot tree
hist(techs$direct_dependencies)
hist(techs$all_dependencies/n_techs)

#dot_sizes <- (((techs$all_dependencies/n_techs)^0.5)) * 50 / max_tier
dot_sizes <- (techs$all_dependencies/n_techs)^0.5*50 / max_tier
plot(techs$ypos~techs$xpos, bty="n", xlab="", xaxt="n", ylab="tier", pch=16, cex=dot_sizes)
points(techs$ypos[keystone_techs]~techs$xpos[keystone_techs], pch=16, cex=dot_sizes[keystone_techs], col="purple")
if (max_tier < 8) {
  for (i in 1:nrow(techs)) {
    text(techs$xpos[i], techs$ypos[i], techs$id[i], pos=1, offset=dot_sizes[i]*0.2 + 1/max_tier)
  }
}
for (i in 1:n_techs) {
  for (j in 1:n_techs) {
    if (dependencies[i,j] == 1) {
      from <- techs[techs$id == i,]
      to <- techs[techs$id == j,]
      lines(c(from$ypos, to$ypos)~c(from$xpos, to$xpos), lwd=(to$all_dependencies/n_techs * 5 + 0.5))
    }
    if (dependencies[i,j] == 2) {
      from <- techs[techs$id == i,]
      to <- techs[techs$id == j,]
      arrows(from$xpos, from$ypos,
             to$xpos, to$ypos,
             length = 0.1,
             col="firebrick",
             angle = 25,
             code=2,
             lwd=(to$all_dependencies/n_techs * 5 + 0.5))
    }
  }
}



