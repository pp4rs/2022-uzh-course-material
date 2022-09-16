tree = list(
  1,
  list(2),
  list(
    3,
    list(5),
    list(6)
  ),
  list(4)
)

print_tree = function(tr) {
  print(tr[[1]])
  if (length(tr) > 1) {
    for (subtree in tr[2:length(tr)]) {
      print_tree(subtree)
    }
  }
}

print_tree(tree)

sum_tree = function(tr) {
  sigma = tr[[1]]
  if (length(tr) > 1) {
    for (subtree in tr[2:length(tr)]) {
      sigma = sigma + sum_tree(subtree)
    }
  }
  return(sigma)
}

sum_tree(tree)
sum_tree(tree[[3]])

reduce_tree = function(tr, f) {
  sigma = tr[[1]]
  if (length(tr) > 1) {
    for (subtree in tr[2:length(tr)]) {
      sigma = f(sigma, reduce_tree(subtree, f))
    }
  }
  return(sigma)
}

reduce_tree(tree, `*`)

map_tree_inplace = function(tr, f) {
  tr[[1]] = f(tr[[1]])
  if (length(tr) > 1) {
    for (subtree in tr[2:length(tr)]) {
      map_tree_inplace(subtree, f)
    }
  }
}

map_tree_inplace(tree, function (x) x^2)
# does not seem to work


map_tree = function(tr, f) {
  
  if (length(tr) == 1) {
    return(list(f(tr[[1]])))
  } else {
    val <- f(tr[[1]])
    new_tree <- list(val)
    for (subtree in tr[2:length(tr)]) {
      mapped_subtree <- map_tree(subtree, f)
      new_tree = append(new_tree, list(mapped_subtree))
    }
    return(new_tree)
  }
  
}

map_tree(tree, function(x) x^2)








