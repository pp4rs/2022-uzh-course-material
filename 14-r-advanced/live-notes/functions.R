adder = function(x, y) {
  return(x + y)
}

(function(x, y) {
  return(x + y)
})(2, 4)

library(purrr)

v = c(1, 2, 3, 4)

square_fun = function(x) {x^2}

squares <- map_dbl(v, function(x) {x^2})
sapply(v, function(x) {x^2})

a <- 5
`<-`(b, 6)

2 + 3
`+`(2, 3)

`%between%` <- function(x, limits) {
  limits = sort(limits)
  if (x >= limits[1] & x <= limits[2]) {
    ret <- TRUE
  } else {
    ret <- FALSE
  }
  return(ret)
}

# between(3, c(1, 4))
3 %between% c(1, 4)


`%pipe%` = function(x, f) {
  f(x)
}

3 %pipe% log

`%c%` = function(s1, s2) {
  stringr::str_c(s1, s2)
}

`%c%`("a", "b")



