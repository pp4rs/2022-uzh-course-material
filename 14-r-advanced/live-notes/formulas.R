modify_formula <- function(form) {
    replace_list <- list()
    env <- environment(form)  # the formula's environment should not change
    modify_formula_iter <- function(form) {
        if (is.call(form)) {
            expr_list <- as.list(form)
            if (as.character(expr_list[[1]]) == "m") {
                varname <- as.character(expr_list[[2]])
                lag <- as.integer(as.character(expr_list[[3]]))
                new_varname <- paste("", varname, "l", lag, sep = "_")
                replace_list <<- c(  # assign in non-local scope
                    replace_list,
                    list(list(varname, lag, new_varname))
                )
                return(as.name(new_varname))
            } else {
                subform_list <- lapply(
                    expr_list[2 : length(expr_list)],
                    modify_formula_iter
                )
                return(as.call(c(list(expr_list[[1]]), subform_list)))
            }
        } else {
            return(form)
        }
    }
    new_form <- as.formula(modify_formula_iter(form))
    environment(new_form) <- env
    return(list(new_form, replace_list))
}

f <- y ~ x1 + x2 + x3 + m(z1, 3) + m(z2, 1)
res <- modify_formula(f)
res[[1]]  # the modified formula
res[[2]]  # the data to handle variable creation/joins

# note:
#  - no error handling
#  - does not handle negative lags