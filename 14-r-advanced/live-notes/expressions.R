form = y ~ x1 + I(x2) + x3

print_variables = function(form) {
  if (is.call(form)) {
    form_list <- as.list(form)
    if (length(form_list) == 2 & as.character(form_list[[1]]) == "I") {
      print(form_list[[2]])
    }
    for (subform in form_list[2 : length(form)]) {
      print_variables(subform)
    }
  }
}

print_variables(form)


my_call = call(
  "+",
  as.name("speed"),
  as.name("dist")
)


