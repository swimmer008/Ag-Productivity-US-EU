check.malmquist <- function(data, id.var, time.var, x.vars, y.vars, g.var) {
  if (!(is.data.frame(data)))
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1)
      stop("Too many id variables (id.var). Only one can be defined",
           call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical)
      stop("Unrecognizable variable in id.var: ", paste(id.var),
           call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1)
      stop("Too many time variables (time.var). Only one can be defined",
           call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical)
      stop("Unrecognizable variable in time.var:", paste(time.var),
           call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical)))
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical ==
                                                                 F], collapse = ","), call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical)))
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical ==
                                                                 F], collapse = ","), call. = FALSE)
  }
  if (!is.null(g.var)) {
    if (length(g.var) > 1) {
      stop("Too many group variables (g.var). Only one can be defined",
           call. = FALSE)
    } else {
      g.var <- varcheck(var = g.var, name = names.var)
    }
  } else {
    g.var <- NULL
  }
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1,
       y.vars = y.vars.1, g.var = g.var)
}
