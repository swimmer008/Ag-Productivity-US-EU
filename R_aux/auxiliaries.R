# Productivity package auxiliary functions ----------

## Varcheck function
varcheck <- function(var, name) {
  if (is.numeric(var)) {
    vars.1 <- name[var]
  } else {
    vars.1 <- var
  }
  var_logical <- vars.1 %in% name
  if (!(all(var_logical))) {
    stop("Unrecognizable variables in ", deparse(substitute(var)),
         " : ", paste(var[var_logical == FALSE], collapse = ", "),
         call. = FALSE)
  }
  vars.1
}

## Shephard Output distance function ----------

DO.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(0, XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  set.rhs(built.lp, c(XOBS, rep(0, n_y)))
  lp.control(built.lp, sense = "max")
  if (rts == "crs") {
    status <- solve(built.lp)
    optval <- get.objective(built.lp)
    optval <- if (optval == 1e+30) Inf else optval
    DO <- 1/optval
  } else {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp,
                   rhs = 1)
    status <- solve(built.lp)
    optval <- get.objective(built.lp)
    optval <- if (optval == 1e+30) Inf else optval
    DO <- 1/optval
  }
  # delete.lp(built.lp)
  DO
}


## Overall output efficiency ----------
## I change the scaling option here because of issues with some observations
## like LA in 2004
DO.ooe <- function(XOBS, YOBS, XREF, YREF, PRICESO, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, n_y + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j,
    ], YREF[j, ]))
  }
  set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0,
                                                         n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs(built.lp, c(XOBS, rep(0, n_y)))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(rep(0, n_y), rep(1, n_t)),
                   type = ctyp, rhs = 1)
  }
  lp.control(built.lp, sense = "max", scaling = c("curtisreid","equilibrate","integers"))
  solve(built.lp)
  OOE <- 1/get.objective(built.lp)
  OOE
}

## Shephard Input distance function ----------

DI.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, 1 + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(0, YREF[j, ]))
  }
  set.objfn(built.lp, c(1, rep(0, n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs(built.lp, c(rep(0, n_x), YOBS))
  set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  lp.control(built.lp, sense = "min")
  if (rts == "crs") {
    status <- solve(built.lp)
    optval <- get.objective(built.lp)
    optval <- if (optval == -1e+30) -Inf else optval
    DI <- 1/optval
  } else {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(0, rep(1, n_t)), type = ctyp,
                   rhs = 1)
    status <- solve(built.lp)
    optval <- get.objective(built.lp)
    optval <- if (optval == -1e+30) -Inf else optval
    DI <- 1/optval
  }
  # delete.lp(built.lp)
  DI
}


## Overall input efficiency ----------

DI.oie <- function(XOBS, YOBS, XREF, PRICESI, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y, n_x + n_t)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i,
      ], XREF[i, ]))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
  }
  set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0,
    n_t)))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
  set.rhs(built.lp, c(rep(0, n_x), YOBS))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(rep(0, n_x), rep(1, n_t)),
      type = ctyp, rhs = 1)
  }
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  OIE <- get.objective(built.lp)
  OIE
}

## Point of maximum productivity ----------

D.tfp <- function(XOBS, YOBS, XREF, YREF, PRICESO, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_x + n_y + 1, n_y + n_x + n_t + 1)
  for (i in 1:n_x) {
    set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x,
      ncol = n_x)[i, ], XREF[i, ], 0))
  }
  for (j in 1:n_y) {
    set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j,
      ], rep(0, n_x), YREF[j, ], 0))
  }
  set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI,
    rep(0, n_t + 1)))
  set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t),
    0))
  set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y),
    "="))
  set.rhs(built.lp, c(rep(0, n_x), rep(0, n_y), 1))
  if (rts != "crs") {
    ctyp <- if (rts == "vrs") {
      "="
    } else {
      if (rts == "nirs") {
        "<="
      } else {
        if (rts == "ndrs") {
          ">="
        }
      }
    }
    add.constraint(built.lp, c(rep(0, n_x + n_y), rep(1,
      n_t), -1), type = ctyp, rhs = 0)
  }
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  return(get.objective(built.lp))
}

## dual shephard output distance function ----------

DO.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_t + 1, n_y + n_x)
  for (i in 1:n_y) {
    set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
  }
  for (j in 1:n_x) {
    set.column(built.lp, n_y + j, c(XREF[j, ], 0))
  }
  if (rts %in% c("vrs", "nirs")) {
    add.column(built.lp, c(rep(1, n_t), 0))
  } else {
    if (rts == "ndrs") {
      add.column(built.lp, c(rep(-1, n_t), 0))
    }
  }
  obj <- if (rts == "crs") {
    c(rep(0, n_y), XOBS)
  } else {
    if (rts %in% c("vrs", "nirs")) {
      c(rep(0, n_y), XOBS, 1)
    } else {
      if (rts == "ndrs") {
        c(rep(0, n_y), XOBS, -1)
      }
    }
  }
  set.objfn(built.lp, obj)
  set.constr.type(built.lp, c(rep(">=", n_t), "="))
  set.rhs(built.lp, c(rep(0, n_t), 1))
  if (rts == "vrs")
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x +
      n_y + 1)
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 +
    n_y):(n_y + n_x)] * XOBS) + if (rts == "crs") {
    0
  } else {
    if (rts %in% c("vrs", "nirs")) {
      get.variables(built.lp)[n_y + n_x + 1]
    } else {
      if (rts == "ndrs") {
        -get.variables(built.lp)[n_y + n_x + 1]
      }
    }
  })
  names(prices_o) <- paste("U", 1:n_y, sep = "")
  prices_o
}

## dual shephard input distance function ----------

DI.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  built.lp <- make.lp(n_t + 1, n_y + n_x)
  for (i in 1:n_y) {
    set.column(built.lp, i, c(YREF[i, ], 0))
  }
  for (j in 1:n_x) {
    set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
  }
  if (rts %in% c("vrs", "ndrs")) {
    add.column(built.lp, c(rep(1, n_t), 0))
  } else {
    if (rts == "nirs") {
      add.column(built.lp, c(rep(-1, n_t), 0))
    }
  }
  obj <- if (rts == "crs") {
    c(YOBS, rep(0, n_x))
  } else {
    if (rts %in% c("vrs", "ndrs")) {
      c(YOBS, rep(0, n_x), 1)
    } else {
      if (rts == "nirs") {
        c(YOBS, rep(0, n_x), -1)
      }
    }
  }
  set.objfn(built.lp, obj)
  set.constr.type(built.lp, c(rep("<=", n_t), "="))
  set.rhs(built.lp, c(rep(0, n_t), 1))
  if (rts == "vrs")
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x +
      n_y + 1)
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/(sum(get.variables(built.lp)[1:n_y] *
    YOBS) + if (rts == "crs") {
    0
  } else {
    if (rts %in% c("vrs", "ndrs")) {
      get.variables(built.lp)[n_x + n_y + 1]
    } else {
      if (rts == "nirs") {
        -get.variables(built.lp)[n_x + n_y + 1]
      }
    }
  })
  names(prices_i) <- paste("V", 1:n_x, sep = "")
  prices_i
}

## window matrix construction ----------

windowFun <- function(year.vec, window) {
  do.call(rbind, lapply(1:(length(year.vec) - window + 1),
    FUN = function(i) seq(year.vec[i], year.vec[i] + window -
      1)))
}

# ## apply a function to divide 2 columns ----------
# fdiv <- function(x) x[, 1] / x[, 2]

## Test for balanced panel data (this function does not work well with tibbles)

# balanced <- function(data, id.var, time.var) {
#   x <- data[, id.var]
#   y <- data[, time.var]
#   if (length(x) != length(y)) stop(paste0("The length of the two vectors (i.e. ", id.var, " and ", time.var, ") differs\n"))
#   x <- data[, id.var][drop = TRUE]
#   y <- data[, time.var][drop = TRUE]
#   z <- table(x, y)
#   if (any(as.vector(z) == 0)) {
#     balanced <- FALSE
#   } else {
#     balanced <- TRUE
#   }
#   return(balanced)
# }

## Return functions (i.e. Levels(); Changes(); Shadowp())

# Levels <- function(object, ...) {
#   if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
#     stop("Function 'Levels' can not be applied to an object of class \"", class(object), "\"")
#   }
#   if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche")) | (is(object, "HicksMoorsteen") & (length(object) == 2))) {
#     return(object$Levels)
#   }
#   if (is(object, "HicksMoorsteen") & (length(object) > 2)) {
#     return(lapply(object, function(x) x$Levels))
#   }
# }
# 
# Changes <- function(object, ...) {
#   if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
#     stop("Function 'Changes' can not be applied to an object of class \"", class(object), "\"")
#   }
#   if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche")) | (is(object, "HicksMoorsteen") & (length(object) == 2))) {
#     return(object$Changes)
#   }
#   if (is(object, "HicksMoorsteen") & (length(object) > 2)) {
#     return(lapply(object, function(x) x$Changes))
#   }
# }
# 
# Shadowp <- function(object, ...) {
#   if (is(object, c("Malmquist"))) {
#     stop("Function 'Shadowp' can not be applied to an object of class \"", class(object)[2], "\"")
#   }
#   if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche", "HicksMoorsteen"))) {
#     stop("Function 'Shadowp' can not be applied to an object of class \"", class(object), "\"")
#   }
#   if (is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche")) & is.null(object$Shadowp)) {
#     stop("No shadow prices are returned in your \"", class(object)[2], "\"", " object.
#        Specifying 'shadow = TRUE' should be considered in the function generating the \"", class(object)[2], "\"", " object.")
#   }
#   if (is(object, "HicksMoorsteen")) {
#     if (length(object) == 2) {
#       stop("No shadow prices are returned in your \"", class(object)[2], "\"", " object.
#        Specifying 'components = TRUE' should be considered in the function generating the \"", class(object)[2], "\"", " object.")
#     } else {
#       List <- lapply(object, function(x) x$Shadowp)
#       return(List[!sapply(List, is.null)])
#     }
#   }
#   return(object$Shadowp)
# }
