# Lowe productivity index

lowe <- function(data, id.var, time.var, x.vars, y.vars, w.vars,
  p.vars, tech.change = TRUE, tech.reg = TRUE, rts = c("vrs",
    "crs", "nirs", "ndrs"), orientation = c("out", "in"),
  parallel = FALSE, cores = max(1, availableCores() - 1), scaled = FALSE,
  window = NULL, by.id = NULL, by.year = NULL, consecutive = FALSE,
  shadow = FALSE) {
  step1 <- check.lowe(data, id.var, time.var, x.vars, y.vars,
    w.vars, p.vars)
  if (!is.pbalanced(data, c(id.var, time.var))) {
    warning("data is unbalanced: changes computed for consecutive periods",
      call. = FALSE)
  }
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS)) {
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  }
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in")
  if (!(orientation %in% ORIENTATION)) {
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  }
  data <- arrange(data, get(step1$time.var), get(step1$id.var))
  year.vec <- unique(pull(data, time.var))
  if (scaled == FALSE) {
    if (any(data[, c(x.vars, y.vars)] >= 1e+05 | data[, c(x.vars,
      y.vars)] <= 1e-04)) {
      warning("Some quantity variables are not between 1e-4 and 1e5.
We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problems\n\r",
        call. = FALSE)
    }
    mean.y <- 1
    mean.x <- 1
  } else {
    mean.y <- data %>%
      summarize(across(all_of(y.vars), mean)) %>%
      as.numeric()
    mean.x <- data %>%
      summarize(across(all_of(x.vars), mean)) %>%
      as.numeric()
    data <- data %>%
      mutate(across(all_of(c(y.vars, x.vars)), function(x) x/mean(x)))
  }
  P.lo <- (data %>%
    summarize(across(all_of(p.vars), mean)) %>%
    as.numeric()) * mean.y
  W.lo <- (data %>%
    summarize(across(all_of(w.vars), mean)) %>%
    as.numeric()) * mean.x
  names(P.lo) <- paste0("U", 1:length(p.vars))
  names(W.lo) <- paste0("V", 1:length(w.vars))
  handlers(global = TRUE)
  handlers("progress")
  registerDoFuture()
  if (parallel == TRUE & cores == 1) {
    parallel <- FALSE
  }
  if (parallel == TRUE & cores > 1) {
    plan(multisession, workers = cores)
  } else {
    plan(sequential)
  }
  if (tech.change == TRUE) {
    if (is.null(window)) {
      res.lo.loop <- lo.1(data, step1, year.vec, tech.reg,
        rts, orientation, PRICESO = P.lo, PRICESI = W.lo,
        mean.x, mean.y, shadow)
    } else {
      window <- as.integer(window)
      if (window == 0 || window == 1 || window == length(year.vec)) {
        stop("window must be comprised between 1 and ",
          paste(length(year.vec)), call. = FALSE)
      }
      res.lo.loop <- lo.3(data, step1, year.vec, tech.reg,
        rts, orientation, PRICESO = P.lo, PRICESI = W.lo,
        mean.x, mean.y, shadow, window)
    }
  } else {
    res.lo.loop <- lo.2(data, step1, rts, orientation, PRICESO = P.lo,
      PRICESI = W.lo, mean.x, mean.y, shadow)
  }
  res.lo.loop <- cbind(data[, c(step1$id.var, step1$time.var)],
    res.lo.loop)
  row.names(res.lo.loop) <- seq(1:dim(res.lo.loop)[1])
  if (is.pbalanced(data, c(id.var, time.var))) {
    if (consecutive) {
      indices <- foreach(ano = 2:(length(year.vec)), .combine = rbind) %dopar%
        {
          int1 <- res.lo.loop[res.lo.loop[, 2] == year.vec[ano],
          1:2]
          int2 <- res.lo.loop[res.lo.loop[, 2] == year.vec[ano],
          3:20]/res.lo.loop[res.lo.loop[, 2] == year.vec[ano -
          1], 3:20]
          cbind(int1, int2)
        }
    } else {
      id.vec <- unique(res.lo.loop[, 1])
      if (!(is.null(by.id)) & !(is.null(by.year))) {
        if (by.id > length(id.vec)) {
          stop("by.id is out of range: by.id must be lower or equal to ",
          paste(length(id.vec)), .call = FALSE)
        }
        if (by.year > length(id.vec)) {
          stop("by.year is out of range: by.year must be lower or equal to ",
          paste(length(year.vec)), .call = FALSE)
        }
        indices <- res.lo.loop[, 3:20]/matrix(res.lo.loop[res.lo.loop[,
          2] == year.vec[by.year], 3:20][by.id, ], nrow = 1)
        indices <- cbind(res.lo.loop[, 1:2], indices)
      } else {
        if (!(is.null(by.id)) & (is.null(by.year))) {
          if (by.id > length(id.vec)) {
          stop("by.id is out of range: by.id must be lower or equal to ",
            paste(length(id.vec)), .call = FALSE)
          }
          indices <- foreach(id = id.vec, .combine = rbind) %dopar%
          {
            int1 <- res.lo.loop[res.lo.loop[, 1] ==
            id, 1:2]
            int2 <- res.lo.loop[res.lo.loop[, 1] ==
            id, 3:20]/res.lo.loop[res.lo.loop[, 1] ==
            id.vec[by.id], 3:20]
            cbind(int1, int2)
          }
        } else {
          if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) {
            stop("by.year is out of range: by.year must be lower or equal to ",
            paste(length(year.vec)), .call = FALSE)
          }
          indices <- foreach(ano = year.vec, .combine = rbind) %dopar%
            {
            res.lo.loop[res.lo.loop[, 2] == ano,
              3:20]/res.lo.loop[res.lo.loop[, 2] ==
              year.vec[by.year], 3:20]
            }
          indices <- cbind(res.lo.loop[, 1:2], indices)
          } else {
          indices <- foreach(ano = year.vec, .combine = rbind) %dopar%
            {
            res.lo.loop[res.lo.loop[, 2] == ano,
              3:20]/res.lo.loop[res.lo.loop[, 2] ==
              year.vec[1], 3:20]
            }
          indices <- cbind(res.lo.loop[, 1:2], indices)
          }
        }
      }
    }
  } else {
    indices <- foreach(ano = 2:(length(year.vec)), .combine = rbind) %dopar%
      {
        commonID <- (data %>%
          dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
          pull(step1$id.var)) %in% (data %>%
          dplyr::filter(get(step1$time.var) == year.vec[ano -
          1]) %>%
          pull(step1$id.var))
          IDnames <- (data %>%
                      dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
                      pull(step1$id.var))[commonID]
        int1 <- res.lo.loop[res.lo.loop[, 2] == year.vec[ano],
          1:2][commonID, ]
        int2 <- res.lo.loop[res.lo.loop[, 2] == year.vec[ano],
          3:20][commonID, ]/res.lo.loop[res.lo.loop[,
          2] == year.vec[ano - 1], 3:20][(data %>%
                                            dplyr::filter(get(step1$time.var) == year.vec[ano - 1]) %>%
                                            pull(step1$id.var)) %in% IDnames, ]
        cbind(int1, int2)
      }
  }
  plan(sequential)
  names(indices)[3:20] <- paste0("d", names(res.lo.loop[, 3:20]))
  if (shadow == TRUE) {
    Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)],
      res.lo.loop[, 21:(dim(res.lo.loop)[2])])
    names(Shadowp) <- c(names(data[id.var]), names(data[time.var]),
      names(data[x.vars]), names(data[y.vars]))
    res.tfp <- list(Levels = res.lo.loop[, 1:20], Changes = indices,
      Shadowp = Shadowp)
  } else {
    res.tfp <- list(Levels = res.lo.loop[, 1:20], Changes = indices)
  }
  class(res.tfp) <- c("list", "Lowe")
  return(res.tfp)
}
