
# Global Malmquist productivity index ----------
globalmalm <- function(data, id.var, time.var, x.vars, y.vars,
  g.var = NULL, orientation = c("out", "in"), parallel = FALSE,
  cores = parallelly::availableCores(omit = 1), scaled = FALSE,
  window = NULL, by.id = NULL, by.year = NULL, consecutive = FALSE) {
  step1 <- check.malmquist(data, id.var, time.var, x.vars,
    y.vars, g.var)
  data <- as.data.frame(data) ## in case it is tibble: mess up year.vec
  if (!plm::is.pbalanced(data, c(id.var, time.var)))
    stop("Malmquist index can only be computed from balanced data. Please consider balancing the data.",
      call. = FALSE)
  if (is.null(g.var)) {
    data <- data[order(data[, step1$time.var], data[, step1$id.var]),
      ]
  } else {
    data <- data[order(data[, step1$g.var], data[, step1$time.var],
      data[, step1$id.var]), ]
    metaFactors <- unique(data[, step1$g.var])
  }
  # if (is.null(g.var)) { data <- data[order(data[,
  # step1$time.var], data[, step1$id.var]), ] } else { data
  # <- data[order(data[, step1$g.var], data[,
  # step1$time.var], data[, step1$id.var]), ] metaFactors
  # <- unique(data[, step1$g.var]) }
  year.vec <-  unique(data[, time.var])
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in")
  if (!(orientation %in% ORIENTATION))
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 |
      data[, c(step1$x.vars, step1$y.vars)] <= 1e-04))
      warning("Some quantity variables are not between 1e-4 and 1e5.
We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problems\n\r",
        call. = FALSE)
  } else {
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[,
      c(step1$x.vars, step1$y.vars)], 2, FUN = function(x) x/mean(x))
  }
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
  if (is.null(window)) {
    if (is.null(g.var)) {
      res_globalmalm_loop <- globalmalm.1(data, step1,
        year.vec, orientation)
    } else {
      res_globalmalm_loop <- globalmalm.1M(data, step1,
        year.vec, orientation, metaFactors)
    }
  } else {
    window <- as.integer(window)
    if (is.null(g.var)) {
      if (window == 0 || window == 1 || window == length(year.vec)) {
        stop("window must be strictly comprised between 1 and ",
          paste(length(year.vec)), call. = FALSE)
      }
      res_globalmalm_loop <- globalmalm.1W(data, step1,
        year.vec, orientation, window)
    } else {
      if (length(window) > 1 && length(window) < length(metaFactors)) {
        stop("window must be a single integer or of the same length as \n
               the number of groups in g.var: ",
          paste0(length(metaFactors)), call. = FALSE)
      } else {
        if (length(window) == 1) {
          window <- rep(window, length(metaFactors))
        }
      }
      res_globalmalm_loop <- globalmalm.1WM(data, step1,
        year.vec, orientation, window, metaFactors)
    }
  }
  res_globalmalm_loop <- cbind(data[, c(step1$id.var, step1$time.var)],
    res_globalmalm_loop)
  row.names(res_globalmalm_loop) <- seq(1:dim(res_globalmalm_loop)[1])
  res_globalmalm_loop <- res_globalmalm_loop[order(res_globalmalm_loop[,
    step1$time.var], res_globalmalm_loop[, step1$id.var]),
    ]
  if (consecutive) {
    if (orientation == "out") {
      data.indices <- foreach(ano = 2:(length(year.vec)),
        .combine = rbind) %dopar% {
        cbind(res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], 1], res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano - 1], 2], res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], 2], if (!is.null(g.var))
          res_globalmalm_loop[res_globalmalm_loop[, 2] ==
          year.vec[ano], 3] else NULL, res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], if (!is.null(g.var))
          -c(1:3, 6) else -c(1:2)]/res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano - 1], if (!is.null(g.var))
          -c(1:3, 6) else -c(1:2)])
      }
    } else {
      data.indices <- foreach(ano = 2:(length(year.vec)),
        .combine = rbind) %dopar% {
        cbind(res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], 1], res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano - 1], 2], res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], 2], if (!is.null(g.var))
          res_globalmalm_loop[res_globalmalm_loop[, 2] ==
          year.vec[ano], 3] else NULL, res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano - 1], if (!is.null(g.var))
          -c(1:3, 6) else -c(1:2)]/res_globalmalm_loop[res_globalmalm_loop[,
          2] == year.vec[ano], if (!is.null(g.var))
          -c(1:3, 6) else -c(1:2)])
      }
    }
    names(data.indices) <- c(step1$id.var, "Year.0", "Year.1",
      if (!is.null(g.var)) step1$g.var else NULL, "global.malmquist",
      "effch", "best.practice.ch", if (!is.null(g.var)) "tech.gap.ratioch" else NULL)
  } else {
    id.vec <- unique(res_globalmalm_loop[, 1])
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      if (by.id > length(id.vec)) {
        stop("by.id is out of range: by.id must be lower or equal to ",
          paste(length(id.vec)), .call = FALSE)
      }
      if (by.year > length(id.vec)) {
        stop("by.year is out of range: by.year must be lower or equal to ",
          paste(length(year.vec)), .call = FALSE)
      }
      data.indices <- sweep(res_globalmalm_loop[, if (!is.null(g.var))
        -c(1:3, 6) else -c(1:2)], MARGIN = 2, STATS = as.numeric(res_globalmalm_loop[res_globalmalm_loop[,
        time.var] == year.vec[by.year], if (!is.null(g.var)) -c(1:3,
        6) else -c(1:2)][by.id, ]), FUN = "/")
      data.indices <- cbind(res_globalmalm_loop[, if (!is.null(g.var))
        1:3 else 1:2], data.indices)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec)) {
          stop("by.id is out of range: by.id must be lower or equal to ",
          paste(length(id.vec)), .call = FALSE)
        }
        data.indices <- foreach(id = id.vec, .combine = rbind) %dopar%
          {
          int1 <- res_globalmalm_loop[res_globalmalm_loop[,
            1] == id, if (!is.null(g.var))
            1:3 else 1:2]
          int2 <- res_globalmalm_loop[res_globalmalm_loop[,
            1] == id, if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]/res_globalmalm_loop[res_globalmalm_loop[,
            1] == id.vec[by.id], if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]
          cbind(int1, int2)
          }
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) {
          stop("by.year is out of range: by.year must be lower or equal to ",
            paste(length(year.vec)), .call = FALSE)
          }
          data.indices <- foreach(ano = year.vec, .combine = rbind) %dopar%
          {
            res_globalmalm_loop[res_globalmalm_loop[,
            2] == ano, if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]/res_globalmalm_loop[res_globalmalm_loop[,
            2] == year.vec[by.year], if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]
          }
          data.indices <- cbind(res_globalmalm_loop[,
          if (!is.null(g.var))
            1:3 else 1:2], data.indices)
        } else {
          data.indices <- foreach(ano = year.vec, .combine = rbind) %dopar%
          {
            res_globalmalm_loop[res_globalmalm_loop[,
            2] == ano, if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]/res_globalmalm_loop[res_globalmalm_loop[,
            2] == year.vec[1], if (!is.null(g.var))
            -c(1:3, 6) else -c(1:2)]
          }
          data.indices <- cbind(res_globalmalm_loop[,
          if (!is.null(g.var))
            1:3 else 1:2], data.indices)
        }
      }
    }
    names(data.indices) <- c(step1$id.var, step1$time.var,
      if (!is.null(g.var)) step1$g.var else NULL, "global.malmquist",
      "effch", "best.practice.ch", if (!is.null(g.var)) "tech.gap.ratioch" else NULL)
  }
  res.tfp <- list(Levels = res_globalmalm_loop, Changes = data.indices)
  class(res.tfp) <- c("list", "GlobalMalmquist")
  return(res.tfp)
}

globalmalm.1 <- function(data, step1, year.vec, orientation) {
  XGREF <- t(as.matrix(data[, step1$x.vars]))
  YGREF <- t(as.matrix(data[, step1$y.vars]))
  p <- progressor(along = 1:length(year.vec))
  res3 <- foreach(ano = 1:length(year.vec), .combine = rbind) %dopar%
    {
      X1 <- t(as.matrix(data[data[, step1$time.var] ==
        year.vec[ano], step1$x.vars]))
      Y1 <- t(as.matrix(data[data[, step1$time.var] ==
        year.vec[ano], step1$y.vars]))
      XREF1 <- X1
      YREF1 <- Y1
      # Malmquist components
      res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] ==
        year.vec[ano], step1$id.var]), .combine = rbind) %do%
        {
          if (orientation == "out") {
          DOGt <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
          DOt <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          res1 <- c(DOGt = DOGt, DOt = DOt, BPGt = DOGt/DOt)
          } else {
          DIGt <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
          DIt <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          res1 <- c(DIGt = DIGt, DIt = DIt, BPGt = DIt/DIGt)
          }
          return(res1)
        }
      p(sprintf("Period = %g", year.vec[ano]))
      return(res2)
    }
  return(res3)
}

globalmalm.1M <- function(data, step1, year.vec, orientation,
  metaFactors) {
  XGREF <- t(as.matrix(data[, step1$x.vars]))
  YGREF <- t(as.matrix(data[, step1$y.vars]))
  p <- progressor(along = 1:length(metaFactors))
  res4 <- foreach(mF = metaFactors, .combine = rbind) %dopar%
    {
      XGREFM <- t(as.matrix(data[data[, step1$g.var] ==
        mF, step1$x.vars]))
      YGREFM <- t(as.matrix(data[data[, step1$g.var] ==
        mF, step1$y.vars]))
      res3 <- foreach(ano = 1:length(year.vec), .combine = rbind) %do%
        {
          X1M <- t(as.matrix(data[data[, step1$g.var] ==
          mF & data[, step1$time.var] == year.vec[ano],
          step1$x.vars]))
          Y1M <- t(as.matrix(data[data[, step1$g.var] ==
          mF & data[, step1$time.var] == year.vec[ano],
          step1$y.vars]))
          XREF1M <- X1M
          YREF1M <- Y1M
          # Malmquist components
          res2 <- foreach(dmu = 1:ncol(X1M), .combine = rbind) %do%
          {
            if (orientation == "out") {
            DOGt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
            DOMt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREFM, YREF = YGREFM,
              rts = "crs")
            DOt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XREF1M, YREF = YREF1M,
              rts = "crs")
            res1 <- c(DOGt = DOGt, DOt = DOt, DOMt = DOMt,
              BPGt = DOMt/DOt, TGRt = DOGt/DOMt)
            } else {
            DIGt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
            DIMt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREFM, YREF = YGREFM,
              rts = "crs")
            DIt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XREF1M, YREF = YREF1M,
              rts = "crs")
            res1 <- c(DIGt = DIGt, DIt = DIt, DIMt = DIMt,
              BPGt = DIt/DIMt, TGRt = DIMt/DIGt)
            }
            return(res1)
          }
          res2 <- cbind(Group = mF, as.data.frame(res2))
          return(res2)
        }
      p(sprintf("Group Frontier = %s", mF))
      return(res3)
    }
  return(res4)
}

globalmalm.1W <- function(data, step1, year.vec, orientation,
  window) {
  windowMat <- windowFun(year.vec, window)
  XGREF <- t(as.matrix(data[, step1$x.vars]))
  YGREF <- t(as.matrix(data[, step1$y.vars]))
  p <- progressor(along = 1:dim(windowMat)[1])
  res3 <- foreach(wd = 1:dim(windowMat)[1], .combine = rbind) %dopar%
    {
      if (wd == 1) {
        X1 <- t(as.matrix(data[data[, step1$time.var] %in%
          windowMat[wd, ], step1$x.vars]))
        Y1 <- t(as.matrix(data[data[, step1$time.var] %in%
          windowMat[wd, ], step1$y.vars]))
      } else {
        X1 <- t(as.matrix(data[data[, step1$time.var] ==
          windowMat[wd, window], step1$x.vars]))
        Y1 <- t(as.matrix(data[data[, step1$time.var] ==
          windowMat[wd, window], step1$y.vars]))
      }
      XREF1 <- t(as.matrix(data[data[, step1$time.var] %in%
        windowMat[wd, ], step1$x.vars]))
      YREF1 <- t(as.matrix(data[data[, step1$time.var] %in%
        windowMat[wd, ], step1$y.vars]))
      # Malmquist components
      res2 <- foreach(dmu = 1:ncol(X1), .combine = rbind) %do%
        {
          if (orientation == "out") {
          DOGt <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
          DOt <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          res1 <- c(DOGt = DOGt, DOt = DOt, BPGt = DOGt/DOt)
          } else {
          DIGt <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
          DIt <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          res1 <- c(DIGt = DIGt, DIt = DIt, BPGt = DIt/DIGt)
          }
          return(res1)
        }
      p(sprintf("Window = %g", wd))
      return(res2)
    }
  return(res3)
}

globalmalm.1WM <- function(data, step1, year.vec, orientation,
  window, metaFactors) {
  XGREF <- t(as.matrix(data[, step1$x.vars]))
  YGREF <- t(as.matrix(data[, step1$y.vars]))
  p <- progressor(along = 1:length(metaFactors))
  res4 <- foreach(mF = metaFactors, .combine = rbind) %dopar%
    {
      XGREFM <- t(as.matrix(data[data[, step1$g.var] ==
        mF, step1$x.vars]))
      YGREFM <- t(as.matrix(data[data[, step1$g.var] ==
        mF, step1$y.vars]))
      windowMat <- windowFun(year.vec, window[which(metaFactors ==
        mF)])
      res3 <- foreach(wd = 1:dim(windowMat)[1], .combine = rbind) %do%
        {
          if (wd == 1) {
          X1M <- t(as.matrix(data[data[, step1$time.var] %in%
            windowMat[wd, ] & data[, step1$g.var] ==
            mF, step1$x.vars]))
          Y1M <- t(as.matrix(data[data[, step1$time.var] %in%
            windowMat[wd, ] & data[, step1$g.var] ==
            mF, step1$y.vars]))
          } else {
          X1M <- t(as.matrix(data[data[, step1$time.var] ==
            windowMat[wd, window[which(metaFactors ==
            mF)]] & data[, step1$g.var] == mF, step1$x.vars]))
          Y1M <- t(as.matrix(data[data[, step1$time.var] ==
            windowMat[wd, window[which(metaFactors ==
            mF)]] & data[, step1$g.var] == mF, step1$y.vars]))
          }
          XREF1M <- t(as.matrix(data[data[, step1$time.var] %in%
          windowMat[wd, ] & data[, step1$g.var] ==
          mF, step1$x.vars]))
          YREF1M <- t(as.matrix(data[data[, step1$time.var] %in%
          windowMat[wd, ] & data[, step1$g.var] ==
          mF, step1$y.vars]))
          # Malmquist components
          res2 <- foreach(dmu = 1:ncol(X1M), .combine = rbind) %do%
          {
            if (orientation == "out") {
            DOGt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
            DOMt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREFM, YREF = YGREFM,
              rts = "crs")
            DOt <- DO.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XREF1M, YREF = YREF1M,
              rts = "crs")
            res1 <- c(DOGt = DOGt, DOt = DOt, DOMt = DOMt,
              BPGt = DOMt/DOt, TGRt = DOGt/DOMt)
            } else {
            DIGt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREF, YREF = YGREF, rts = "crs")
            DIMt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XGREFM, YREF = YGREFM,
              rts = "crs")
            DIt <- DI.sh(XOBS = X1M[, dmu], YOBS = Y1M[,
              dmu], XREF = XREF1M, YREF = YREF1M,
              rts = "crs")
            res1 <- c(DIGt = DIGt, DIt = DIt, DIMt = DIMt,
              BPGt = DIt/DIMt, TGRt = DIMt/DIGt)
            }
            return(res1)
          }
          res2 <- cbind(Group = mF, as.data.frame(res2))
          return(res2)
        }
      p(sprintf("Group Frontier = %s", mF))
      return(res3)
    }
  return(res4)
}
