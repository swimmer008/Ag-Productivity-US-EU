# Data check for price based indices (lowe ...) ----------

check.lowe <- function(data, id.var, time.var, x.vars, y.vars,
  w.vars, p.vars) {
  if (!(is.data.frame(data))) {
    stop("data must be a dataframe")
  }
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) {
      stop("Too many id variables (id.var). Only one can be defined",
        call. = FALSE)
    }
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) {
      stop("Unrecognizable variable in id.var: ", paste(id.var),
        call. = FALSE)
    }
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) {
      stop("Too many time variables (time.var). Only one can be defined",
        call. = FALSE)
    }
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) {
      stop("Unrecognizable variable in time.var:", paste(time.var),
        call. = FALSE)
    }
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
    if (!(all(var_logical))) {
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical ==
        FALSE], collapse = ","), call. = FALSE)
    }
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
    if (!(all(var_logical))) {
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical ==
        FALSE], collapse = ","), call. = FALSE)
    }
  }
  if (missing(w.vars)) {
    stop("Missing w variables (w.vars)", call. = FALSE)
  } else {
    if (is.numeric(w.vars)) {
      w.vars.1 <- names.var[w.vars]
    } else {
      w.vars.1 <- w.vars
    }
    var_logical <- w.vars.1 %in% names.var
    if (!(all(var_logical))) {
      stop("Unrecognizable variables in w.vars:", paste(w.vars[var_logical ==
        FALSE], collapse = ","), call. = FALSE)
    }
  }
  if (missing(p.vars)) {
    stop("Missing p variables (p.vars)", call. = FALSE)
  } else {
    if (is.numeric(p.vars)) {
      p.vars.1 <- names.var[p.vars]
    } else {
      p.vars.1 <- p.vars
    }
    var_logical <- p.vars.1 %in% names.var
    if (!(all(var_logical))) {
      stop("Unrecognizable variables in p.vars:", paste(p.vars[var_logical ==
        FALSE], collapse = ","), call. = FALSE)
    }
  }
  if (length(w.vars) != length(x.vars)) {
    stop("x.vars and w.vars must be of the same length",
      call. = FALSE)
  }
  if (length(p.vars) != length(y.vars)) {
    stop("y.vars and p.vars must be of the same length",
      call. = FALSE)
  }
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1,
    y.vars = y.vars.1, w.vars = w.vars.1, p.vars = p.vars.1)
}

# Lowe first step (with technical change) ----------

lo.1 <- function(data, step1, year.vec, tech.reg, rts, orientation,
  PRICESO, PRICESI, mean.x, mean.y, shadow) {
  p <- progressor(along = 1:(length(year.vec)))
  res3 <- foreach(ano = 1:(length(year.vec)), .combine = rbind) %dopar%
    {
      X1 <- t(as.matrix(data %>%
        dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
        select(step1$x.vars)))
      Y1 <- t(as.matrix(data %>%
        dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
        select(step1$y.vars)))
      if (tech.reg == TRUE) {
        XREF1 <- X1
        YREF1 <- Y1
      } else {
        XREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% year.vec[1:ano]) %>%
          select(step1$x.vars)))
        YREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% year.vec[1:ano]) %>%
          select(step1$y.vars)))
      }
      P1 <- sweep(t(as.matrix(data %>%
        dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
        select(step1$p.vars))), MARGIN = 1, STATS = mean.y,
        FUN = "*")
      W1 <- sweep(t(as.matrix(data %>%
        dplyr::filter(get(step1$time.var) == year.vec[ano]) %>%
        select(step1$w.vars))), MARGIN = 1, STATS = mean.x,
        FUN = "*")
      res2 <- foreach(dmu = 1:ncol(X1), .combine = rbind) %dopar%
        {
          AO <- sum(PRICESO * Y1[, dmu])
          AI <- sum(PRICESI * X1[, dmu])
          TFP <- AO/AI
          MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu],
          XREF = XREF1, YREF = YREF1, PRICESO, PRICESI,
          rts)
          TFPE <- TFP/MP
          if (shadow == TRUE) {
          SHO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          SHI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          } else {
          SHO <- NULL
          SHI <- NULL
          }
          if (orientation == "out") {
          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
          RE <- DO.ooe(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, PRICESO,
            rts)
          OME <- RE/OTE
          ROSE <- ((AO/RE)/AI)/MP
          OSME <- OME * ROSE
          RME <- TFPE/(OTE * OSE)
          REV <- sum(Y1[, dmu] * P1[, dmu])
          COST <- sum(X1[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF,
            P = P, W = W, TT = TT, AO = AO, AI = AI,
            TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE,
            OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME,
            RME = RME, RE = RE, PRICEI = SHI, PRICEO = SHO)
          } else {
          if (orientation == "in") {
            ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
            ISE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/ITE
            CE <- DI.oie(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, PRICESI,
            rts)
            IME <- CE/ITE
            RISE <- (AO/(AI * CE))/MP
            ISME <- IME * RISE
            RME <- TFPE/(ITE * ISE)
            REV <- sum(Y1[, dmu] * P1[, dmu])
            COST <- sum(X1[, dmu] * W1[, dmu])
            PROF <- REV/COST
            P <- REV/AO
            W <- COST/AI
            TT <- P/W
            res1 <- c(REV = REV, COST = COST, PROF = PROF,
            P = P, W = W, TT = TT, AO = AO, AI = AI,
            TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE,
            ISE = ISE, IME = IME, RISE = RISE, ISME = ISME,
            RME = RME, CE = CE, PRICEI = SHI, PRICEO = SHO)
          }
          }
          return(res1)
        }
      p(sprintf("Period = %g", year.vec[ano]))
      res2
    }
  res3
}

# Lowe first step (without technical change) ----------

lo.2 <- function(data, step1, rts, orientation, PRICESO, PRICESI,
  mean.x, mean.y, shadow) {
  X1 <- t(as.matrix(data[, step1$x.vars]))
  Y1 <- t(as.matrix(data[, step1$y.vars]))
  XREF1 <- X1
  YREF1 <- Y1
  P1 <- sweep(t(as.matrix(data[, step1$p.vars])), MARGIN = 1,
    STATS = mean.y, FUN = "*")
  W1 <- sweep(t(as.matrix(data[, step1$w.vars])), MARGIN = 1,
    STATS = mean.x, FUN = "*")
  p <- progressor(along = 1:ncol(X1))
  res2 <- foreach(dmu = 1:ncol(X1), .combine = rbind, .options.future = list(chunk.size = round(ncol(X1)/max(1,
    detectCores() - 1)))) %dopar% {
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1,
      YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    if (shadow == TRUE) {
      SHO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu],
        XREF = XREF1, YREF = YREF1, rts)
      SHI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu],
        XREF = XREF1, YREF = YREF1, rts)
    } else {
      SHO <- NULL
      SHI <- NULL
    }
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu],
        XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu],
        XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      RE <- DO.ooe(XOBS = X1[, dmu], YOBS = Y1[, dmu],
        XREF = XREF1, YREF = YREF1, PRICESO, rts)
      OME <- RE/OTE
      ROSE <- ((AO/RE)/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/(OTE * OSE)
      REV <- sum(Y1[, dmu] * P1[, dmu])
      COST <- sum(X1[, dmu] * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P,
        W = W, TT = TT, AO = AO, AI = AI, TFP = TFP,
        MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME,
        ROSE = ROSE, OSME = OSME, RME = RME, RE = RE,
        PRICEI = SHI, PRICEO = SHO)
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
          dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
          dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/ITE
        CE <- DI.oie(XOBS = X1[, dmu], YOBS = Y1[, dmu],
          XREF = XREF1, YREF = YREF1, PRICESI, rts)
        IME <- CE/ITE
        RISE <- (AO/(AI * CE))/MP
        ISME <- IME * RISE
        RME <- TFPE/(ITE * ISE)
        REV <- sum(Y1[, dmu] * P1[, dmu])
        COST <- sum(X1[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF,
          P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP,
          MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE,
          IME = IME, RISE = RISE, ISME = ISME, RME = RME,
          CE = CE, PRICEI = SHI, PRICEO = SHO)
      }
    }
    p(sprintf("Observation = %g", dmu))
    return(res1)
  }
  res2
}

# Lowe first step (with window and technical change)
# ----------

lo.3 <- function(data, step1, year.vec, tech.reg, rts, orientation,
  PRICESO, PRICESI, mean.x, mean.y, shadow, window) {
  windowMat <- windowFun(year.vec, window)
  p <- progressor(along = 1:dim(windowMat)[1])
  res3 <- foreach(wd = 1:dim(windowMat)[1], .combine = rbind) %dopar%
    {
      if (wd == 1) {
        X1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          ]) %>%
          select(step1$x.vars)))
        Y1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          ]) %>%
          select(step1$y.vars)))
        XREF1 <- X1
        YREF1 <- Y1
        P1 <- sweep(t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          ]) %>%
          select(step1$p.vars))), MARGIN = 1, STATS = mean.y,
          FUN = "*")
        W1 <- sweep(t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          ]) %>%
          select(step1$w.vars))), MARGIN = 1, STATS = mean.x,
          FUN = "*")
      } else {
        X1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) == windowMat[wd,
          window]) %>%
          select(step1$x.vars)))
        Y1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) == windowMat[wd,
          window]) %>%
          select(step1$y.vars)))
        if (tech.reg == TRUE) {
          XREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
            ]) %>%
          select(step1$x.vars)))
          YREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
            ]) %>%
          select(step1$y.vars)))
        } else {
          XREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[1:wd,
            ]) %>%
          select(step1$x.vars)))
          YREF1 <- t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[1:wd,
            ]) %>%
          select(step1$y.vars)))
        }
        P1 <- sweep(t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          window]) %>%
          select(step1$p.vars))), MARGIN = 1, STATS = mean.y,
          FUN = "*")
        W1 <- sweep(t(as.matrix(data %>%
          dplyr::filter(get(step1$time.var) %in% windowMat[wd,
          window]) %>%
          select(step1$w.vars))), MARGIN = 1, STATS = mean.x,
          FUN = "*")
      }
      res2 <- foreach(dmu = 1:ncol(X1), .combine = rbind) %dopar%
        {
          AO <- sum(PRICESO * Y1[, dmu])
          AI <- sum(PRICESI * X1[, dmu])
          TFP <- AO/AI
          MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu],
          XREF = XREF1, YREF = YREF1, PRICESO, PRICESI,
          rts)
          TFPE <- TFP/MP
          if (shadow == TRUE) {
          SHO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          SHI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          } else {
          SHO <- NULL
          SHI <- NULL
          }
          if (orientation == "out") {
          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
          RE <- DO.ooe(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, PRICESO,
            rts)
          OME <- RE/OTE
          ROSE <- ((AO/RE)/AI)/MP
          OSME <- OME * ROSE
          RME <- TFPE/(OTE * OSE)
          REV <- sum(Y1[, dmu] * P1[, dmu])
          COST <- sum(X1[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF,
            P = P, W = W, TT = TT, AO = AO, AI = AI,
            TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE,
            OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME,
            RME = RME, RE = RE, PRICEI = SHI, PRICEO = SHO)
          } else {
          if (orientation == "in") {
            ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts)
            ISE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/ITE
            CE <- DI.oie(XOBS = X1[, dmu], YOBS = Y1[,
            dmu], XREF = XREF1, YREF = YREF1, PRICESI,
            rts)
            IME <- CE/ITE
            RISE <- (AO/(AI * CE))/MP
            ISME <- IME * RISE
            RME <- TFPE/(ITE * ISE)
            REV <- sum(Y1[, dmu] * P1[, dmu])
            COST <- sum(X1[, dmu] * W1[, dmu])
            PROF <- REV/COST
            P <- REV/AO
            W <- COST/AI
            TT <- P/W
            res1 <- c(REV = REV, COST = COST, PROF = PROF,
            P = P, W = W, TT = TT, AO = AO, AI = AI,
            TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE,
            ISE = ISE, IME = IME, RISE = RISE, ISME = ISME,
            RME = RME, CE = CE, PRICEI = SHI, PRICEO = SHO)
          }
          }
          return(res1)
        }
      p(sprintf("Window = %g", wd))
      res2
    }
  res3
}

# Lowe print fonction ----------

print.Lowe <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nLowe productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
  cat("\n\nLowe productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]][-c(1:2)], digits = digits),
    digits = digits)
  if (!is.null(x[["Shadowp"]])) {
    cat("\n\nLowe productivity shadow prices (summary):\n\n")
    print(summary(x[["Shadowp"]][-c(1:2)], digits = digits),
      digits = digits)
  }
  cat("\n")
  invisible(x)
}
