#' Simulate Observational Data
#'
#' Function to simulate observational data using the algorithm in Young and Tchetgen Tchetgen (2014)
#'
#' @param ns number of subjects
#' @param nv no of visits including baseline visit
#' @param conf coefficient of X2 in the treatment and outcome models, controlling confounding strength of X2
#' @param treat_prev intercept of the treatment model, controlling overall treatment prevalence
#' @param all_treat (TRUE/FALSE)= enforcing treatments as if in an RCT
#' @param all_control (TRUE/FALSE)= enforcing non-treatments/control as if in an RCT
#' @param censor (TRUE/FALSE)= adding dependent censoring or not
#'
#' @importFrom stats ave rbinom rnorm
#'
#' @noRd
data_gen_censored <- function(ns,
                              nv,
                              conf = 0.5,
                              treat_prev = 0,
                              all_treat = FALSE,
                              all_control = FALSE,
                              censor = TRUE) {
  assert_flag(all_treat)
  assert_flag(all_control)
  assert_flag(censor)
  nvisit <- nv + 1

  X1 <- rep(0, nvisit * ns) ## place holders for time-varying covariates
  X2 <- rep(0, nvisit * ns) ## place holders for time-varying covariates
  Z2 <- rnorm(nvisit * ns, 0, 1)
  X3 <- rep(rbinom(ns, 1, 0.5), each = nvisit) # gender
  X4 <- rep(rnorm(ns, 0, 1), each = nvisit) # baseline continuous covariate
  age <- round(rnorm(ns, 35, 12), digits = 0) ## age at baseline
  age <- rep(age, each = nvisit) + rep((-1):(nv - 1), ns) ## age at each visit


  A <- rep(0, nvisit * ns) ## place holders for current  treatments
  Ap <- rep(0, nvisit * ns) ## place holders for  previous treatments
  CAp <- rep(0, nvisit * ns) ## place holders for sum of previous treatment A



  Y <- rep(0, nvisit * ns) ## place holders for outcome vector
  Yp <- rep(0, nvisit * ns) ## place holders for previous outcome vector

  ## Fill in initial values
  seq1 <- seq(1, nvisit * ns - nv, nvisit)

  X1[seq1] <- 0 ###  time-varying covariates at visit -1, by convention set at 0
  X2[seq1] <- 0

  P0 <- list() ## list of treatment probabilities
  P0[[1]] <- rep(0, ns)
  seqlist <- list()
  seqlist[[1]] <- seq1
  CAp[seq1] <- rep(0, ns)

  for (k in 2:nvisit) {
    ## update covariates


    seqlist[[k]] <- seqlist[[k - 1]] + 1
    Ap[seqlist[[k]]] <- A[seqlist[[k - 1]]]
    CAp[seqlist[[k]]] <- CAp[seqlist[[k - 1]]] + Ap[seqlist[[k]]]

    X1P0 <- 1 / (1 + exp(Ap[seqlist[[k]]]))
    X1[seqlist[[k]]] <- rbinom(ns, 1, X1P0) ## binary time-varying confounder

    X2[seqlist[[k]]] <- Z2[seqlist[[k]]] - 0.3 * Ap[seqlist[[k]]] ## continuous time-varying confounder

    ## update treatment
    lpp <- as.numeric(treat_prev) + Ap[seqlist[[k]]] + 0.5 * X1[seqlist[[k]]] + as.numeric(conf) * X2[seqlist[[k]]] -
      0.2 * X3[seqlist[[k]]] + X4[seqlist[[k]]] - 0.3 * (age[seqlist[[k]]] - 35) / 12
    P0[[k]] <- 1 / (1 + exp(-lpp))

    if (all_treat) {
      A[seqlist[[k]]] <- 1.0
    } else {
      if (all_control) {
        A[seqlist[[k]]] <- 0.0
      } else {
        ## Generate treatment at current visit based on  covariates, previous treatment
        A[seqlist[[k]]] <- rbinom(ns, 1, P0[[k]])
      }
    }
    ## Generate outcome

    lp <- -5 - 1.2 * A[seqlist[[k]]] + 0.5 * X1[seqlist[[k]]] + as.numeric(conf) * X2[seqlist[[k]]] +
      X3[seqlist[[k]]] + X4[seqlist[[k]]] + 0.5 * (age[seqlist[[k]]] - 35) / 12

    Yp[seqlist[[k]]] <- Y[seqlist[[k - 1]]]
    Y[seqlist[[k]]] <- (rbinom(ns, 1, 1 / (1 + exp(-lp)))) * as.numeric(Yp[seqlist[[k]]] == 0) +
      as.numeric(Yp[seqlist[[k]]] == 1)
  }

  ## Make data frame
  ID <- rep(1:ns, each = nv)

  ## Align data by removing values
  NSEQ <- seq1

  X1 <- X1[-NSEQ]
  X2 <- X2[-NSEQ]
  X3 <- X3[-NSEQ]
  X4 <- X4[-NSEQ]
  age <- age[-NSEQ]

  A <- A[-NSEQ]
  Ap <- Ap[-NSEQ]
  CAp <- CAp[-NSEQ]
  Y <- Y[-seq(1, nvisit * ns - nv, nvisit)]
  Yp <- Yp[-seq(1, nvisit * ns - nv, nvisit)]

  ## Create data frame

  DATA <- data.frame(ID, t = rep(c(0:(nv - 1)), ns), A, Ap, CAp, X1, X2, X3, X4, age, Y, Yp)
  ## eligibility criteria: age>=18, had no treatment so far, no event so far
  DATA$eligible <- as.numeric(DATA$age >= 18 & CAp == 0 & Yp == 0)


  ## censoring
  if (censor) {
    ## Probability of dropout
    Dprob <- 1 / (1 + exp(1 + Ap + 0.5 * X1 - 0.5 * X2 + 0.2 * X3 - 0.2 * X4 + (age - 35) / 12))

    DATA$C <- rbinom(nv * ns, 1, Dprob) ## C=0 is remain in the study

    indfun <- function(n) {
      if (sum(n) == 0) {
        rep(0, nv)
      } else {
        k <- min(which(n == 1))
        c(rep(0, k), rep(1, nv - k))
      }
    }

    RL <- ave(DATA$C, DATA$ID, FUN = indfun)


    eligCum <- ave(DATA$eligible, DATA$ID, FUN = cumsum)

    DATA$age_s <- (DATA$age - 35) / 12

    # remove observations after event occurrence and censoring, and not eligible
    DATA2 <- DATA[RL == 0 & DATA$Yp == 0 & eligCum > 0, ]
    DATA2[, c("ID", "t", "A", "X1", "X2", "X3", "X4", "age", "age_s", "Y", "C", "eligible")]
  } else {
    eligCum <- ave(DATA$eligible, DATA$ID, FUN = cumsum)

    DATA$age_s <- (DATA$age - 35) / 12
    DATA2 <- DATA[DATA$Yp == 0 & eligCum > 0, ]
    DATA2[, c("ID", "t", "A", "X1", "X2", "X3", "X4", "age", "age_s", "Y", "C", "eligible")]
  }
}
