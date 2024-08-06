

rma_dl <- function(y, v){
  # Dersimonian and Laird estimate of tau2
  Wi <- 1/v
  sum_wi <- sum(Wi)
  Q <- sum(Wi*y^2) - (sum(Wi*y)^2/sum_wi)
  df <- length(y) - 1
  C <- sum_wi - (sum(Wi^2)/sum_wi)
  tau2_est <- max(0, (Q - df)/C)

  Wistar <- 1/(v + tau2_est)

  y_pooled <- sum(Wistar*y)/sum(Wistar)
  v_y_pooled <- 1/sum(Wistar)
  se_y_pooled <- sqrt(v_y_pooled)
  #$return(list(y_pooled, v_y_pooled, se_y_pooled))
  out <- structure(list(b = structure(y_pooled, dim = c(1L, 1L
  ), dimnames = list("intrcpt", NULL)), beta = structure(y_pooled, dim = c(1L,
                                                                           1L), dimnames = list("intrcpt", NULL)), se = se_y_pooled,
  zval = y_pooled/se_y_pooled, pval = pnorm(abs(y_pooled), sd = se_y_pooled, lower.tail = FALSE)*2, ci.lb = y_pooled - 1.96*se_y_pooled,
  ci.ub = y_pooled + 1.96*se_y_pooled, vb = structure(0.00201396498309675, dim = c(1L,
                                                                                   1L), dimnames = list("intrcpt", "intrcpt")), tau2 = tau2_est,
  measure = "GEN",
  method = "DL", model = "rma.uni", weighted = TRUE, test = "z",
  parms = 2, int.only = TRUE, int.incl = TRUE, intercept = TRUE,
  allvipos = TRUE, coef.na = c(X = FALSE),

  se.tau2 = NA, tau2.fix = FALSE, tau2.f = tau2_est,
  I2 = NA, H2 = NA, R2 = NULL,
  vt = NA, QE = NA, QEp = NA,
  QM = NA, QMdf = c(1L, NA), QMp = NA,
  k = length(y), k.f = length(y), k.eff = length(y), k.all = length(y), p = 1L, p.eff = 1L

  #
  #   tau2 = c(object$rma_before$tau2, object$rma_after$tau2),
  #   tau2_SE = c(object$rma_before$se.tau2, object$rma_after$se.tau2),
  #   `I^2` = c(object$rma_before$I2, object$rma_after$I2),
  #   `H^2` = c(object$rma_before$H2, object$rma_after$H2),
  #   "Q-test" = c(object$rma_before$QE, object$rma_after$QE),
  #   df = c(object$rma_before$k - object$rma_before$p, object$rma_after$k - object$rma_after$p),
  #   Q_p = c(object$rma_before$QEp, object$rma_after$QEp),
  #   Intercept = c(object$rma_before$beta, object$rma_after$beta),
  #   se = c(object$rma_before$se, object$rma_after$se),
  #   ci.lb = c(object$rma_before$ci.lb, object$rma_after$ci.lb),
  #   ci.ub = c(object$rma_before$ci.ub, object$rma_after$ci.ub),
  #   p = c(object$rma_before$pval, object$rma_after$pval)
  # )

  ), class = c("rma.uni",
                                                                                                                                                            "rma"))
  return(out)

}
