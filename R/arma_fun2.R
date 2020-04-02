arma_fun2 <- function(coef, x, y,
                      LOWER, UPPER,constraints, penalty, nms, arma){
  names(coef) = nms
  coef_reg = coef[0:ncol(x)]
  coef_arma = coef[(ncol(x)+1):length(coef)]

  if(is.null(constraints)){
    z = 0
  }else{
    text_split = strsplit(constraints, ',')[[1]]
    TH <- data.table::setDT(as.list(coef))
    z = sapply(text_split, function(cond){
      with(TH, !eval(rlang::parse_expr(cond)))
    })
    z = any(z) * 1
  }

  trarma <- ArimaTransf(coef_arma, arma)
  n.cond = arma[1] + arma[5] * arma[3]
  res <- ArimaEstimation(y - x %*% coef_reg,
                         arma, trarma[[1L]], trarma[[2L]],
                         n.cond, F)

  0.5 * log(res) + any(coef > UPPER) * penalty +
    any(coef < LOWER) * penalty + z * penalty

}
