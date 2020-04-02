


#' @useDynLib ConsReg ConsReg_Estimation
ArimaEstimation <- function(...) .Call(ConsReg_Estimation, ...)

#' @useDynLib ConsReg ConsReg_transPars2
ArimaTransf <- function(...) .Call(ConsReg_transPars2, ...)

#' @useDynLib ConsReg ConsReg_Matrix
ArimaMatrix <- function(...) .Call(ConsReg_Matrix, ...)
