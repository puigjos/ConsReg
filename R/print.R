


#' @export
#' @rdname ConsReg

print.ConsReg <- function (x, digits = max(3L, getOption("digits") - 3L),
                           ...)
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n",
                         collapse = "\n"), "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
    x$metrics
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}



#' @export
#' @rdname ConsRegArima
print.ConsRegArima <- function (x, digits = max(3L, getOption("digits") - 3L),
                                ...)
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n",
                         collapse = "\n"), "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
    x$metrics
    cat(
      "\nsigma^2 estimated as ", format(x$sigma2, digits = digits),
      ":  AIC=", format(round(x$aic, 2L)), "\n", sep = ""
    )
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
