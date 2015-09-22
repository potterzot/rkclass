# Statistical tests

#' Wald test






#wcp <- function(m, w) {
#  #wm = diag(sqrt(w)) %*% m
#  #wm = sweep(m, MARGIN=1, sqrt(w), `*`)
#  wm = m * sqrt(w)
#  qwm = qr(wm)
#  rwm = qr.R(qwm)
#  rr = crossprod(rwm)
#  print(rr)
#  print(wcrossprod(m,m,w))
#}
