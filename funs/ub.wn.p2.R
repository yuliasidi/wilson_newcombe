ub.wn.p2 <- function (pc, lb.pc, ub.pc, pt, lb.pt, ub.pt){
  pc - pt + sqrt( (ub.pc - pc)^2 + (pt - lb.pt)^2 )
}
