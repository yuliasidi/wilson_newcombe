lb.wn.p2 <- function (pc, lb.pc, ub.pc, pt, lb.pt, ub.pt){
  pc - pt - sqrt( (pc - lb.pc)^2 + (ub.pt - pt)^2 )
}
