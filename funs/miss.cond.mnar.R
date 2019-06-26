miss.cond.mnar <- function(do.target, p.cond.y1, p.y1){
  (do.target - p.cond.y1*p.y1)/(1 - p.y1)
}