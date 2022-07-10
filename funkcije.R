izracunajMetrike <- function(matrica) {
  TP <- matrica[2,2] # true positive
  TN <- matrica[1,1] # true negative
  FP <- matrica[1,2] # false positive
  FN <- matrica[2,1] # false negative
  a <- sum(diag(matrica)) / sum(matrica)
  p <- TP / (TP + FP)
  r <- TP / (TP + FN)
  F1 <- 2*p*r / (p + r)
  c(accuracy = a, precision = p, recall = r, F1 = F1)
}



