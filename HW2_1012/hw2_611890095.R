A <- matrix(c(25,-2,4,-2,4,1,4,1,9),3,3)
A
Avalues <- eigen(A)$values
Avectors <- eigen(A)$vectors
Avalues
Avectors
J <- Avectors%*%diag(Avalues)%*%t(Avectors)
J                   
Q <- t(matrix(Avalues))%*%A%*%matrix(Avalues)
Q
K <- Avectors%*%sqrt(diag(Avalues))%*%t(Avectors)
K
solve(K) #反矩陣
