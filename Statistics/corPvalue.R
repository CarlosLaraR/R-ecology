#Function to compute p-value of correlations.
#The output is a matrix with the p-value of the correlation
#Function obtained from http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram. 
#I stored this function in my personal repository to be used in my own code. Please give all the credit to sthda.com

# df is a data.frame
corPvalue<- function(df, ...) {
    mat.df <- as.matrix(df)
    n <- ncol(mat.df)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat.df[, i], mat.df[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat.df)
  p.mat
}

