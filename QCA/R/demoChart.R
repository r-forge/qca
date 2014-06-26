`demoChart` <-
function(primes = c(""), configs = c(""), prod.split="") {
    if (prod.split != "") prod.split <- paste("\\", prod.split, sep="")
    
    primes.split <- strsplit(primes, prod.split)
    configs.split <- strsplit(configs, prod.split)
    
    mtrx <- matrix(FALSE, nrow=length(primes), ncol=length(configs))
    
    for (i in seq(nrow(mtrx))) {
        for (j in seq(ncol(mtrx))) {
            mtrx[i, j] <- all(primes.split[[i]] %in% configs.split[[j]])
        }
    }
    
    colnames(mtrx) <- configs
    rownames(mtrx) <- primes
    return(mtrx)
}

