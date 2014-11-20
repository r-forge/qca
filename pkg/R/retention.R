`retention` <- 
function(data, outcome = "", conditions = c(""), type = "error",
         assump = "independent", n.cut = 1, incl.cut = 1, p.pert = 0.1,
         n.pert = 1) {
    
    names(mydata) <- toupper(names(data))
    conditions <- toupper(conditions)
    outcome <- toupper(outcome)
    if (any(conditions == "")) {
        conditions <- names(data)[which(names(data) != outcome)]
    }
    
    data <- data[, c(conditions, outcome)]
    
    # unique combinations
    udata <- unique(data[, conditions])
    rownames(udata) <- seq(nrow(udata))
    cpos <- cneg <- rep(0, nrow(udata))
    
    # count how many combinations are positive and how many are negative
    for (i in seq(nrow(udata))) {
        for (j in seq(nrow(data))) {
            if (all(udata[i, ] == data[j, conditions])) {
                if (data[j, outcome] == 1) {
                    cpos[i] <- cpos[i] + 1 # positive
                }
                else if (data[j, outcome] == 0) {
                    cneg[i] <- cneg[i] + 1 # negative
                }
            }
        }
    }
    total <- cpos + cneg
    udata <- udata[total >= n.cut, , drop = FALSE]
    cpos <- cpos[total >= n.cut]
    cneg <- cneg[total >= n.cut]
    total <- total[total >= n.cut]
    calculatePairs <- function(x, n.pert, type = "deletion") {
        pairsxl <- combn(nrow(udata), x)
        nofsetsxl <- 0
        for (j in seq(ncol(pairsxl))) {
            cposneg <- NULL
            for (l in seq(x)) {
                cposneg <- c(cposneg, cbind(cpos, cneg)[pairsxl[l, j], ])
            }
            allpairs <- createMatrix(cposneg + 2) - 1
            allpairs <- allpairs[apply(allpairs, 1, function(y) all(y >= 0)), , drop = FALSE]
            linesubset <- rep(TRUE, nrow(allpairs))
            for (l in seq(x)) {
        
                # at least one measurement error for each configuration l
                linesubset <- linesubset & rowSums(allpairs[, l*2 - c(1, 0)]) >= 1
            }
            allpairs <- allpairs[linesubset & rowSums(allpairs) <= n.pert, , drop = FALSE]
            for (i in seq(nrow(allpairs))) {
                lchanges <- rep(FALSE, x)
                for (l in seq(x)) {
                    initially <- cpos[pairsxl[l, j]]/total[pairsxl[l, j]]
                    if (type == "deletion") {
                        newtotaless <- total[pairsxl[l, j]] - allpairs[i, l*2 - 1]
                        after <- (cpos[pairsxl[l, j]] - allpairs[i, l*2 - 1])/newtotaless
                        lchanges[l] <- ((initially >= incl.cut & after <  incl.cut) | newtotaless <  n.cut) |
                                       ((initially <  incl.cut & after >= incl.cut) & newtotaless >= n.cut)
                    }
                    else if (type == "error") {
                        after <- (cpos[pairsxl[l, j]] + allpairs[i, l*2] - allpairs[i, l*2 - 1])/total[pairsxl[l, j]]
                        lchanges[l] <- (initially >= incl.cut & after <  incl.cut) |
                                       (initially <  incl.cut & after >= incl.cut)
                    }
                }
                if (all(lchanges)) {
                    combs <- 1
                    for (l in seq(x)) {
                        combs <- combs*choose(cpos[pairsxl[l, j]], allpairs[i, l*2 - 1])
                        combs <- combs*choose(cneg[pairsxl[l, j]], allpairs[i, l*2])
                    }
                    nofsetsxl <- nofsetsxl + combs*choose(nrow(dat) - sum(cposneg), n.pert - sum(allpairs[i, ]))
                }
            }
        }
        return(nofsetsxl)
    }
    if (assump == "dependent") {
        if (nrow(udata) < n.pert) {
            stop("\nCannot make pairs of ", n.pert, " lines with only ", nrow(udata), " unique configurations.\n\n", call. = FALSE)
        }
        
        # initialize number of sets of D cases that affect configurations of type l
        nofsets <- 0
        for (i in seq(n.pert)) {
            nofsetsxl <- calculatePairs(i, n.pert, type = type)
            nofsets <- nofsets + ifelse(i %% 2 == 1, nofsetsxl, -1*nofsetsxl)
        }
        return(as.vector(1 - nofsets/choose(nrow(dat), n.pert)))
    }
    else if (assump == "independent") {
        pfinal <- 1
        if (type == "deletion") {
            for (l in seq(nrow(udata))) {
                ptmp <- 1
                
                # calculate all possible pairs of positives and negatives
                allpairs <- createMatrix(c(cpos[l], cneg[l]) + 2) - 1
                allpairs <- allpairs[apply(allpairs, 1, function(x) all(x >= 0)), , drop = FALSE]
                
                # at least one positive or one negative must be deleted
                allpairs <- allpairs[rowSums(allpairs) >= 1, , drop = FALSE]
                
                # loop over all surviving combinations
                for (i in seq(nrow(allpairs))) {
                    # is new total (after deleting some cases) less than n.cut?
                    newtotaless <- total[l] - allpairs[i, 1] - allpairs[i, 2]
                    initially <- cpos[l]/total[l]
                    # first column  of all pairs is equivalent of "i" in formula;
                    # second column to "j"
                    after <- (cpos[l] - allpairs[i, 1])/newtotaless
                    # only valid for conservative solution type;
                    # if parsimonious, losing a negative initial combination due to new total less than n.cut might also change the solution
                    # here, we state new total at least n.cut only to make sure when a negative combination enters the group of positive ones
                    if (((initially >= incl.cut & after <  incl.cut) | newtotaless <  n.cut) |
                        ((initially <  incl.cut & after >= incl.cut) & newtotaless >= n.cut)) {
                           ptmp <- ptmp - dbinom(allpairs[i, 1], cpos[l], p.pert) * dbinom(allpairs[i, 2], cneg[l], p.pert)
                    }
                }
    
                # final probability gets changed, for each unique combination
                pfinal <- pfinal*ptmp
            }
        }
        else if (type == "error") {
            for (l in seq(nrow(udata))) {
                ptmp <- 1
                # calculate all possible pairs of positives and negatives
                allpairs <- createMatrix(c(cpos[l], cneg[l]) + 2) - 1
                allpairs <- allpairs[apply(allpairs, 1, function(x) all(x >= 0)), , drop = FALSE]
                # at least one positive or one negative must be changed
                allpairs <- allpairs[rowSums(allpairs) >= 1, , drop = FALSE]
                # loop over all surviving combinations
                for (i in seq(nrow(allpairs))) {
                    initially <- cpos[l]/total[l]
                    # first column  of allpairs is the equivalent of "i" in the formula, second column to "j"
                    after <- (cpos[l] - allpairs[i, 1] + allpairs[i, 2])/total[l]
                    if ((initially >= incl.cut & after < incl.cut) | (initially < incl.cut & after >= incl.cut)) {
                           ptmp <- ptmp - dbinom(allpairs[i, 1], cpos[l], p.pert) * dbinom(allpairs[i, 2], cneg[l], p.pert)
                    }
                }
                
                # final probability gets changed, for each unique combination
                pfinal <- pfinal*ptmp
            }
        }
        return(pfinal)
    }
}
