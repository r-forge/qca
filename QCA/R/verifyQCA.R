`verify.data` <-
function(data, outcome = "", conditions = c("")) {
     # check if the data has column names
    if (is.null(colnames(data))) {
        cat("\n")
        stop("Please specify the column names for your data.\n\n", call. = FALSE)
    }
    
     # check the outcome specified by the user
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome set.\n\n", call. = FALSE)
    }
    else if (! outcome %in% colnames(data)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
    }
    
    # subset the data with the conditions specified
    if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
        }
        if (!all(conditions %in% names(data))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
        }
    }
    else if (nchar(conditions) > 0) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
        }
        else {
            cat("\n")
            stop("Cannot find a solution with only one causal condition.\n\n", call. = FALSE)
        }
    }
}

         
         
`verify.expl` <-
function(data, outcome = "", conditions = c(""), incl.rem = FALSE,
         expl.1 = FALSE, expl.0 = FALSE, expl.ctr = FALSE, expl.mo = FALSE,
         incl.1 = FALSE, incl.0 = FALSE, incl.ctr = FALSE, incl.mo = FALSE,
         quiet = FALSE, details = FALSE, chart = FALSE, use.letters = TRUE,
         show.cases = FALSE, diffmatrix=TRUE) {
    
    # check if all cases have been included in analysis
    if ((expl.0 | incl.0) & (expl.1 | incl.1) & (expl.ctr | incl.ctr) & incl.rem) {
        cat("\n")
        stop("You have included all cases in the analysis!\n\n", call. = FALSE)
    }
    
     # if more than 26 conditions (plus one outcome), we cannot use letters
    if (use.letters & ncol(data) > 27) {
        cat("\n")
        stop("Cannot use letters. There are more than 26 conditions.\n\n", call. = FALSE)
    }
    
    # check if the user specifies something to explain
    if (sum(expl.0, expl.1, expl.ctr) == 0 ) {
        cat("\n")
        stop("You have not specified what to explain.\n\n", call. = FALSE)
    }
    
    # checking for complete data (without missings)
    if (any(is.na(data))) {
        cat("\n")
        stop("Missing data found; this is not (yet) supported.\n\n", call. = FALSE)
    }
}


`verify.tt` <-
function(data, outcome = "", conditions = c(""), complete = FALSE, show.cases = FALSE, incl.cut1 = 1, incl.cut0 = 1, inf.test) {
    
    if (class(data) != "data.frame") {
        cat("\n")
        stop(paste("You have to provide a data frame, the current \"data\" argument contains an object\n",
                   "       of class \"", class(data), "\"",
                   ifelse(class(data) == "sS", ", created by superSubset()", ""),
                   ifelse(class(data) == "tt", ", created by truthTable()", ""),
                   ifelse(class(data) == "pof", ", created by pof()", ""),
        ".\n\n", sep=""), call. = FALSE)
    }
    
    if (is.tt(data)) {
        data <- data$initial.data
    }
    
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome set.\n\n", call. = FALSE)
    }
    else if (! outcome %in% colnames(data)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
    }
    
     # subset the data with the conditions specified
    if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('"', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
        }
        if (!all(conditions %in% colnames(data))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
        }
    }
    else if (nchar(conditions) > 0) {
        cat("\n")
        stop("Cannot create a truth table with only one condition.\n\n", call. = FALSE)
    }
    
    # checking for complete data (without missings)
    if (any(is.na(data))) {
        cat("\n")
        stop("Missing data found; this is not (yet) supported.\n\n", call. = FALSE)
    }
    
    # checking for the two including cut-offs
    if (any(c(incl.cut1, incl.cut0) < 0) | any(c(incl.cut1, incl.cut0) > 1)) {
        cat("\n")
        stop("The including cut-off(s) should be bound to the interval [0, 1].\n\n", call. = FALSE)
    }
    
    if (incl.cut0 > incl.cut1 & incl.cut0 < 1) {
        cat("\n")
        stop("incl.cut0 cannot be greater than incl.cut1.\n\n", call. = FALSE)
    }
    
    data <- data[, c(conditions, outcome)]
    
    data <- as.data.frame(lapply(data, function(x) {
        x <- as.character(x)
        x[x %in% c("-", "dc")] <- -1
        return(as.numeric(x))
    }))
    
    if (any(data[, conditions] > 1 & data[, conditions] %% 1 > 0)) {
        cat("\n")
        stop("Uncalibrated data.\nFuzzy sets should have values bound to the interval [0 , 1] and all other values should be crisp.\n\n", call. = FALSE)
    }
    
    verify.inf.test(inf.test, data)
}


`verify.qca` <-
function(data, outcome = "", conditions = c(""), explain = c(""),
         include = c(""), use.letters = FALSE) {

     # check if the data has column names
    if (is.null(colnames(data))) {
        cat("\n")
        stop("Please specify the column names for your data.\n\n", call. = FALSE)
    }
    
     # check the outcome specified by the user
    if (nchar(outcome) == 0) {
        cat("\n")
        stop("You haven't specified the outcome set.\n\n", call. = FALSE)
    }
    else if (! outcome %in% colnames(data)) {
        cat("\n")
        stop("The outcome's name is not correct.\n\n", call. = FALSE)
    }
    
     # check if the user specifies something to explain
    if (all(explain == c(""))) {
        cat("\n")
        stop("You have not specified what to explain.\n\n", call. = FALSE)
    }
    
     # check if the user specifies something to explain
    if (!all(explain %in% c(0, 1, "C"))) {
        cat("\n")
        stop("You should explain either 0, 1, or \"C\".\n\n", call. = FALSE)
    }
    
    chexplain <- c(0, 1)[which(0:1 %in% explain)]
    chinclude <- c(0, 1)[which(0:1 %in% include)]
    if (length(chinclude) > 0) {
        if (any(chinclude != chexplain)) {
            chinclude <- chinclude[which(chinclude != chexplain)]
            cat("\n")
            stop(paste("You cannot include ", chinclude, " since you want to explain ", chexplain, ".\n\n", sep=""), call. = FALSE)
        }
    }
    
     # check if explain has both 1 and 0
    if (length(chexplain) == 2) {
        cat("\n")
        stop("Combination to be explained not allowed.\n\n", call. = FALSE)
    }
    
    if (!all(include %in% c("?", "0", "1", "C"))) {
        cat("\n")
        stop("You can only include one or more of the following: \"?\", \"C\", \"0\" and \"1\".\n\n", call. = FALSE)
    }
    
    
    # subset the data with the conditions specified
    if (length(conditions) > 1) {
        if (outcome %in% conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
        }
        if (!all(conditions %in% names(data))) {
            cat("\n")
            stop("The conditions' names are not correct.\n\n", call. = FALSE)
        }
    }
    else if (length(conditions) == 1) {
        if (outcome == conditions) {
            cat("\n")
            stop('Variable "', outcome, '" cannot be both outcome _and_ condition!\n\n', call. = FALSE)
        }
        else {
            cat("\n")
            stop("Cannot find a solution with only one causal condition.\n\n", call. = FALSE)
        }
    }
    
     # if more than 26 conditions (plus one outcome), we cannot use letters
    if (use.letters & ncol(data) > 27) {
        cat("\n")
        stop("Cannot use letters. There are more than 26 conditions.\n\n", call. = FALSE)
    }
    
    # checking for complete data (without missings)
    if (any(is.na(data))) {
        cat("\n")
        stop("Missing data found; this is not (yet) supported.\n\n", call. = FALSE)
    }
}



`verify.dir.exp` <-
function(data, outcome, conditions, dir.exp = c()) {
    
    # checking the directional expectations
    if (is.null(dir.exp)) {
        return(dir.exp)
    }
    else {
        
        # delc is dir.exp.list.complete
        delc <- vector("list", length = length(conditions))
        names(delc) <- conditions
        
        for (i in seq(length(delc))) {
            # sometimes a condition cand have 0, 1 and "-" as values
            # see RagStr$EBA, which is also treated as a factor by default, in R
            
            values <- sort(unique(data[, conditions[i]]))
            if (is.factor(values)) {
                values <- as.character(values)
            }
            
            max.value <- values[length(values)]
            
            if (max.value != "-") {
                delc[[i]] <- rep(0, length(seq(0, as.numeric(max.value))))
                names(delc[[i]]) <- seq(0, as.numeric(max.value))
            }
        }
        
        if (length(dir.exp) != length(conditions)) {
            cat("\n")
            stop("Number of expectations does not match number of conditions.\n\n", call. = FALSE)
        }
        
        # del is dir.exp.list
        del <- strsplit(as.character(dir.exp), split=";")
        
        if (!is.null(names(dir.exp))) {
            if (length(names(dir.exp)) != length(conditions)) {
                cat("\n")
                stop("All directional expectations should have names, or none at all.\n\n", call. = FALSE)
            }
            else if (length(setdiff(names(dir.exp), conditions)) > 0) {
                cat("\n")
                stop("Incorect names of the directional expectations.\n\n", call. = FALSE)
            }
            names(del) <- names(dir.exp)
            del <- del[conditions]
        }
        else {
            names(del) <- conditions
        }
        
        for (i in seq(length(del))) {
            
            values <- strsplit(del[[i]], split = "")
            values <- unlist(lapply(values, function(x) {
                paste(x[x != " "], collapse = "")
            }))
            
            if (all(values %in% c("-", "dc"))) {
                delc[[i]] <- -1
            }
            else {
                values <- setdiff(values, c("-", "dc"))
                if (length(setdiff(values, names(delc[[i]])) > 0)) {
                    cat("\n")
                    stop(paste("Values specified in the directional expectations do not appear in the data, for condition \"", conditions[i], "\".\n\n", sep=""), call. = FALSE)
                }
                else {
                    delc[[i]][as.character(values)] <- 1
                }
            }
        }
        return(delc)
    }
}





`verify.mqca` <-
function(data, outcome = "", conditions = c("")) {
    
    mvoutcome <- grepl("[{]", outcome) # there is a "{" sign in the outcome names
    outcome.value <- rep(-1, length(outcome))
    
    if (any(mvoutcome)) {
        outcome.copy <- outcome
        
        outcome.copy <- strsplit(outcome.copy, split = "")
        outcome.name <- outcome.value <- vector(mode="list", length=length(outcome))
        
        for (i in seq(length(outcome.copy))) {
            if (mvoutcome[i]) {
                outcome.value[[i]] <- as.numeric(outcome.copy[[i]][which(outcome.copy[[i]] == "{") + 1])
                outcome.name[[i]] <- paste(outcome.copy[[i]][seq(1, which(outcome.copy[[i]] == "{") - 1)], collapse="")
            }
            else {
                outcome.value[[i]] <- -1
                outcome.name[[i]] <- outcome[i]
            }
        }
        
        outcome <- unlist(outcome.name)
        
        if (length(intersect(outcome, names(data))) < length(outcome)) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            stop(paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep=""), call. = FALSE)
        }
        
        for (i in seq(length(outcome))) {
            if (mvoutcome[i]) {
                if (!any(unique(data[, outcome.name[[i]]]) == outcome.value[[i]])) {
                    cat("\n")
                    stop(paste("The value {", outcome.value[[i]], "} does not exist in the outcome \"", outcome.name[[i]], "\".\n\n", sep=""), call. = FALSE)
                }
            }
        }
        
        outcome.value <- unlist(outcome.value)
    }
    else {
        
        if (length(intersect(outcome, names(data))) < length(outcome)) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            stop(paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep=""), call. = FALSE)
        }
        
        fuzzy.outcome <- apply(data[, outcome, drop=FALSE], 2, function(x) any(x %% 1 > 0))
        
        # Test if outcomes are in fact multi-valent, even if the user did not specify that 
        if (any(!fuzzy.outcome)) {
            outcome.copy <- outcome[!fuzzy.outcome]
            
            for (i in outcome.copy) {
                valents <- unique(data[, i])
                if (!all(valents %in% c(0, 1))) {
                    cat("\n")
                    stop(paste("Please specify the value of outcome variable \"", i, "\" to explain.\n\n", sep = ""), call. = FALSE)
                }
            }
        }
        
    }
    
    if (all(conditions == c(""))) {
        conditions <- names(data)
    }
    
    
    
    if (length(setdiff(outcome, conditions)) > 0) {
        outcome <- setdiff(outcome, conditions)
        cat("\n")
        stop(paste("Outcome(s) not present in the conditions' names: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep=""), call. = FALSE)
    }
    
    invisible(return(list(mvoutcome = mvoutcome, outcome = outcome, outcome.value = outcome.value, conditions = conditions)))
    
}




`verify.inf.test` <- function(inf.test, data) {
    if (all(inf.test != "")) {
        if (inf.test[1] != "binom") {
            cat("\n")
            stop("For the moment only \"binom\"ial testing for crisp data is allowed.\n\n", call. = FALSE)
        }
        else {
            fuzzy <- apply(data, 2, function(x) any(x %% 1 > 0))
            if (any(fuzzy)) {
                cat("\n")
                stop("The binomial test only works with crisp data.\n\n", call. = FALSE)
            }
        }
        
        if (length(inf.test) > 1) {
            alpha <- as.numeric(inf.test[2])
            if (is.na(alpha) | alpha < 0 | alpha > 1) {
                cat("\n")
                stop("The second value of inf.test should be a number between 0 and 1.\n\n", call. = FALSE)
            }
        }
    }
}

