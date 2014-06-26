`eqmccLoop` <-
function(data, outcome = "", neg.out = FALSE, conditions = c(""), n.cut = 1,
         incl.cut1 = 1, incl.cut0 = 1, explain = c("1"), include = c(""), row.dom = FALSE,
         min.dis = FALSE, omit = c(), dir.exp = c(), details = FALSE, show.cases = FALSE,
         use.tilde = FALSE, use.letters = FALSE, inf.test = c(""), relation = "suf", ...) {
    
    check.object <- verify.mqca(data, outcome, conditions)
    
    conditions <- check.object$conditions
    
    data <- data[, unique(conditions, check.object$outcome)]
    
    eqmcc.list <- solution.list <- vector(mode = "list", length = length(outcome))
    names(eqmcc.list) <- names(solution.list) <- outcome
    
    for (i in seq(length(outcome))) {
        
        conditions <- names(data)[- which(names(data) == check.object$outcome[i])]
        
        eqmcc.list[[i]] <- eqmcc(data, outcome = outcome[i], conditions = conditions, n.cut=n.cut, incl.cut1 = incl.cut1,
                                 incl.cut0 = incl.cut0, neg.out=neg.out, explain = explain, include = include,
                                 row.dom = row.dom, min.dis = min.dis, relation = relation, show.cases = show.cases, ... = ...)
    }
    
    return(structure(eqmcc.list, class="mqca"))
    
}
