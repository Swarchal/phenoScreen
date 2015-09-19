theta <- function(a, b){
    
    # computes the angle bewteen two vectors ('a' and 'b')
    acos(
        sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))
    )}

