##This function calculates percent rank of a value v in another numeric list l##
##Return in percent##
##@Harry Peng Zhao

rank_in_percent <- function(v, l ) { #v represents a number, l is a list
    if (is.na(v) | v ==0) {
      return (NA)
    } else{
      return (length(l[l<= v]) / length(l) *100)  
    }
  
}