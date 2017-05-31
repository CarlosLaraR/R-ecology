f_best_modularity<-function(mod.list,...) {
results=sapply(mod.list, function(x) x@likelihood)
print(max(results))
results.max<- which (results== max(results))
results.max<-results.max[1]
mod.best<-mod.list[[results.max]]      
return(mod.best)
 }