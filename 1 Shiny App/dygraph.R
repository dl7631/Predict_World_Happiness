library(dygraphs)
library(xts)

str(mdeaths)
str(fdeaths)
names(mdeaths)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

myyear =   as.Date(as.character(1995:2014), "%Y")
myvec1 <- 1:20
myvec2 <- rep(c(10, 20, 10, 20), 5)

myvec1 <- xts(x = myvec1, order.by = myyear)
myvec2 <- xts(x = myvec2, order.by = myyear)

bothvecs <- cbind(myvec1, myvec2)
names(bothvecs) <- c("vec1", "vec2")
dygraph(bothvecs) 
 
 
#   dySeries("vec1", label = "Vec1") %>% 
#   dySeries("vec2", label = "Vec2")
  
?dygraph
