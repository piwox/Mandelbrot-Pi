mandel_r <- function(epsilon)
{
   n=0
   z=0
   z_v=numeric()
   x_v=numeric()
   while(z<2)
   {
      n=n+1
      x_v[2*n-1]=z
      x_v[2*n]=z
      z_v[2*n-1]=z
      
      z=z^2+0.25+epsilon
      z_v[2*n]=z
   }
return(list(z_v[1:(length(z_v)-2)],x_v[1:(length(z_v)-2)],n))
}
# library(plotly)
# plot_escape <- function(eps)
# {
# d<-mandel_r(eps)
# #length(d[[2]])
# #plot(y=d[[1]],x=d[[2]][1:3140])
# 
# range = d[[2]]
# #range = sort(c(range,range))
# #range
# par <- range^2 +1/4 + eps
# #length(par)
# #tail(par)
# p<-plot_ly(data=data.frame(d), y=~par,x=~d[[2]],type='scatter',mode='lines')  %>% 
#    add_trace(y=~range,line=list(color="009900")) %>%
#    add_trace(y=~d[[1]],line=list(color = "#FF0000"))
# print(p)
# print(d[[3]])
# }
# plot_escape(7e-3)
# plot_atan <- function(eps)
# {
# range = seq(-5,5,by=0.01)
# v_2 = atan(1.5/sqrt(eps))
# v_0 = atan(-0.5/sqrt(eps))
# v = atan((range-0.5)/sqrt(eps))
# print(plot_ly(d=data.frame(v),x=~range,y=~v,type="scatter",mode="line") %>% add_markers(x=0,y=v_0,marker = list(color = "#FF0000"))%>% add_markers(x=2,y=v_2,marker = list(color = "#FF0000"))%>% layout(showlegend = FALSE))
# }
# plot_atan(0.00001)

