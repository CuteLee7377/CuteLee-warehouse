n=50000;
r=0.7;r_e=(1-r*r)^.5;
X=rnorm(n);
Y=X*r+r_e*rnorm(n);
Y=ifelse(X>0,Y,-Y);
plot(X,Y,col="pink")


library(shiny)
library(bslib)
shinyApp(ui = ui, server = server)
ui <- page_sidebar(
  title = "CuteLee says HELLO!",
  sidebar = sidebar("Sidebar"),
  card_image("/Users/lisirui/Desktop/heartbeat.png")
)
runGitHub( "CuteLee-warehouse", "CuteLee7377")
