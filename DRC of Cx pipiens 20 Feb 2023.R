#Installed drc and opened the file using RStudio's interface...
library(drc)
help("drm")
PipiensDRCFormat910Feb23Complexmode <- read.csv("C:/Users/12163/Downloads/PipiensDRCFormat910Feb23Complexmode.csv")
dose <- as.Date(PipiensDRCFormat910Feb23Complexmode$Dose, origin = '1970-01-01')
response <- as.numeric(PipiensDRCFormat910Feb23Complexmode$Response)
drcmodel <-drm(response~dose, data=PipiensDRCFormat910Feb23Complexmode, fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(drcmodel, type="all")
summary(drcmodel)
ED(drcmodel, c(10,20,50,75), interval="delta")
library(ggplot2)

#adding the details to the plot
Text.Styling <- theme(
  axis.title = element_text(family = "Times New Roman", size = (14), colour = "black"),
  axis.text = element_text(family = "Times New Roman", colour = "black", size = (10)))

y.title <- expression(paste("Proportion of ", italic("Cx. pipiens"), " collected"))

#Making the plot and making it pretty

help("plot")
plot(drcmodel, type="confidence", broken = TRUE, add = TRUE, xlab = "Date Value", ylab = y.title)
citation("drc")
