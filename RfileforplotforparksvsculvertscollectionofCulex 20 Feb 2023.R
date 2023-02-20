#Uploading the data and components needed for graph making

dataforparkvsculvertplot <- read.csv("C:/Users/12163/Downloads/Stuff For Assembling ESA Presentation/dataforparkvsculvertplotv2.csv")
library(ggplot2)
PVC <- dataforparkvsculvertplot
set.seed(1)
help("ggplot")

#Letting R know that these are dates and converting to the proper datavalues
P.Off <- as.Date(PVC$Park.Offset, origin = '1970-01-01')
C.Off <- as.Date(PVC$Culvert.Offset, origin = '1970-01-01')

#Making 2 independent y-layers because grouping hard in R
data.1 <- data.frame(y=PVC$Park.Traps, x=P.Off)
data.2 <- data.frame(y=PVC$Culvert.Aspriration, x=C.Off)

# ggplot(mapping = aes(x, y)) +
#  geom_bar(data = data.1, width = 0.8, stat = 'identity') +
#  geom_bar(data = data.2, width = 0.4, stat = 'identity', fill = 'white') +
#  theme_classic() + scale_y_continuous(expand = c(0, 0))

#adding the details to the plot
Text.Styling <- theme(
  axis.title = element_text(family = "Times New Roman", size = (14), colour = "black"),
  axis.text = element_text(family = "Times New Roman", colour = "black", size = (10)))
  
y.title <- expression(paste("Number of ", italic("Culex"), " collected"))

#Making the plot and making it pretty
ggplot(mapping = aes(x=x, y=y)) + 
  geom_bar(data = data.1, width = 1.25, stat = 'identity', fill = 'blue') +
  geom_bar(data = data.2, width = 1.25, stat = 'identity', fill = 'red') + 
  scale_x_date(date_labels = "%B", breaks = "1 month")+
  theme(axis.text.x = element_text(angle=25, hjust = 0.9)) +
  labs(x = "Month", y = y.title) + Text.Styling


help("as.Date")
