#Creating a Bar plot based on Number of Screens and Genre
pdata=read.csv("C:/Users/user/Downloads/Movies.csv.xls")
library(ggplot2)
ggplot(pdata,aes(x=Number.of.Screens,y=Genre,fill=Number.of.Screens))+  
  geom_col()
#Creating a bar plot supporting Genre and Number of Movies(count) 
library(ggplot2)
show(ggplot(data=pdata,
            aes(x=Genre)) +   geom_bar(col="black",fill=c("red","orange","yellow","green","blue","purple","pink","maroon","violet","pink","darkgreen","darkblue","maroon","yellow"))+
       xlab("Genre") + 
       ylab("Number of Movies")+
       ggtitle("Movie Count by Genre")+
       theme_classic())
#Interpreting Remake or Franchise of movies using Barplot
pdata=read.csv("C:/Users/user/Downloads/Movies.csv.xls")
table(pdata$Whether.Remake)
barplot(xtabs(~pdata$Whether.Remake),main="WhetherRemake  ",xlab="",col=c("pink","green"),border="red")


#Creation of pie charts for depicting the count of New Actors
pdata=read.csv("C:/Users/user/Downloads/Movies.csv.xls")
v<-table(pdata$New.Actor)
print(v)
pie(xtabs(~pdata$New.Actor), main="Whether New Actors", col=c("darkgreen", "green"))
legend("topright",c("Old","New"),cex=0.8,fill=c("darkgreen","green"))

#Plotting  a 3D pie to analyze the various genres
pdata
head(pdata)
pdata$Genre
?table
table(pdata$Genre)ist
library(plotly)

# Load your dataset
pdata <- read.csv("C:/Users/user/Downloads/Movies.csv.xls")

# Create the 3D pie chart
plot_ly(pdata, labels = ~Genre, values = ~Number.of.Screens, type = 'pie', hole = 0.4, 
        rotation = -90, sort = FALSE, direction = 'clockwise') %>%
  layout(title = "3D Pie Chart", 
         scene = list(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           zaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         ),
         margin = list(l = 0, r = 0, t = 0, b = 0))

# Scatterplot between Revenue, Budget, and Number of Screens
pdata <- read.csv("C:/Users/user/Downloads/Movies.csv.xls")

# Extract relevant columns
scatterplot_data <- pdata[, c("Revenue.INR.", "Budget.INR.", "Number.of.Screens")]

# Set up PNG file for saving the scatterplot
png(file = "scatterplot.png")

# Create scatterplot
plot(x = scatterplot_data$Revenue.INR., 
     y = scatterplot_data$Budget.INR.,
     xlab = "Revenue (INR)",
     ylab = "Budget (INR)",
     xlim = c(325000, 2.10e+09),
     ylim = c(450000, 5651020000),
     main = "Revenue vs. Budget",
     col = 'chocolate'
)

# Create scatterplot matrix
pairs(~Revenue.INR. + Budget.INR. + Number.of.Screens, data = scatterplot_data,
      main = "Scatterplot Matrix",
      col = "blue")

# Save the plot
dev.off()


#Predicting the revenue of the movie based on the movie budget and hence creating a linear line of regression between them
pdata=read.csv("C:/Users/user/Downloads/Movies.csv.xls")
print(pdata)
model <- lm(Revenue.INR. ~ Budget.INR., data = pdata)
summary(model)
y=pdata$Budget.INR.
x=pdata$Revenue.INR.
relation=lm(x~y)
print(relation)
new=data.frame(y=c(35000,53100000))
predict(relation,new)
library(ggplot2)
ggplot(pdata,aes(x=Revenue.INR.,y=Budget.INR.))+
geom_point()+
geom_smooth(method="lm",se=FALSE,color="green")

