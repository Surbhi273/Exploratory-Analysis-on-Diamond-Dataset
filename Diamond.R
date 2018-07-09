data("diamonds")
View(diamonds)

####Histogram of diamond price######
ggplot(data=diamonds) + geom_histogram(binwidth=500, aes(x=diamonds$price) ) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$") + ylab("Frequency") + theme_classic()

##Mean##
mean(diamonds$price)

###Median###
median(diamonds$price)

####Number of diamonds less than a particular price###
sum(diamonds$price < 500)

sum(diamonds$price < 250)

sum(diamonds$price >= 15000)

###Diamond Price Distribution With binwidth of 1000###
ggplot(data=diamonds) + geom_histogram(binwidth=500, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$ - Binwidth 500") + ylab("Frequency") + theme_minimal() + xlim(0,2500)

####Diamond Price Distribution With binwidth of 100####
ggplot(data=diamonds) + geom_histogram(binwidth=100, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$- Binwidth 100") + ylab("Frequency") + theme_minimal() + xlim(0,2500)

### Diamond Price Distribution With binwidth of 50###
ggplot(data=diamonds) + geom_histogram(binwidth=50, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$ - Binwidth 50") + ylab("Frequency") + theme_minimal() + xlim(0,2500)


####Diamond Price distribution by Cut###
ggplot(data=diamonds) + geom_histogram(binwidth=100, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution by Cut") + xlab("Diamond Price U$") + ylab("Frequency") + theme_minimal() + facet_wrap(~cut)


###Diamond cut with Maximum Price###
subset(diamonds, price == max(price))

###Diamond cut with Minimum Price###
subset(diamonds, price == min(price))

###Lowest Median Price####
a = diamonds[which(diamonds$cut == "Fair"),]
b = diamonds[which(diamonds$cut == "Good"),]
c = diamonds[which(diamonds$cut == "Very Good"),]
d = diamonds[which(diamonds$cut == "Premium"),]
e = diamonds[which(diamonds$cut == "Ideal"),]

####Median value of different cuts###
median(a$price)
median(b$price)
median(c$price)
median(d$price)
median(e$price)

####Get different frequency scales (the y axis) to accomodate for specific patterns######
ggplot(data=diamonds) + geom_histogram(binwidth=100, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution by Cut") + xlab("Diamond Price U$") + ylab("Frequency") + theme_minimal() + facet_wrap(~cut, scales="free_y")

####Plotting price per carat of different cuts####
ggplot(data=diamonds) + geom_histogram(binwidth=50, aes(x=diamonds$price/diamonds$carat)) + ggtitle("Diamond Price per Carat Distribution by Cut") + xlab("Diamond Price per Carat U$") + ylab("Frequency") + theme_minimal() + facet_wrap(~cut)

####Plotting price per carat of different cuts and using Log10#####
ggplot(data=diamonds) + geom_histogram(binwidth=0.01, aes(x=diamonds$price/diamonds$carat)) + ggtitle("Diamond Price per Carat Distribution by Cut") + xlab("Diamond Price per Carat U$ - LOG 10 Scale") + ylab("Frequency") + theme_minimal() + facet_wrap(~cut) + scale_x_log10()

#######Price of diamonds using box plots######
ggplot(diamonds, aes(factor(cut), price, fill=cut)) + geom_boxplot() + ggtitle("Diamond Price according Cut") + xlab("Type of Cut") + ylab("Diamond Price U$") + coord_cartesian(ylim=c(0,7500))

#####Diamond Chart according Clarity####
ggplot(diamonds, aes(factor(clarity), price, fill=clarity)) + geom_boxplot() + ggtitle("Diamond Price according Clarity") + xlab("Clarity") + ylab("Diamond Price U$") + coord_cartesian(ylim=c(0,7500))

###Creating subsets####
d = subset(diamonds, diamonds$color == 'D')
j = subset(diamonds, diamonds$color == 'J')

summary(d)

IQR(d$price)

summary(j)

IQR(j$price)

####Diamond Price per Carat according Color###
ggplot(diamonds, aes(factor(color), (price/carat), fill=color)) + geom_boxplot() + ggtitle("Diamond Price per Carat according Color") + xlab("Color") + ylab("Diamond Price per Carat U$")

####Limit the price range to under and see a smaller picture###
ggplot(diamonds, aes(factor(color), (price/carat), fill=color)) + geom_boxplot() + ggtitle("Diamond Price per Carat according Color") + xlab("Color") + ylab("Diamond Price per Carat U$") + coord_cartesian(ylim=c(0,7500))

####Diamond Frequency by Chart####
ggplot(data=diamonds, aes(x=carat)) + geom_freqpoly() + ggtitle("Diamond Frequency by Carat") + xlab("Carat Size") + ylab("Count")

ggplot(data=diamonds, aes(x=carat)) + geom_freqpoly(binwidth = 0.025) + ggtitle("Diamond Frequency by Carat") + xlab("Carat Size") + ylab("Count") + scale_x_continuous(minor_breaks = seq(0, 5.5, 0.1))

