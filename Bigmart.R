
## load packages
library(data.table) 
library(dplyr)      
library(ggplot2)     
library(caret)      
library(corrplot)  
library(xgboost)   
library(cowplot)

## Read data sets 

train = fread("Train_UWu5bXk.csv")
test = fread("Test_u94Q5KV.csv")
submission = fread("SampleSubmission_TmnO39y.csv")

#data sctructure 

## column names
names(train)

## column names
names(test)

## structure of train data
str(train)

## structure of the data
str(test)

##combinig the two data sets 

test[,Item_Outlet_Sales := NA] ## add Item_Outlet_Sales to test data

combined = rbind(train, test) # merging train and test datasets

dim(combined)

## UNIVARIATE DATA ANALYSIS
#since our target variable is continuous,I plotted a histogram to visualize will be able to show us how skewed our data is.

##Univariate plots 
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "blue") +
  xlab("Item_Outlet_Sales")

## checking the distribution of indepedent variables

plot1 = ggplot(combined) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "red")

plot2 = ggplot(combined) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "red")

plot3 = ggplot(combined) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "red")

plot_grid(plot1, plot2, plot3, nrow = 1)

##variable transformation

#Item Fat Content
ggplot(combined %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

combined$Item_Fat_Content[combined$Item_Fat_Content == "LF"] = "Low Fat"
combined$Item_Fat_Content[combined$Item_Fat_Content == "low fat"] = "Low Fat"
combined$Item_Fat_Content[combined$Item_Fat_Content == "reg"] = "Regular"

ggplot(combined %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

# plot for Item_Type
plot4 = ggplot(combined %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

plot4

# plot for Outlet_Identifier

plot5 = ggplot(combined %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot5



# plot for Outlet_Size
plot6 = ggplot(combined %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot6

second_row = plot_grid(plot5, plot6, nrow = 1)
second_row
plot_grid(plot4, second_row, ncol = 1)



# plot for Outlet_Establishment_Year
plot7 = ggplot(combined %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

plot7

# plot for Outlet_Type

# plot for Outlet_Type
plot8 = ggplot(combined %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

plot8

# ploting both plots together
plot_grid(plot7, plot8, ncol = 2)

##BIVARIATE DATA ANALYSIS
# Item_Weight vs Item_Outlet_Sales
train = combined[1:nrow(train)]

# Item_Weight vs Item_Outlet_Sales
plot9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "gold4", alpha = 0.2) +
  theme(axis.title = element_text(size = 8.5))

plot9

# Item_Visibility vs Item_Outlet_Sales
plot10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "gold4", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

plot10

# Item_MRP vs Item_Outlet_Sales
plot11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "gold4", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

plot11

second_row_2 = plot_grid(plot10, plot11, ncol = 2)

plot_grid(plot9, second_row_2, nrow = 2)


# Item_Type vs Item_Outlet_Sales
plot12 = ggplot(train) + geom_boxplot(aes(Item_Type, Item_Outlet_Sales), fill = "maroon4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 9.5))
plot12


# Item_Fat_Content vs Item_Outlet_Sales
plot13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "maroon4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 9.5),
        axis.title = element_text(size = 9.5))
plot13

######################### cODE CONTIMUED IN THE rmd file ######################################


