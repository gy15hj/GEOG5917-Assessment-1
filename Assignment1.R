# test for package existance and install
if (!is.element("tidyverse", installed.packages()))
  install.packages("tidyverse", dep = T)
if (!is.element("nycflights13", installed.packages()))
  install.packages("nycflights13", dep = T)

install.packages("ggplot2")

# load into the R session
library(tidyverse)
library(nycflights13)
library(ggplot2)

#read data into R and convert to tibble format 
cl <- as_tibble(read.csv("dh_causal_lookup.csv"))
pl <- as_tibble(read.csv("dh_product_lookup.csv"))
sl <- as_tibble(read.csv("dh_store_lookup.csv"))
tr <- as_tibble(read.csv("dh_transactions.csv"))

#inspect tibbles to ensure data has been read in correctly 
cl
pl
sl
tr 

##############################################################################
################################# Question 1 #################################
##############################################################################

# Return the position (index) of records where the ‘product_description’ column of the
# ‘pl’ tibble equals ‘PRIVATE LABEL THIN SPAGHETTI’ and assign to 'product.index'
product.index <- which(pl$product_description == 'PRIVATE LABEL THIN SPAGHETTI')

# Return the position (index) of records where the ‘commodity’ column 
# of the ‘pl’ tibble equals ‘pasta’ and assign to commod.index'
commod.index <- which(pl$commodity == 'pasta')

# Return the upc column for the indexes related to the relevent products/commodity and 
# assign to upc.prod/upc.commod as a single vector. 
upc.prod <- unlist(pl[product.index,"upc"])
upc.commod <- unlist(pl[commod.index,"upc"])

# Return records from the transaction data where the UPC matches the UPC code of the
# Private Label Thin Spaghetti. Filter out the 'NA' records. Assign results to 'tmp1'.
tmp1 <- filter(tr, !is.na(match(tr$upc, upc.prod)))

# Return records from the transaction data where the UPC matches the UPC code for 'pasta'.
# Filter out the 'NA' records. Assign results to 'tmp2'.
tmp2 <- filter(tr, !is.na(match(tr$upc, upc.commod)))


# Return the count of purchases of the product within the subset of the transactions 
# data, removing duplicates within the houshold column to get the number of unique 
# households that purchase Private Label Thin Spaghetti. Assign result to 'n.product'.
n.product <- length(unique(tmp1$household))

# Return the count of purchases of the commodity within the subset of the transactions 
# data, removing duplicates within the houshold column to get the number of unique 
# households that purchase pasta. Assign to result 'n.commod'.
n.commod <- length(unique(tmp2$household))

# Print outcomes
n.product
n.commod

# Calculate the percentage of households that purchase Private Label Thin Spaghetti out 
# of all households that purchase pasta.
Q1 <- ((n.product/n.commod)*100)

# Print answer 
Q1


df <- data.frame(
  group = c("Private Label Thin Spaghetti", "All other pasta products"),
  value = c(Q1,(1-Q1))
)
head(df)

# Create a basic bar
pie = ggplot(df, aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Household reach of Private Label Thin Spaghetti")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie

##############################################################################
################################# Question 2 #################################
##############################################################################

# Filter 'tmp1' (already filtered to include only the relevent product) by region.
# First return only records where 'geography' equals '1' and assign to 'tmp1.region1' 
# then return only records where 'geography' equals '2' and assign to 'tmp1.region2'.
tmp1.region1 <- filter(tmp1, geography == 1)
tmp1.region2 <- filter(tmp1, geography == 2)

tmp1.region1 
tmp1.region2

# Test that all records from 'tmp1' have been included in either subset.
if (nrow(tmp1) == ((nrow(tmp1.region1)) + (nrow(tmp1.region2)))) {
print("tmp1 region subsets correct")
}

# Return the count of purchases of the product within the subset of the transactions 
# data for each region, removing duplicates within the houshold column to get the number of 
# unique households that purchase Private Label Thin Spaghetti within each region. 

n.product.region1 <-length(unique(tmp1.region1$household))
n.product.region2 <-length(unique(tmp1.region2$household))

# Filter 'tmp2' (already filtered to include only the relevent commodity) by region.
# First return only records where 'geography' equals '1' and assign to 'tmp2.region1' 
# then return only records where 'geography' equals '2' and assign to 'tmp2.region2'.
tmp2.region1 <- filter(tmp2, geography == 1)
tmp2.region2 <- filter(tmp2, geography == 2)

tmp2.region1
tmp2.region2

# Test that all records from 'tmp2' have been included in either subset.
if (nrow(tmp2) == ((nrow(tmp2.region1)) + (nrow(tmp2.region2)))) {
  print("tmp2 region subsets correct")
}

# Return the count of purchases of the commodity within the subset of the transactions 
# data for each region, removing duplicates within the houshold column to get the number of 
# unique households that purchase pasta within each region. 
n.commod.region1 <-length(unique(tmp2.region1$household))
n.commod.region2 <-length(unique(tmp2.region2$household))

n.commod.region1
n.commod.region2

# Calculate the percentage of households that purchase Private Label Thin Spaghetti out 
# of all households that purchase pasta in each of the two regions. 
Q2.region1 <- ((n.product.region1/n.commod.region1)*100)
Q2.region2 <- ((n.product.region2/n.commod.region2)*100)

#Print answers 
Q2.region1 
Q2.region2


# Create dataframes containing the figures necessary to plot a pie chart with headings suitable 
# for plot labels 
Q2.region1.df <- data.frame(
  group = c("Private Label Thin Spaghetti", "All other pasta products"),
  value = c(Q2.region1,(100-Q2.region1))
)

Q2.region2.df <- data.frame(
  group = c("Private Label Thin Spaghetti", "All other pasta products"),
  value = c(Q2.region2,(100-Q2.region2))
)


plot_pie <- function(dataframe) 
  # Plotting pie charts to show household penetration of product
  #
  # Plots household penetration as bar chart, before converting it to
  #
  # Input - either of the two dataframes created for each region
  # 
  # Output - pie chart showing household penetration of PLT Spaghetti in either region 
  {
  pie = ggplot(dataframe, aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1)
  pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value), "%")), 
                                                    position = position_stack(vjust = 0.5))
  pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A"))
  pie = pie + labs(x = NULL, y = NULL, fill = NULL, 
                   title = "Household Penetration of Private Label Thin Spaghetti in Region 1")
  pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                      axis.text = element_blank(),
                                      axis.ticks = element_blank(),
                                      plot.title = element_text(hjust = 0.0, color = "#666666"))
  return(pie)                                   
  }

# Call function for each region.
plot_pie(Q2.region1.df)
plot_pie(Q2.region2.df)






# Create a basic bar
pie = ggplot(Q2.region2.df, aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Household Penetration of Private Label Thin Spaghetti in Region 2")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.0, color = "#666666"))
pie


#dont add up riddle me that 
n.commod.region1 + n.commod.region2

n.commod

##############################################################################
################################# Question 3 #################################
##############################################################################

# Define commodity name allowing it to be easily altered by user. 
commodity.name <- "pasta"

# Return the position (index) of records where the ‘commodity’ column of the
# ‘pl’ tibble equals the predefined commodity name and assign to 'commod.index'
commod.index <- which(pl$commodity == commodity.name)

# Return the upc column for the indexes related to the relevent commodity (assign to upc.commod).
upc.commod <- unlist(pl[commod.index,"upc"])

# Return records from the transaction data (tr) where the UPC matches the UPC code of the
# commodity. Filter out the 'NA' records. Assign results to 'commodity.tr'.
commodity.tr <- filter(tr, !is.na(match(tr$upc, upc.commod)))

# Group all purchses of commodity in question by household and then filter to only include the 
# minimum day row for that household (i.e first purchase of commodity).
# Filter again to only include those purchases that used coupons.
first.purchase <- commodity.tr %>% 
  group_by(household) %>% 
  filter(day == min(day)) %>% 
  filter(time_of_transaction == min(time_of_transaction)) %>% 
  filter (coupon == 1)

# Assign all households whose first purchase used a coupon to list.
coupon.households <- first.purchase$household

# Filter commodity.tr (tibble containing all purchases of commodity) to only include 
# transactions by households that match 'coupon.households'
households <- filter(commodity.tr, !is.na(match(commodity.tr$household, coupon.households)))

# Group purchases within 'households' by household and filter to include all transactions 
# barr the first i.e any additional purchases.
add.purch <- households %>%
  group_by(household) %>% 
  filter(day != min(day))

# Identifiy how many unique households made their first purchse using a coupon.- because some transactions happened at the same time both with a coupon
Q3.couponpurchase <- length(unique(first.purchase$household))

#Identifty house many unique households made an additional purchase. 
Q3.repeatpurchase <- length(unique(add.purch$household))

#Calculate percentage.  
Q3 = (Q3.repeatpurchase/Q3.couponpurchase)*100

Q3

coupon_impact <- function(commodity.name) {
  # Calculate the impact of coupon sales 
  #
  # @description The function identifies initial sales of a commodity made using a coupon and 
  # then identifies repeat purchases of the same commodity, by the same households. 
  # 
  # @param commodity.name  
  # @param 
  # @param 
  commod.index <- which(pl$commodity == commodity.name)
  upc.commod <- unlist(pl[commod.index,"upc"])
  commodity.tr <- filter(tr, !is.na(match(tr$upc, upc.commod)))
  
  first.purchase <- commodity.tr %>% 
    group_by(household) %>% 
    filter(day == min(day)) %>% 
    filter(time_of_transaction == min(time_of_transaction)) %>% 
    filter (coupon == 1)
  
  coupon.households <- first.purchase$household
  households <- filter(commodity.tr, !is.na(match(commodity.tr$household, coupon.households)))
  
  additional.purchase <- households %>%
    group_by(household) %>% 
    filter(day != min(day))
 
   Q3.couponpurchase <- length(unique(first.purchase$household))
  Q3.repeatpurchase <- length(unique(additional.purchase$household))
  Q3 = (Q3.repeatpurchase/Q3.couponpurchase)*100
  return(Q3)
  }

coupon_impact("pasta")
coupon_impact("pasta sauce")
coupon_impact("syrups")
coupon_impact("pancake mixes")


mini.tr <- head(tr, 10000)


# Calculate the impact of coupons on commodity sales 
#
# The function identifies households who purchased a commodity using a coupon and then 
# identifies whether repeat purchases of the same commodity were made by the same households. 
# 
# Input
# commod - The name of the commodity being studied. 
#
# Output
# Q3 - list containing the name of the commodity being studied, the number of households that 
# made their first purchase of the commodity with a coupon (coupon.purchase) and either did 
# repurchase (repeat.purchase) or did not repurchase (no.repeat.purchase). The proportion of 
# coupons that resulted in repeat sales is also calculated and included. 
Q3.function <- function(commod) {
  tr %>% left_join(pl) %>% filter(coupon == 1) %>% filter(commodity == commod) -> r1
  h <- unique(r1$household)
  tr %>% left_join(pl) %>% filter(commodity == commod) -> r2 
  coupon.purchase <- vector()
  repeat.purchase <- vector()
  no.repeat.purchase <- vector()
  for (i in h){
    r2 %>% filter(household == i) %>% arrange(day, -coupon) %>% select(coupon) %>% unlist -> tmp
    if (tmp[1] == 1 & length(tmp) >1) {
      repeat.purchase <- c(repeat.purchase, i)}
    if (tmp[1] == 1 & length(tmp) == 1) {
      no.repeat.purchase <- c(no.repeat.purchase, i)}
    if (tmp[1] == 1) {
      coupon.purchase <- c(coupon.purchase, i)}
    }
  Q3 <- list("Commodity" = commod, 
             "First purchase with coupon" = length(coupon.purchase), 
             "Repurchased" = length(repeat.purchase), 
             "Not Repurchased" = length(no.repeat.purchase),
             "Impact" = ((length(repeat.purchase))/(length(coupon.purchase))*100))
  return(Q3)
}

Q3.function("pasta")
Q3.function("pasta sauce")
Q3.function("syrups")
Q3.function("pancake mixes")


tr %>% left_join(pl) %>% filter(coupon == 1) %>% filter(commodity == "pasta") -> r1
h <- unique(r1$household)
length(h)
