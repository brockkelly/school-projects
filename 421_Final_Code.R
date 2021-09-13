library(readr)
library(tidyverse)
library(dplyr)

# your path here
setwd("~/Desktop/econ 421/final project")

# importing our data 

GDPdata <- read_csv("GDPdata.csv")
employmentdata <- read_csv("hoursdata.csv")

# view(GDPdata)
# view(employmentdata)

# making industries factors

GDPdata$industry <- as.factor(GDPdata$`North American Industry Classification System (NAICS)`)
employmentdata$industry <- as.factor(employmentdata$`North American Industry Classification System (NAICS)`)

#levels(GDPdata$industry)
#levels(employmentdata$industry)

# formatting date variables
library(zoo)
employmentdata$date <- as.Date(as.yearmon(employmentdata$REF_DATE))
GDPdata$date <- as.Date(as.yearmon(GDPdata$REF_DATE))

# renaming columns

GDPdata <- GDPdata[c("date", "industry", "VALUE")]
employmentdata <- employmentdata[c("date", "industry", "VALUE")]

# since the NAICS industries are collected differently in the GDP and labour hours data,
# I have collected the rows under the correct industry labels here

widerdata <- pivot_wider(GDPdata, date, names_from = industry, values_from = "VALUE")

widerdata$`Wholesale and retail trade [41, 44-45]` <- rowSums(cbind(widerdata$`Wholesale trade [41]`, widerdata$`Retail trade [44-45]`))

widerdata$`Total employed, all industries` <- widerdata$`All industries [T001]`

widerdata$`Services-producing sector` <- widerdata$`Service-producing industries [T003]`

widerdata$`Information, culture and recreation [51, 71]` <- rowSums(cbind(widerdata$`Information and cultural industries [51]`, 
                                                                widerdata$`Arts, entertainment and recreation [71]`))

widerdata$`Goods-producing sector` <- widerdata$`Goods-producing industries [T002]`

widerdata$`Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]` <- rowSums(cbind(widerdata$`Forestry and logging [113]`,
                                                                                               widerdata$`Fishing, hunting and trapping [114]`,
                                                                                               widerdata$`Mining, quarrying, and oil and gas extraction [21]`))

widerdata$`Finance, insurance, real estate, rental and leasing [52-53]` <- rowSums(cbind(widerdata$`Finance and insurance [52]`,
                                                                               widerdata$`Real estate and rental and leasing [53]`))

widerdata$`Business, building and other support services [55-56]` <- rowSums(cbind(widerdata$`Management of companies and enterprises [55]`,
                                                                         widerdata$`Administrative and support, waste management and remediation services [56]`))

widerdata$`Agriculture [111-112, 1100, 1151-1152]` <- rowSums(cbind(widerdata$`Crop and animal production [11A]`,
                                                          widerdata$`Support activities for agriculture and forestry [115]`))

widerdata$`Total actual hours worked, all industries` <- widerdata$`All industries [T001]`

# filtering data for only NAICS columns from employment data set

widerdata <- cbind(widerdata$date, widerdata[, levels(employmentdata$industry)])

longerdata <- pivot_longer(widerdata, cols = colnames(widerdata[, - 1]), names_to = "industry", values_to = "value")

names(longerdata)[names(longerdata) == "widerdata$date"] <- "date"

# combining GDP and employment hours data
total <- merge(longerdata, employmentdata, by.longerdata = date)

names(total)[names(total) == "value"] <- "GDP"
names(total)[names(total) == "VALUE"] <- "employment"

# generating our productivity measure
total <- mutate(total, productivity = GDP/employment) 

# refactorzing industry
total$industry <- as.factor(total$industry)

# Adding recession shading into the graphs

library(quantmod)

# recession are defined by the following metric from FRED - CANRECDM

recessions.df = read.table(textConnection(
  "Peak, Trough
2007-08-01, 2009-08-01
2011-10-01, 2012-03-01
2014-08-01, 2016-07-01
2017-08-01, 2019-03-01
2020-03-01, 2021-01-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

# Visualizing our data - You can pretty much ignore all of this.

# Looking at specific industries

levels(total$industry)

# this creates a dataframe subset for looking at specific industries

subset <- filter(total, industry %in% c("Services-producing sector", "Goods-producing sector"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Total actual hours worked, all industries

subset <- filter(total, industry == c("Total actual hours worked, all industries"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Accommodation and Food Services

subset <- filter(total, industry == c("Accommodation and food services [72]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Agriculture

subset <- filter(total, industry == c("Agriculture [111-112, 1100, 1151-1152]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# Construction [23]

subset <- filter(total, industry == c("Construction [23]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Educational services [61]

subset <- filter(total, industry == c("Educational services [61]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Finance, insurance, real estate, rental and leasing [52-53]

levels(total$industry)
subset <- filter(total, industry == c("Professional, scientific and technical services [54]"))

prodplot <- ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

empplot <- ggplot(subset) + geom_line(aes(x=date, y=employment), colour = "firebrick3") + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
  xlab("Date") + 
  ylab("Employment Hours (Thousands)") + 
  ggtitle("Oil and Mining Industry Employment") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

gdpplot <- ggplot(subset) + geom_line(aes(x=date, y=GDP), colour = "dodgerblue3") + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
  xlab("Date") + 
  ylab("Output (Millions)") + 
  ggtitle("Oil and Mining Industry Output") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

library(ggpubr)
library(cowplot)
library(gridExtra)

ggarrange(empplot, gdpplot, ncol = 2, nrow = 1)

# Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]

subset <- filter(total, industry == c("Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Adding oil price data - maybe later can scale this for two y axes

library(tidyquant)
df<-tq_get("MCOILWTICO",                         # get selected symbols
            get="economic.data",             # use FRED
            from="2005-12-31")

subset <- filter(total, industry == c("Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_line(df, mapping = aes(x = date, y = price))


library(tidyverse)
library(lubridate)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# two y-axes

outputColor <- "blue"
oilColor <- "red"

coeff <- 1000
ggplot(subset, aes(x = date)) +
  
  geom_line(aes(y = employment / coeff), color = outputColor, size = 0.4) +
  geom_line(df, mapping = aes(y = price), color = oilColor, size  = 0.4) + 
  ggtitle("Oil Price vs. Employment") +
  xlab("Date") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Oil Price",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Employment")) + 
  
  theme_ipsum() + 
  
  theme( axis.title.y = element_text(colour = oilColor, size=10), 
         axis.title.y.right = element_text(colour = outputColor, size=10),
         plot.title = element_text(face = "bold", hjust = 0.5))


#  Health Care

subset <- filter(total, industry == c("Health care and social assistance [62]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Information, culture and recreation [51, 71]

subset <- filter(total, industry == c("Information, culture and recreation [51, 71]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Manufacturing [31-33]

subset <- filter(total, industry == c("Manufacturing [31-33]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Manufacturing [31-33]

subset <- filter(total, industry == c("Manufacturing [31-33]"))

ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during Recessions") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Generating some growth rates

total$emp_growth <- with(total, ave(employment, industry, 
                            FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))*100
total$gdp_growth <- with(total, ave(GDP, industry, 
                                    FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))*100
total$prod_growth <- with(total, ave(productivity, industry, 
                                    FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))*100

subset <- filter(total, industry == c("Manufacturing [31-33]"))

ggplot(subset, aes(x = date, y = prod_growth, colour = industry)) + 
  geom_line() +
  xlab("Date") +
  ylab("% Change in Productivity") +
  ggtitle("Productivity") +
  theme(plot.title = element_text(face = "bold"))

# calculating mean growth rates for each industry during COVID-19

# Recent Data

subset <- filter(non_total_data, date > as.Date("2020-01-01"))
ggplot(subset) + geom_line(aes(x=date, y=productivity, colour = industry)) + 
  xlab("Date") + 
  ylab("Labour Productivity") + 
  ggtitle("Canadian Labour Productivity during COVID-19") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

by_industry <- group_by(subset, industry)

prod_growth <- as.data.frame(summarize(by_industry, change = (last(productivity) - first(productivity)) *100 / first(productivity) ) )

emp_growth <- as.data.frame(summarize(by_industry, change = (last(employment) - first(employment)) * 100/ first(employment) ) )

gdp_growth <- as.data.frame(summarize(by_industry, change = (last(GDP) - first(GDP)) * 100 / first(GDP) ) )

mean_growth_data <- cbind(prod_growth, emp_growth$change, gdp_growth$change)

colnames(mean_growth_data) <- c("industry", "Productivity", "Employment", "Output")

mean_growth_data <- pivot_longer(mean_growth_data, cols = colnames(mean_growth_data[, - 1]), names_to = "measure", values_to = "value")

ggplot(mean_growth_data, aes(x = measure, y = value, colour = industry)) +
  geom_col(position = "dodge", fill = "white", alpha = 0.4) +
  xlab("Measure") +
  ylab("% Change since February 2020") +
  labs(colour = "NAICS Sector") +
  ggtitle("Employment, GDP, and Productivity During COVID-19") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# Employment and GDP by sector -  for looking at trends in gdp and employment hours

# data frame that doesn't include aggregates
non_total_data <- filter(total, !industry %in% c("Services-producing sector", 
                                                                    "Goods-producing sector", 
                                                                    "Total actual hours worked, all industries"))


dat <- pivot_wider(total, date, names_from = industry, values_from = c("GDP", "employment"))

# developing some growth rates to look at

GDPper <- dat[, 1:20] %>%
  mutate(across(c(2:20),.fns = ~./dat[, 17]))
 
EMPper <- cbind(date = dat$date, dat[, 21:39]) %>% 
  mutate(across(c(2:20),.fns = ~./dat[, 36]))

GDP_perc_data <- as.data.frame(pivot_longer(GDPper, cols = colnames(GDPper[, - 1]), names_to = "industry"))

EMP_perc_data <- as.data.frame(pivot_longer(EMPper, cols = colnames(EMPper[, - 1]), names_to = "industry"))

GDP_perc_data <- cbind(GDP_perc_data$date, GDP_perc_data$industry, GDP_perc_data$value)

EMP_perc_data <- cbind(EMP_perc_data$date, EMP_perc_data$industry, EMP_perc_data$value)

colnames(GDP_perc_data) <-  c("date", "industry", "GDP")

colnames(EMP_perc_data) <-  c("date", "industry", "employment")

ggplot(EMP_perc_data, aes(x = date, y = employment, colour = industry)) + 
  geom_line() +
  xlab("Date") +
  ylab("Employment") +
  ggtitle("Employment by Sector") +
  theme(plot.title = element_text(face = "bold"))

ggplot(GDP_perc_data, aes(x = date, y = GDP, colour = industry)) + 
  geom_line() +
  xlab("Date") +
  ylab("%GDP") +
  ggtitle("GDP by Sector") +
  theme(plot.title = element_text(face = "bold"))

# looking at relative goods-services breakdown

subset <- filter(GDP_perc_data, industry %in% c("GDP_Services-producing sector", "GDP_Goods-producing sector"))

ggplot(subset, aes(x = date, y = GDP, colour = industry)) + 
  geom_line() +
  xlab("Date") +
  ylab("GDP") +
  ggtitle("GDP by Sector") +
  theme(plot.title = element_text(face = "bold"))

# filtering for smaller industries

small_employment <- filter(EMP_perc_data, employment < 0.2)

small_GDP <- filter(GDP_perc_data, GDP < 0.25 & GDP > 0.05)

ggplot(small_employment) + geom_line(aes(x=date, y=employment, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("% Employed") + 
  ggtitle("Sectoral Employment Trends") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggplot(small_GDP) + geom_line(aes(x=date, y=GDP, colour = industry)) + 
  theme_bw() + 
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2) +
  xlab("Date") + 
  ylab("% GDP") + 
  ggtitle("Sectoral GDP Trends") +
  labs(color = "NAICS Industry") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
