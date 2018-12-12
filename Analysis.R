library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(tidyr)

seattle.data <- read.csv("Data USA - Bar Chart of Wage by Gender in Common Jobs in Seattle, Wa.csv", stringsAsFactors = F)
sf.data <- read.csv("Data USA - Bar Chart of Wage by Gender in Common Jobs in San Francisco, Ca.csv", stringsAsFactors = F)

seattle.data <- na.omit(seattle.data)
sf.data <- na.omit(sf.data)

total.data <- left_join(seattle.data, sf.data, by = c("soc_name", "year", "sex_name"))
total.data$soc.x <- as.numeric(total.data$soc.x)
total.data$soc.y <- as.numeric(total.data$soc.y)
total.data <- na.omit(total.data)
drop.columns <- c("sex.x" , "sex.y", "soc.x", "soc.y", "geo.x", 
                  "geo.y", "geo_name.x", "geo_name.y", 
                  "avg_wage_ft_moe.x", "avg_wage_ft_moe.y", 
                  "num_ppl_moe.x" ,  "num_ppl_moe.y")
total.data <- total.data[, !(names(total.data) %in% drop.columns)]
colnames(total.data) <- c("year", "soc_name", "sex_name", "num_ppl.wa",
                          "avg_wage_ft.wa", "num_ppl.ca",
                          "avg_wage_ft.ca")
wa.c <- c("year", "soc_name", "sex_name", "avg_wage_ft.wa")
wa.data <- total.data[wa.c]
wa.data.a <- wa.data %>% 
             group_by(year, soc_name) %>% 
             spread(key = sex_name, value = avg_wage_ft.wa)

ca.c <- c("year", "soc_name", "sex_name", "avg_wage_ft.ca")
ca.data <-  total.data[ca.c]
ca.data.a <- ca.data %>% 
  group_by(year, soc_name) %>% 
  spread(key = sex_name, value = avg_wage_ft.ca)

ca.data.a <- na.omit(ca.data.a)
wa.data.a <- na.omit(wa.data.a)

total.data <- merge(ca.data.a, wa.data.a, by = c("year", "soc_name"))
colnames(total.data) <- c("year", "soc_name", "Female.CA", "Male.CA", "Female.WA", "Male.WA")

# converting all computation variables to int for calculation efficiency
total.data$Female.CA <- as.numeric(total.data$Female.CA)
total.data$Male.CA <- as.numeric(total.data$Male.CA)
total.data$Female.WA <- as.numeric(total.data$Female.WA)
total.data$Male.WA <- as.numeric(total.data$Male.WA)

# tech field selection
tech.data.1 <- filter(total.data,total.data$soc_name ==  "Computer & information research scientists")
tech.data.2 <- filter(total.data, total.data$soc_name ==  "Computer systems analysts")    
tech.data.3 <- filter(total.data, total.data$soc_name == "Information security analysts") 
tech.data.4 <- filter(total.data, total.data$soc_name ==  "Web developers")                           
tech.data.5 <- filter(total.data, total.data$soc_name ==  "Database administrators")                                                                                    
tech.data.6 <- filter(total.data, total.data$soc_name == "Network & computer systems administrators")  
tech.data.7 <- filter(total.data, total.data$soc_name ==  "Computer network architects")                                                                      
tech.data.8 <- filter(total.data, total.data$soc_name ==  "Computer support specialists")                                                                                                             
tech.data.9 <- filter(total.data, total.data$soc_name == "Other Computer Occupations") 
tech.data.10 <- filter(total.data,total.data$soc_name == "Computer programmers")

tech.data <- rbind(tech.data.1, tech.data.10, tech.data.2, tech.data.3, tech.data.4, tech.data.5, tech.data.6,
                   tech.data.7, tech.data.8, tech.data.9)
## EXPLORATORY DATA ANALYSIS


# analyzing the average wage difference between CA & WA
# calculating the difference between men's salary and women's salary
### CAUTION FOR NEGATIVE VALUES! 
tech.data$diff.CA <- tech.data$Male.CA - tech.data$Female.CA
tech.data$diff.WA <- tech.data$Male.WA - tech.data$Female.WA


# filter for 2014
tech.data.2014 <- filter(tech.data, year == 2014)

# filter for 2015
tech.data.2015 <- filter(tech.data, year == 2015)

# filter for 2016
tech.data.2016 <- filter(tech.data, year == 2016)

# average salary of men in Seattle for tech occupations - 2016
avg.wa.m <- mean(tech.data.2016$Male.WA) # $95747.11

# average salary of women in Seattle for tech occupations - 2016
avg.wa.w <- mean(tech.data.2016$Female.WA) # $84489.18

# average salary of men in San Francisco for tech occupations - 2016
avg.ca.m <- mean(tech.data.2016$Male.CA) # $97870.83

# average salary of women in San Francisco for tech occupations - 2016
avg.ca.w <- mean(tech.data.2016$Female.CA) # $84615.45

# sf-tech data
sf.tech <- tech.data[c("year", "soc_name", "Female.CA", "Male.CA", "diff.CA")]

# seattle-tech data
sea.tech <- tech.data[c("year", "soc_name", "Female.WA", "Male.WA", "diff.WA")]

years <- c('2014', '2015', '2016')
y.diff.ca <- c(mean(tech.data.2014$diff.CA), mean(tech.data.2015$diff.CA), mean(tech.data.2016$diff.CA))
y.diff.wa <- c(mean(tech.data.2014$diff.WA), mean(tech.data.2015$diff.WA), mean(tech.data.2016$diff.WA))

data.diff <- data.frame(years, y.diff.ca, y.diff.wa)

# visualizing the above calculations by year 
g.1 <- data.diff %>% 
      plot_ly() %>%
      add_trace(x = ~years, y = ~y.diff.ca, type = 'scatter', mode = "lines+markers+text", 
            text = ~y.diff.ca, textposition = 'middle right',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)),
      name = "San Francisco") %>%
  add_trace(x = ~years, y = ~y.diff.wa, type = 'scatter', mode = "lines+markers+text", 
            text = ~y.diff.wa, textposition = 'middle right',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)),
            name = "Seattle") %>%
  layout(title = "Trend of Difference in Mean Wages between Men & Women in Technology Roles (2014-2016)",
         xaxis = list(title = "Years"),
         yaxis = list(title = "Difference in Mean Wages ($)"))
g.1

# analyzing the trend of wages by occupation - mean 
vars.needed <- c("soc_name", "Female.CA", "Male.CA", 
                 "Female.WA", "Male.WA")
tech.data.o1 <- tech.data[vars.needed]
xyz <- list((tech.data.o1$soc_name))
tech.data.o <- aggregate(x = tech.data.o1,by= xyz, FUN = mean)
tech.data.o$diff.ca <- tech.data.o$Male.CA - tech.data.o$Female.CA
tech.data.o$diff.wa <- tech.data.o$Male.WA - tech.data.o$Female.WA

# visualizng the obove trend 
g.2 <- plot_ly(tech.data.o, y = ~Group.1, x = ~diff.ca,
               type = 'bar', 
               text = ~Group.1, textposition = 'auto',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)),
               name = "San Francisco")  %>%
      add_trace(x = ~diff.wa,
                type = 'bar', 
                text = ~Group.1, textposition = 'auto',
                marker = list(color = 'rgb(58,200,25)',
                              line = list(color = 'rgb(8,48,107)', width = 1.5)),
                name = "Seattle") %>% 
    layout(title = "Occupational Trend of Difference in Wages between Men & Women in Technology Roles",
           barmode = 'group',
           yaxis = list(title = "Occupations", showticklabels = FALSE),
           xaxis = list(title = "Difference in Mean Wages ($)"))

g.2

# Paired t-test
# Null Hypothesis - San Francisco and Seattle have same wage gap (mean value) - tech roles
tech.data.t <- tech.data.o %>% 
              gather(`diff.wa`, `diff.ca`, key = "City", value = "Mean Wage Difference")
tech.data.t <- tech.data.t[c("City", "soc_name", "Mean Wage Difference")]
t.test(tech.data.t$`Mean Wage Difference` ~ tech.data.t$City, data = tech.data.t) 

##############
# medical jobs
med.data.1 <- filter(total.data,total.data$soc_name == "Health practitioner support technologists & technicians")
med.data.2 <- filter(total.data,total.data$soc_name == "Health diagnosing & treating practitioners, all other")
med.data.3 <- filter(total.data,total.data$soc_name == "Emergency medical technicians & paramedics")
med.data.4 <- filter(total.data,total.data$soc_name == "Dentists")
med.data.5 <- filter(total.data,total.data$soc_name == "Medical assistants")
med.data.6 <- filter(total.data,total.data$soc_name =="Medical, dental, & ophthalmic laboratory technicians" )
med.data.7<- filter(total.data,total.data$soc_name == "Nurse anesthetists")
med.data.8 <- filter(total.data,total.data$soc_name == "Registered nurses")
med.data.9 <- filter(total.data,total.data$soc_name == "Clinical laboratory technologists & technicians")
med.data.10 <- filter(total.data,total.data$soc_name == "Physicians & surgeons")
med.data <- rbind(med.data.1, med.data.2, med.data.3, med.data.4, med.data.5,
                  med.data.6, med.data.7, med.data.8, med.data.9, med.data.10)

## EXPLORATORY DATA ANALYSIS

# analyzing the average wage difference between CA & WA
# calculating the difference between men's salary and women's salary
### CAUTION FOR NEGATIVE VALUES! 
med.data$diff.CA <- med.data$Male.CA - med.data$Female.CA
med.data$diff.WA <- med.data$Male.WA - med.data$Female.WA

# filter for 2014
med.data.2014 <- filter(med.data, year == 2014)

# filter for 2015
med.data.2015 <- filter(med.data, year == 2015)

# filter for 2016
med.data.2016 <- filter(med.data, year == 2016)

# average salary of men in Seattle for med occupations - 2016
avg.wa.m.m <- mean(med.data.2016$Male.WA) # $100758.6

# average salary of women in Seattle for med occupations - 2016
avg.wa.w.m <- mean(med.data.2016$Female.WA) # $74148.8

# average salary of men in San Francisco for med occupations - 2016
avg.ca.m.m <- mean(med.data.2016$Male.CA) # $101135.1

# average salary of women in San Francisco for med occupations - 2016
avg.ca.w.m <- mean(med.data.2016$Female.CA) # $82424.18

y.diff.ca.m <- c(mean(med.data.2014$diff.CA), mean(med.data.2015$diff.CA), mean(med.data.2016$diff.CA))
y.diff.wa.m <- c(mean(med.data.2014$diff.WA), mean(med.data.2015$diff.WA), mean(med.data.2016$diff.WA))

data.diff.m <- data.frame(years, y.diff.ca.m, y.diff.wa.m)

# visualizing the above calculations by year 
g.3 <- data.diff.m %>% 
  plot_ly() %>%
  add_trace(x = ~years, y = ~y.diff.ca.m, type = 'scatter', mode = "lines+markers+text", 
            text = ~y.diff.ca.m, textposition = 'middle right',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)),
            name = "San Francisco") %>%
  add_trace(x = ~years, y = ~y.diff.wa.m, type = 'scatter', mode = "lines+markers+text", 
            text = ~y.diff.wa.m, textposition = 'middle right',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)),
            name = "Seattle") %>%
  layout(title = "Trend of Difference in Mean Wages between Men & Women in Medical Roles (2014-2016)",
         xaxis = list(title = "Years"),
         yaxis = list(title = "Difference in Mean Wages ($)"))
g.3

# analyzing the trend of wages by occupation - mean 
med.data.o1 <- med.data[vars.needed]
xyz.m <- list((med.data.o1$soc_name))
med.data.o <- aggregate(x = med.data.o1,by= xyz.m, FUN = mean)
med.data.o$diff.ca <- med.data.o$Male.CA - med.data.o$Female.CA
med.data.o$diff.wa <- med.data.o$Male.WA - med.data.o$Female.WA

# visualizng the obove trend 
g.4 <- plot_ly(med.data.o, y = ~Group.1, x = ~diff.ca,
               type = 'bar', 
               text = ~Group.1, textposition = 'auto',
               marker = list(color = 'rgb(250,20,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)),
               name = "San Francisco")  %>%
  add_trace(x = ~diff.wa,
            type = 'bar', 
            text = ~Group.1, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5)),
            name = "Seattle") %>% 
  layout(title = "Occupational Trend of Difference in Wages between Men & Women in Medical Roles",
         barmode = 'group',
         yaxis = list(title = "Occupations", showticklabels = FALSE),
         xaxis = list(title = "Difference in Mean Wages ($)"))

g.4

# Paired t-test
# Null Hypothesis - San Francisco and Seattle have same wage gap (mean value) - med roles
med.data.t <- med.data.o %>% 
  gather(`diff.wa`, `diff.ca`, key = "City", value = "Mean Wage Difference")
med.data.t <- med.data.t[c("City", "soc_name", "Mean Wage Difference")]
t.test(med.data.t$`Mean Wage Difference` ~ med.data.t$City, data = med.data.t) 


