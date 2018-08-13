#PLEASE NOTE THIS CODE WILL TAKE 3-4 MINS TO COMPLETE THE EXECUTION
#MANY GRAPHS ARE GENERATED!!
#THANK YOU FOR YOUR PATIENCE
#--clearing workingspace and loading required libraries ------- --------
#clearing workingspace and loading required libraries 
rm(list=ls())
library(xlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
#install.packages("reshape2")
library(reshape2)
library(xlsx)
#install.packages("corrplot")
library(corrplot)
#install.packages("GGally")

library(GGally)
#seting working directory
setwd("D:\\Loans Case Study\\")
getwd()
start_time <- Sys.time()
#--loading dataframe with loans data--------------
tmp_loans.data <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

#Data Understanding  and Prepration
str(tmp_loans.data)
summary(tmp_loans.data)
#View(tmp_loans.data)
#Data(frame) contains 39717 obs. of  111 variables. With many NA's.

#checking NA's counts per variable / colomns.
(tmp_loan.data_NA_stats <- sapply(colnames(tmp_loans.data), function(x) length(which(is.na(tmp_loans.data[,x])))))
str(tmp_loan.data_NA_stats)
length_id <- length(tmp_loans.data$id)

#--1.  Data preparation ---- 
#--1.1 checkinig for duplicates ------------
which(duplicated(tmp_loans.data))
which(duplicated(tmp_loans.data$id))
which(duplicated(tmp_loans.data$member_id))
which(duplicated(tmp_loans.data))
#no duplicates found

#--1.2 Removing cols that have all NA's -----
#Many cols have all NA's!! Implying these variable have no meaningfull information for any analysis.
#Removing all the colomns that have all NA's (39717 NA's)
#is.na(loans.data[])
all_na_cols <- lapply(colnames(tmp_loans.data), function(x)  length(which(is.na(tmp_loans.data[,x])))!= length_id)
class(all_na_cols)
loans.data <- tmp_loans.data[,which(all_na_cols[]==1)]
summary(loans.data)
str(loans.data)

# about 54 colomns had all observations as NA's; these are removed.

#--1.3 Data modification (format changes, derived fields etc) ---------------
#-- Date variable conversions
loans.data$issue_d_conv <-  as.Date(paste('01-',loans.data$issue_d,sep=''),'%d-%b-%y')
str(loans.data$issue_d_conv)
summary(loans.data$issue_d_conv)
unique(loans.data$issue_d_conv)

loans.data$issue_d_conv_year <-  as.integer(format(loans.data$issue_d_conv, "%Y"))
str(loans.data$issue_d_conv_year)
unique(loans.data$issue_d_conv_year)

loans.data$issue_d_conv_month_num <-  as.integer(format(loans.data$issue_d_conv, "%m"))
str(loans.data$issue_d_conv_month)
unique(loans.data$issue_d_conv_month)

#interest rate conversion, Revolving line utilization rate 
loans.data$int_rate_conv <- as.numeric(gsub("%", "", loans.data$int_rate))
summary(loans.data$int_rate_conv)

loans.data$revol_util_conv <- as.numeric(gsub("%", "", loans.data$revol_util))
summary(loans.data$revol_util_conv)

#calculating the actual charged off amount; subtract net principle paid from principle
loans.data$derieved_chargedoff_amnt <- loans.data$funded_amnt - loans.data$total_rec_prncp
str(loans.data$derieved_chargedoff_amnt)

#calculating the charge amount as percentage of funded_amt
loans.data$derieved_chargedoff_per = round((loans.data$derieved_chargedoff_amnt * 100 / loans.data$funded_amnt),  digit =2)

#term conversion into number 36 / 60
loans.data$term_conv <- as.numeric(trimws((gsub(" months", "", loans.data$term))))

#employment duration to numeric
loans.data$emp_length_conv <- gsub("years", "", loans.data$emp_length)
loans.data$emp_length_conv <- gsub("year", "", loans.data$emp_length_conv)
loans.data$emp_length_conv <- gsub("\\+", "", loans.data$emp_length_conv)
loans.data$emp_length_conv <- gsub("<", "", loans.data$emp_length_conv)
loans.data$emp_length_conv <- as.numeric(loans.data$emp_length_conv)


str(loans.data$emp_length_conv)
summary(loans.data$emp_length_conv)
unique(loans.data$emp_length_conv)
#--2. Quick statistics on numeric variables / continious variables-----
#geting the class of variables
(loans.data.col_class <- unlist(sapply(loans.data,class)))
(loans.data.var_continious_summary <- sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) summary(x)))

#geting the quantile for numeric and integer variables.
quantile_for <- c(0, 0.25, 0.50, 0.75, 1.00)
loans.data.var_continious_summary <- sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) quantile(x, quantile_for, na.rm = TRUE))
#View(loans.data.var_continious_summary)
#sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) length(which(is.na(x))))

#geting count of mean for numeric and integer variables.
variable_means <- as.numeric(sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) mean(x, rm.na = TRUE)))
loans.data.var_continious_summary <- rbind(loans.data.var_continious_summary, variable_means)
#str(loans.data.var_continious_summary)

#geting count of NA's, not NA's and total for numeric and integer variables.
no_of_NAs <- unlist(sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) length(which(is.na(x)))))
loans.data.var_continious_summary <- rbind(loans.data.var_continious_summary, no_of_NAs)
#View(loans.data.var_continious_summary)
not_NAs <- unlist(sapply(loans.data[,loans.data.col_class=="numeric" | loans.data.col_class=="integer"], function(x) length(which(!is.na(x)))))
loans.data.var_continious_summary <- rbind(loans.data.var_continious_summary, not_NAs)

total <- no_of_NAs + not_NAs
loans.data.var_continious_summary <- rbind(loans.data.var_continious_summary, total)

#geting count of NA's in percentage for numeric and integer variables.
no_of_NAs_percentage <- (no_of_NAs / length_id) * 100
loans.data.var_continious_summary <- rbind(loans.data.var_continious_summary, no_of_NAs_percentage)
#str(loans.data.var_continious_summary)
#View(loans.data.var_continious_summary)

#transposeing for better readability
loans.data.var_continious_summary <- t(loans.data.var_continious_summary)
loans.data.var_continious_summary[,1:6] <- round(loans.data.var_continious_summary[,1:6], digits = 2)
loans.data.var_continious_summary[,7:9] <- round(loans.data.var_continious_summary[,7:9], digits = 0)
loans.data.var_continious_summary[,10] <- round(loans.data.var_continious_summary[,10], digits = 2)

View(loans.data.var_continious_summary)

#--3. Quick statistics on Categorical------------
#selecting categorical variables that has unique values < 55. Sub grade seem to have hi number of unique values. 
#one can convert all the charector variables into factor to find out the no of unique.. but i want to summarize in a
#df the summary of categorical variables.
str(loans.data)
loans.data.col_class <- unlist(sapply(loans.data,class))
loans.data.colnames_categorical <- sapply(loans.data[,loans.data.col_class=="character"], 
                                           function(x) length(unique(x)) < 55)
loans.data.var_categorical <- loans.data[,names(loans.data.colnames_categorical)]
str(loans.data.var_categorical)
length(unique(loans.data.var_categorical$issue_d))
View(loans.data.var_categorical)

#function to append the summary of categorical variables one after the other. the summary contains the name, count
#count % and the category
variable_sumary <- function(x) {
  result <- loans.data %>%
    group_by(loans.data.var_categorical[,x]) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
  sum_count <- sum(result$count)
  result$count_percentage <- round((result$count *100 / sum_count),2) 
  names(result)[1] <- "Category"
  result$categorical_Variable_names <-  names(loans.data.var_categorical[x])
  return(result)
}

loans.data.var_categorical_summary <- 1
for (i in 1:ncol(loans.data.var_categorical)) {
  if (length(unique(loans.data.var_categorical[,i])) <= 55){
      print (i)
      loans.data.var_categorical_summary <- rbind(loans.data.var_categorical_summary, variable_sumary(i))
    next
  }
}

#View(loans.data.var_categorical_summary)
# removing the first observation that was to initialize the df.
loans.data.var_categorical_summary <- loans.data.var_categorical_summary %>% 
  filter(categorical_Variable_names != "1")

#quick check to see if we are misssing any observation. all count shoud be equal to 39717
loans.data.var_categorical_summary %>%
  group_by(categorical_Variable_names) %>%
  summarise(sum(count))
View(loans.data.var_categorical_summary)

#--4. quick plots on categorical variable -------------
#--4.1 univariate categorical plots--------
plot.graphs <- function(cat_x) {
#  result <- loans.data %>%
   print(cat_x)
   print("processing graphs. thank you for your patience... Please wait...")
   graph_tmp <- loans.data.var_categorical_summary %>%
     filter(categorical_Variable_names == cat_x)
  
   result <- ggplot(data = graph_tmp, aes(x = Category, y = count, color = Category, fill = Category)) +
                    geom_bar(alpha = 0.7, stat = "identity") + 
                    labs(x = cat_x) +
                    geom_text(aes(label = count), position = position_dodge(1), color = "black") 
#    labs(colour = x)
  return(result)
}

category_collection <- unique(loans.data.var_categorical_summary$categorical_Variable_names)

for(j in 1:length(category_collection)){
  plot(plot.graphs(category_collection[j]))
}



##
# Key inferences
# - term - 36 months has 29096 and 10621 loans observation including closed, charged of and current. 
# - grade 'B' and 'A' had the max observation
# - sub_grade B3, A4, A3, B5, B4 has max loan observations than the rest
# - emp_length '10+' has the significant loan observation 8879 than the rest.
# - home_ownership 'RENT' and 'MORTGAGE' combined has nearly all the observations.
# - addr_state 'CA' has the max observation 7099, next is NY - 3812
# - based on loan_status fully paid, charged  off, current has 39250, 5627 and 1140 observations respectivly
# - purpose 'debt_consolidation' had the max observation next is 'credit_card'

#--4.2 bivariate categorical plots --------------
plot.graphs_continious <- function(cat_x, cat_y, cat_color) {
#  print("start")
   print(cat_x)
#  print(cat_y)
#  print(cat_color)
   print("processing graphs. thank you for your patience... Please wait...")
  #1. stack categorical variable vs count
  plot(ggplot(data = loans.data) +
    geom_bar(aes_string(x = cat_x, fill = cat_color), alpha = 0.8, 
             stat = "count", position = "stack")) 
  
  #2. fill categorical variable colored by another categorical values
  plot(ggplot(data = loans.data) +
    geom_bar(aes_string(x = cat_x, y = cat_y,  fill = cat_color), alpha = 0.8, 
             stat = "identity", position = "fill")) 
  
  
  #3. fill categorical variable vs agregated value, colored by another categorical values
  plot(ggplot(data = loans.data) +
    geom_bar(aes_string(x = cat_x, y = cat_y,  fill = cat_color), alpha = 0.8, 
             stat = "identity", position = "stack"))
  result <- "successfuly ending"
#  print(result)
  return(result)
}

#category_collection <- unique(loans.data.var_categorical_summary$categorical_Variable_names)
# based on the earlier plots run  plots 

category_collection_ploting <- c("term", "grade", "sub_grade", "emp_length", "home_ownership", "verification_status",
"issue_d", "loan_status", "purpose", "addr_state")
#ploting categorical variables vs count colored by loan_status

for(j in 1:length(category_collection_ploting)){
  plot.graphs_continious(category_collection_ploting[j], "funded_amnt", "loan_status")
}

#ploting categorical variables vs count colored by purpose
for(j in 1:length(category_collection_ploting)){
  plot.graphs_continious(category_collection_ploting[j], "funded_amnt", "purpose")
}

#ploting categorical variables vs count colored by verification_status
for(j in 1:length(category_collection_ploting)){
   plot.graphs_continious(category_collection_ploting[j], "funded_amnt", "verification_status")
}



#--5.histogram graph on continious variable ------
#--5.1 on funded amount (actual amout that is funded) where is the risk to the financial institution-----
ggplot(data = loans.data) +
  geom_freqpoly(aes(x=funded_amnt, color = loan_status)) +
#  geom_histogram(aes(x=funded_amnt, fill = loan_status), binwidth=100,alpha = 0.7)
  geom_histogram(aes(x=funded_amnt, fill = loan_status), binwidth=1000,alpha = 0.7)
#loans are geting rounded of to the neares 500 or 5000. spike at 5000, 10000, 15000 etc!!

ggplot(data = loans.data) +
  geom_freqpoly(aes(x=funded_amnt, color = verification_status)) +
  #  geom_histogram(aes(x=funded_amnt, fill = loan_status), binwidth=100,alpha = 0.7)
  geom_histogram(aes(x=funded_amnt, fill = verification_status), binwidth=1000,alpha = 0.7)


#theres a huge count of not verified once, faceting it by loan_status
ggplot(data = loans.data) +
  geom_freqpoly(aes(x=funded_amnt, color = verification_status)) +
  #  geom_histogram(aes(x=funded_amnt, fill = loan_status), binwidth=100,alpha = 0.7)
  geom_histogram(aes(x=funded_amnt, fill = verification_status), binwidth=1000,alpha = 0.7)+
  facet_grid(.~ loan_status)

#calculating the actual charged off amount
#subtract net principle paid from principle
#
loans.data$derieved_chargedoff_amnt <- loans.data$funded_amnt - loans.data$total_rec_prncp
loans.data1 <- loans.data %>%
  filter(loans.data$loan_status != "Fully Paid")
ggplot(data = loans.data1) +
  geom_freqpoly(aes(x=derieved_chargedoff_amnt, color = verification_status)) +
  #  geom_histogram(aes(x=derieved_chargedoff_amnt, fill = loan_status), binwidth=100,alpha = 0.7)
  geom_histogram(aes(x=derieved_chargedoff_amnt, fill = verification_status), binwidth=1000,alpha = 0.7)+
  facet_grid(.~ loan_status)

#--5.2 funded amount frequency vs grade, funded amout frequency vs sub grades --------
funded_amnt_by_grade_vs_status <- loans.data %>%
  select(funded_amnt, grade, loan_status) %>%
  group_by(grade, loan_status) %>%
  summarise(total_funded_amnt = sum(funded_amnt))

#View(funded_amnt_by_grade_vs_status)
ggplot(data = funded_amnt_by_grade_vs_status) +
  geom_bar(aes(x = grade, y = total_funded_amnt,  fill = loan_status), alpha = 0.8, 
           stat = "identity", position = "dodge") 
#this is not the exat ammount charged off; this just shows the funded amount of fully paid, 
#current and charged offin terms of actual amout at risk is in B, C, D, E

#ploting charged off amount againt subgrades

chargedoff_summary_data <- loans.data %>%
  select(funded_amnt, derieved_chargedoff_amnt, sub_grade, loan_status) %>%
  group_by(sub_grade, loan_status) %>%
  summarise(total_funded_amnt = sum(funded_amnt),
            total_derieved_chargedoff_amnt = as.integer(sum(derieved_chargedoff_amnt)),
            derieved_chargedoff_per = (total_funded_amnt - total_derieved_chargedoff_amnt)* 100 / total_funded_amnt,
    count=n())%>%
  arrange(desc(total_derieved_chargedoff_amnt))

View(chargedoff_summary_data)
sum(chargedoff_summary_data$total_derieved_chargedoff_amnt)
sum(chargedoff_summary_data$total_funded_amnt)

str(chargedoff_summary_data$total_funded_amnt)
str(chargedoff_summary_data$total_derieved_chargedoff_amnt)
ggplot(data = chargedoff_summary_data) +
  geom_bar(aes(x = sub_grade, y = total_derieved_chargedoff_amnt,  fill = loan_status), alpha = 0.8, 
           stat = "identity", position = "dodge") 

for(j in 1:length(category_collection_ploting)){
  plot.graphs_continious(category_collection_ploting[j], "derieved_chargedoff_amnt", "verification_status")
}
#length(is.na(loans.data$derieved_chargedoff_amnt))

#--5.3----box plots for DTI, interest across categorical variables ----------
ggplot(data = loans.data) +  
  geom_boxplot(aes(x = grade, y = int_rate_conv, fill = grade), alpha=0.7) 
  
ggplot(data = loans.data) +  
  geom_boxplot(aes(x = sub_grade, y = int_rate_conv, fill = sub_grade), alpha=0.7) 

ggplot(data = loans.data) +  
  geom_boxplot(aes(x = grade, y = revol_util_conv, fill = grade), alpha=0.7) 

ggplot(data = loans.data) +  
  geom_boxplot(aes(x = sub_grade, y = revol_util_conv, fill = sub_grade), alpha=0.7) 



ggplot(data = loans.data) +  
  geom_boxplot(aes(x = grade, y = dti, fill = grade), alpha=0.7) 

ggplot(data = loans.data) +  
  geom_boxplot(aes(x = sub_grade, y = dti, fill = sub_grade), alpha=0.7) 

#--5.4 Scater plot---------
ggplot(data=loans.data)+
  geom_point(aes(x = derieved_chargedoff_amnt, y = dti))

ggplot(data=loans.data)+
  geom_point(aes(x = derieved_chargedoff_amnt, y = int_rate_conv))

ggplot(data=loans.data)+
  geom_point(aes(x = dti, y = int_rate_conv, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = dti, y = int_rate_conv), alpha = 0.5 , position="jitter")


ggplot(data=loans.data)+
  geom_point(aes(x = funded_amnt, y = dti, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = dti), alpha = 0.5 , position="jitter")
#  facet_grid(grade ~ verification_status)

ggplot(data=loans.data)+
  geom_point(aes(x = funded_amnt, y = int_rate_conv, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = int_rate_conv), alpha = 0.5 , position="jitter")
#  facet_grid(grade ~ verification_status)

ggplot(data=loans.data)+
  geom_point(aes(x = funded_amnt, y = annual_inc, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = annual_inc), alpha = 0.5 , position="jitter")


tmp_graph_data <- loans.data %>%
  filter(derieved_chargedoff_per > 5 & loan_status == "Charged Off" & (annual_inc < 100000 | is.na(annual_inc))) %>%
#  filter(loan_status == "Charged Off") %>%
  select(funded_amnt,
         derieved_chargedoff_amnt, 
         annual_inc,
         dti, 
         int_rate_conv, 
         revol_util_conv, 
         grade,
         verification_status,
         derieved_chargedoff_per) 

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = funded_amnt, y = annual_inc, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = annual_inc), alpha = 0.5 , position="jitter")

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = funded_amnt, y = annual_inc, size=derieved_chargedoff_amnt, color = derieved_chargedoff_amnt,fill = derieved_chargedoff_amnt), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = annual_inc), alpha = 0.5 , position="jitter") +
  facet_grid(. ~ grade)

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = funded_amnt, y = annual_inc, size=derieved_chargedoff_per, color = derieved_chargedoff_per, fill = derieved_chargedoff_per), alpha=0.6)+
  geom_jitter(aes(x = funded_amnt, y = annual_inc), alpha = 0.5 , position="jitter")

#dti vs revol_util %
ggplot(data=tmp_graph_data)+
  geom_point(aes(x = dti, y = revol_util_conv, size=derieved_chargedoff_per, color = derieved_chargedoff_per, fill = derieved_chargedoff_per), alpha=0.6)+
  geom_jitter(aes(x = dti, y = revol_util_conv), alpha = 0.5 , position="jitter")
#inference higher the dti and revol_util %, higher the chagrge off and charge off amount

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = dti, y = revol_util_conv, size=derieved_chargedoff_per, color = verification_status, fill = verification_status), alpha=0.6)+
  geom_jitter(aes(x = dti, y = revol_util_conv), alpha = 0.5 , position="jitter")
#inference higher the dti and revol_util %, higher the chagrge off and charge off amount

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = dti, y = int_rate_conv, size=derieved_chargedoff_per, color = derieved_chargedoff_per, fill = derieved_chargedoff_per), alpha=0.6)+
  geom_jitter(aes(x = dti, y = int_rate_conv), alpha = 0.5 , position="jitter")

ggplot(data=tmp_graph_data)+
  geom_point(aes(x = dti, y = int_rate_conv, size=derieved_chargedoff_per, color = verification_status, fill = verification_status
                 ), alpha=0.6)+
  geom_jitter(aes(x = dti, y = int_rate_conv), alpha = 0.5 , position="jitter")

#by profession
tmp_graph_data <- loans.data %>%
  select(funded_amnt, 
         derieved_chargedoff_amnt, 
         annual_inc,
         dti, 
         int_rate_conv, 
         grade,
         derieved_chargedoff_per,
         emp_title) %>%
  mutate(emp_title_conv = tolower(emp_title))%>%
  group_by(emp_title_conv) %>%
  summarise(sum(funded_amnt), sum(derieved_chargedoff_amnt),count=n()) %>%
  arrange(desc(count))
#View(tmp_graph_data)


#--6. correlation matrix------------

correlation_data <- loans.data %>%
#    filter(loan_status == "Charged Off") %>%
    filter(derieved_chargedoff_per > 5 & loan_status == "Charged Off" & (annual_inc < 100000 | is.na(annual_inc))) %>%
  
    select(funded_amnt,
         int_rate_conv,
         revol_util_conv,
         emp_length_conv,
#         grade,
#         home_ownership,
#         purpose,
         annual_inc,
         dti,
         derieved_chargedoff_amnt,
         term_conv,
         derieved_chargedoff_per)

#View(correlation_data)

cor(correlation_data, method = "pearson", use = "complete.obs")
ggpairs(correlation_data)
#--7 writing dataframes to excel csv file for analysis in Tableau ----------
#--writing to csv file as xlsx write take hell lot of time, compromising writing to multile tabs in xlsx file. hence multiple csv's
write.csv(loans.data, "LOANS-R-OUTPUT.csv")
write.csv(loans.data.var_continious_summary, "loans.data.var_continious_summary.csv")
write.csv(loans.data.var_categorical_summary, "loans.data.var_categorical_summary.csv")

print(start_time)
Sys.time()
print("Program ended, thank you!")
