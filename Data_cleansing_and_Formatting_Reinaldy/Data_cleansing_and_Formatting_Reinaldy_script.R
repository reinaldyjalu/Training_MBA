# Title       : Formatting_Data_Online_Retail.R 
# Description : This is a formatting process of online
#               retail dataset using RFM Method
# Objective   : To format data (after cleaning)
# Author      : Reinaldy Jalu Nusantara
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail

#load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

#load data
df_onlineretail = read.csv('dataset/OnlineRetail.csv', stringsAsFactors = F)

#preview first 6 rows of data
head(df_onlineretail)

#preview last 6 rows of data
tail(df_onlineretail)

#summary of the data
summary(df_onlineretail)

#display a plot showing frequency of missing values of each variable
#didapatkan bahwa missing values field CustomerID sebanyak 24.93%
plot_missing(df_onlineretail)

#set benih random
set.seed(10)

#sampling data
ol_sample = df_onlineretail[sample(1:nrow(df_onlineretail), size=10000),]
summary(ol_sample)

#we are going to clean ol_sample data

df_data = ol_sample

#checking variable data
str(df_data)
plot_str(df_data)

#InvoiceDate is in 'character' data type
#convert InvoiceDate to Date data type
df_data$InvoiceDate = dmy_hm(df_data$InvoiceDate)

#Are there any missing data?
plot_missing(df_data)

# Customer ID have 25% missing value. 
#(in this case, we think that those empty data because the customer has no customer ID or bought as a guest)

#If the data is going to be dropped:
df_data_drop = df_data[!is.na(df_data$CustomerID),]

#If the data with empty CustomerID is going to be kept
#replace with unique value that did not yet inputted
max_CustID = max(df_data[!is.na(df_data$CustomerID),]$CustomerID)
df_data_imput = df_data
df_data_imput$CustomerID[is.na(df_data_imput$CustomerID)] = sample(max_CustID+10000:max_CustID+10000+length(df_data$CustomerID), size = sum(is.na(df_data_imput$CustomerID)), replace=F)
length(df_data[is.na(df_data$CustomerID),])

#check missing data again
plot_missing(df_data_drop)
plot_missing(df_data_imput)

# Quiz        : Please make a tidy table from produk, transaksi, and profil_pelanggan,
#             , thus contain the following variables using df_data_drop data frame:
# CustomerID | Recency | Frequency | Amount
# Recency     : Jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency   : Jumlah transaksi yang terjadi dalam 6 bulan terakhir
# Monetary    : Jumlah uang yang dibelanjakan oleh CustomerID unik

frequency = df_data_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))

monetary = df_data_drop %>% group_by(CustomerID) %>% summarise(monetary = sum(UnitPrice*Quantity))

recency = df_data_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate, ymd("2011-12-31"))))/86400)

# Quiz join ketiga nya
#(hint: dengan menggunakan left_join function)

df_rfm = recency %>% left_join(frequency, by="CustomerID") %>% left_join(monetary, by="CustomerID")

write.csv(df_rfm, "data/online_retail_clean_reinaldy.csv", quote=F, row.names = F)

# Check summary df_rfm, 
summary(df_rfm)

# Median of recency, frequency, and monetary already known

# Encode with this condition: 
# If more than median, then high
# if less than median, then low
# and assign as variabel: df_rfm_encode

med_recency = median(df_rfm$recency)
med_frequency = median(df_rfm$frequency)
med_monetary = median(df_rfm$monetary)

df_rfm_encode = df_rfm %>% mutate(recency = ifelse(recency<=med_recency,0,1),
                                  frequency = ifelse(frequency<=med_freq,0,1),
                                  monetary = ifelse(monetary<=med_monetary,0,1))

write.csv(df_rfm_encode, "online_retail_cluster_reinaldy.csv", quote = F, row.names = F)

# Create new dataframe which consists of CustomerID | Country
origin = df_data_drop %>% select(CustomerID, Country) %>% distinct()

#join with df_rfm_encode with origin data frame and assign as df_rfm_full
df_rfm_full = df_rfm_encode %>% left_join(origin, by=)

