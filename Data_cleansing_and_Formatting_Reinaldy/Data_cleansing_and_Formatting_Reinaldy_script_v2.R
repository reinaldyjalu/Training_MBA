# Title       : Data_cleansing_and_Formatting_Reinaldy_script_v2.R
# Deskripsi   : Script R untuk tugas Data Formatting Training MBA by IYKRA
# Objective   : untuk memformat data setelah dilakukan cleansing
# Author      : Reinaldy Jalu Nusantara
# Data Source : https://archive.ics.uci.edu/ml/datasets/online+retail

# Install Package 'readxl' untuk membaca data input bertipe .xls dan .xlsx
install.packages('readxl')

# Load package 'readxl'
library('readxl')

# Membaca dataset bertipe excel
# data diambil dari: https://archive.ics.uci.edu/ml/datasets/online+retail
df_online_retail = read_excel('dataset/Online_Retail.xlsx')

# Tampilkan 6 data teratas 
head(df_online_retail)

# Tampilkan 6 data terbawah
tail(df_online_retail)

# Tampilkan summary data
summary(df_online_retail)

# Tampilkan jumlah missing data ke dalam plot
library(DataExplorer)
plot_missing(df_online_retail)
# Terdapat 24.93% missing data pada field CustomerID

# Tampilkan struktur data
str(df_online_retail)

# Drop data dengan CustomerID = Na
df_online_retail_drop = df_online_retail[!is.na(df_online_retail$CustomerID),] 

# Cek Missing data menggunakan plot
plot_missing(df_online_retail_drop)
#Sudah tidak ada missing data di field CustomerID

# Cek Summary
summary(df_online_retail_drop)

#load library 'tidyverse'
library('tidyverse')

# Membuat tabel dengan 2 variabel: 
# CustomerID | frequency
# frequency adalah berapa kali satu orang membeli barang
frequency = df_online_retail_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))
head(frequency)

# Membuat tabel dengan 2 variabel:
# CustomerID | monetary
# monetary adalah berapa jumlah uang yang dibelanjakan oleh masing-masing orang
monetary = df_online_retail_drop %>% group_by(CustomerID) %>% summarise(monetary = sum(UnitPrice*Quantity))
head(monetary)

# Menambahkan satu kolom di tabel df_online_retail_drop bernama 'recency'
# recency berisi berapa hari sejak pembelian terakhir customer ke tanggal 31 Desember 2011
library('lubridate')
recency = df_online_retail_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate, ymd('2011-12-31'))))/86400)
head(recency)

# Join ketiganya
df_rfm = recency %>% left_join(frequency, by = 'CustomerID') %>% left_join(monetary, by = 'CustomerID')

# Tampilkan hanya CustomerID, Recency, Frequency, dan Monetary sesuai soal:
df_result = df_rfm %>% select(CustomerID, recency, frequency, monetary)
head(df_result)

# write result ke file csv di direktori: ../dataset/
write.csv(df_result, file = "dataset/online_retail_result_reinaldy.csv", quote=F, row.names = F)
