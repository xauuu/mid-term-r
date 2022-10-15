#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("gridExtra")

# Import library
library(dplyr)
library(tidyverse)
library(lubridate)
library(gridExtra)
# Read in dataset
data = read.csv('https://raw.githubusercontent.com/xauuu/mid-term-r/main/sales.csv', header = T, na.strings = '')
names(data)
dim(data)
## đổi tên cột sang chữ thường
data <- rename_with(data, tolower)
## replace '.' to '_'
names(data) <- gsub("\\.", "_", names(data))
str(data)
## đổi tên cột tax_5_ thành tax_5
data <- rename(data, tax_5 = tax_5_)
str(data)
# loại bỏ trùng lặp
data <- distinct(data)
dim(data)
View(data)
summary(data)

# kiểm tra các cột char để biết các giá trị phù hợp
table(data$branch)
table(data$city)
table(data$customer_type)
table(data$gender)
table(data$product)
table(data$payment)

#type conversion
#data$branch <- as.factor(data$branch)
#data$customer_type <- as.factor(data$customer_type)
#data$gender <- as.factor(data$gender)
#data$city <- as.factor(data$city)
#data$product <- as.factor(data$product)
#data$payment <- as.factor(data$payment)

# thay đổi cột ngày sang kiểu Date
data$date <- as.Date(data$date, "%m/%d/%y")

str(data)

colSums(is.na(data))

# Loại bỏ NA
data_omit <- na.omit(data)
colSums(is.na(data_omit))

# tạo các cột mới cho năm, tháng và ngày
data$year <- format(as.Date(data$date), "%Y")
data$month <- format(as.Date(data$date), "%m")
data$day <- format(as.Date(data$date), "%d")


data$month_name <-
  factor(
    as.numeric(data$month),
    levels = c(1, 2, 3),
    labels = c("January", "February", "March")
  )

data$weekday <- factor(
  wday(data$date, week_start = 1),
  levels = c(1, 2, 3, 4, 5, 6, 7),
  labels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
)

# Chi nhánh nào có doanh số cao nhất?
sales_by_store <- data %>%
  group_by(branch) %>%
  summarize(total_sales = sum(total))
#x <- group_by(data, branch)
#sales_by_store <- summarize(x, t = sum(total))
#sales_by_store
print(sales_by_store)

sales_by_store %>%
  ggplot(aes(x = branch, y = total_sales)) +
  geom_col(fill = 'green4') +
  labs(
    title = "Total Sales by Store",
    subtitle = "Sales between Jan-Mar 2019",
    x = "Branch",
    y = "Total Sales $"
  )

# Chi nhánh nào có doanh số cao nhất theo tháng?
sales_by_store_month <- data %>%
  group_by(branch, month) %>%
  summarize(total_sales = sum(total))

sales_by_store_month %>%
  ggplot(aes(x = branch, y = total_sales, fill = branch)) +
  geom_col() +
  labs(
    title = "Sales by Branch",
    subtitle = "Sales between Jan-Mar 2019",
    x = "Branch",
    y = "Total Sales"
  ) +
  facet_wrap(~ month)

# Trung bình các đánh giá của mỗi chi nhánh?
avg_ratings_store <- data %>%
  group_by(branch) %>%
  summarize(avg_store_rating = mean(rating))
avg_ratings_store

# Phương thức thanh toán nào phổ biến nhất?
pmt_method <- data %>%
  group_by(payment) %>%
  count()
pmt_method

pie(pmt_method$n,
    labels = c("Cash", "Credit Card", "E-wallet"),
    main = "Breakdown of Payment Methods")
