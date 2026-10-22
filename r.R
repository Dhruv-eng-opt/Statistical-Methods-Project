View(UPI_Policy_Impact_Dataset)
head(UPI_Policy_Impact_Dataset)

install.packages(c("ggplot2", "dplyr", "corrplot"))
library(ggplot2)
library(dplyr)
library(corrplot)

str(UPI_Policy_Impact_Dataset)
summary(UPI_Policy_Impact_Dataset)


####################################################################
ggplot(UPI_Policy_Impact_Dataset, aes(x = Date, y = Transaction_Volume_Millions)) +
  geom_line(color = "blue") +
  labs(title = "UPI Transaction Volume Growth Over Time",
       x = "Year", y = "Volume (Millions)")


#################################################################3
ggplot(UPI_Policy_Impact_Dataset, aes(x = Transaction_Value_INR_Crores,
                 y = Transaction_Volume_Millions)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm") +
  labs(title = "Transaction Value vs Volume",
       x = "Value (INR Crores)", y = "Volume (Millions)")


#################################################################
ggplot(UPI_Policy_Impact_Dataset, aes(x = Avg_Ticket_Size_INR,
                 y = Transaction_Volume_Millions)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm") +
  labs(title = "Avg Ticket Size vs Volume",
       x = "Avg Ticket Size", y = "Volume")

#############################################################

numeric_UPI_Policy_Impact_Dataset <- UPI_Policy_Impact_Dataset %>% select_if(is.numeric)

cor_matrix <- cor(numeric_UPI_Policy_Impact_Dataset)

corrplot(cor_matrix, method = "color", type = "upper")

#############################################################

ggplot(UPI_Policy_Impact_Dataset, aes(x = Date, y = Transaction_Volume_Millions)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.Date("2016-11-01"), color="red", linetype="dashed") +
  geom_vline(xintercept = as.Date("2020-03-01"), color="purple", linetype="dashed") +
  labs(title = "UPI Growth with Policy Events",
       x = "Year", y = "Volume")

############################################################
boxplot(UPI_Policy_Impact_Dataset$Transaction_Volume_Millions,
        main = "UPI Volume Outliers")

############################################################
#Build Multiple linear regression Model
model <- lm(Transaction_Volume_Millions ~ 
              Avg_Ticket_Size_INR +
              Policy_Demonetization_Nov2016 +
              Policy_Zero_MDR_Jan2020 +
              Policy_CreditCard_on_UPI_Jun2022 +
              Policy_Tighter_Limits_Aug2025 +
              Event_COVID19_Lockdown_Mar2020,
            data = UPI_Policy_Impact_Dataset)

summary(model)


############################################################
#Extract Key Values
# R-squared
summary(model)$r.squared

# Adjusted R-squared
summary(model)$adj.r.squared

# F-statistic
summary(model)$fstatistic

# Coefficients table
coef(summary(model))


############################################################
#Multicollinearity

install.packages("car")   # run once
library(car)
vif(model)

library(zoo)
# Calculate 3-month and 6-month moving averages for volume
UPI_Policy_Impact_Dataset$MA3 <- rollmean(
  UPI_Policy_Impact_Dataset$Transaction_Volume_Millions,
  k = 3,
  fill = NA,
  align = "right"
)
UPI_Policy_Impact_Dataset$MA6 <- rollmean(
  UPI_Policy_Impact_Dataset$Transaction_Volume_Millions,
  k = 6,
  fill = NA,
  align = "right"
)

# Plot smoothed trends
ggplot(UPI_Policy_Impact_Dataset, aes(x = as.Date(Date))) +
  geom_line(aes(y = Transaction_Volume_Millions, color = "Actual"), alpha = 0.3) +
  geom_line(aes(y = MA3, color = "3-Month MA"), size = 1) +
  geom_line(aes(y = MA6, color = "6-Month MA"), size = 1.2) +
  labs(title = "UPI Growth: Smoothed Trends", x = "Year", y = "Volume (Millions)")


# Define Phases
phase_A <- UPI_Policy_Impact_Dataset %>% filter(Date < "2020-03-01")
phase_B <- UPI_Policy_Impact_Dataset %>% filter(Date >= "2020-03-01")

# Run the test
wilcox.test(phase_A$MoM_Volume_Growth_Pct,
            phase_B$MoM_Volume_Growth_Pct)


