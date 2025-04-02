# Load required packages
library(tidyverse)
library(lubridate)

# Create sample data (replace with your actual data)
set.seed(123)
data <- tibble(
  customer_id = 1:1000,
  cohort = sample(c("2023-01", "2023-02", "2023-03", "2023-04"), 
                  1000, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  signup_date = as.Date(paste0(cohort, "-01")) + days(sample(0:27, 1000, replace = TRUE)),
  activity_month = signup_date %m+% months(sample(0:6, 1000, replace = TRUE)),
  revenue = round(runif(1000, 10, 500), 2),
  active = sample(c(0,1), 1000, replace = TRUE, prob = c(0.3, 0.7))
)

# Calculate vintage metrics
vintage_data <- data %>%
  mutate(months_since_signup = interval(signup_date, activity_month) %/% months(1)) %>%
  group_by(cohort, months_since_signup) %>%
  summarize(
    customers = n_distinct(customer_id),
    total_revenue = sum(revenue),
    avg_revenue = mean(revenue),
    active_customers = sum(active),
    retention_rate = active_customers / first(customers)
  ) %>%
  arrange(cohort, months_since_signup)

# View the results
print(vintage_data)

# Visualize retention by cohort
ggplot(vintage_data, aes(x = months_since_signup, y = retention_rate, 
                         color = cohort, group = cohort)) +
  geom_line() +
  geom_point() +
  labs(title = "Customer Retention by Cohort",
       x = "Months Since Signup",
       y = "Retention Rate") +
  theme_minimal()

# Visualize revenue by cohort
ggplot(vintage_data, aes(x = months_since_signup, y = avg_revenue, 
                         color = cohort, group = cohort)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Revenue by Cohort",
       x = "Months Since Signup",
       y = "Average Revenue") +
  theme_minimal()



#########PERFORMANCE WINDOW USING VINTAGE ANALYSIS
library(tidyverse)
library(lubridate)


set.seed(123)
n_customers <- 1000

# First create the cohort assignments
cohort_assignments <- sample(c("2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4"), 
                             n_customers, replace = TRUE)

# Then create the data frame
data <- tibble(
  customer_id = 1:n_customers,
  cohort = sample(c("2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4"), 
                  n_customers, replace = TRUE)
) %>%
  group_by(cohort) %>%
  mutate(
    signup_date = case_when(
      cohort == "2023-Q1" ~ as.Date("2023-01-01") + days(sample(0:89, n(), replace = TRUE)),
      cohort == "2023-Q2" ~ as.Date("2023-04-01") + days(sample(0:89, n(), replace = TRUE)),
      cohort == "2023-Q3" ~ as.Date("2023-07-01") + days(sample(0:89, n(), replace = TRUE)),
      cohort == "2023-Q4" ~ as.Date("2023-10-01") + days(sample(0:89, n(), replace = TRUE))
    )
  ) %>%
  ungroup() %>%
  mutate(
    activity_date = signup_date + days(sample(0:365, n(), replace = TRUE)),
    revenue = ifelse(activity_date >= signup_date, round(runif(n(), 10, 500), 2), 0)
  )
## fixed window approach
window_length <- 6 # months

vintage_fixed_window <- data %>%
  mutate(
    months_since_signup = interval(signup_date, activity_date) %/% months(1)
  ) %>%
  filter(months_since_signup <= window_length) %>%
  group_by(cohort, months_since_signup) %>%
  summarize(
    active_customers = n_distinct(customer_id),
    total_revenue = sum(revenue),
    .groups = "drop"
  )

## Analyzing window performance
# Calculate cumulative metrics within window
window_analysis <- vintage_fixed_window %>%
  group_by(cohort) %>%
  arrange(months_since_signup) %>%
  mutate(
    cum_revenue = cumsum(total_revenue),
    retention_rate = active_customers / first(active_customers)
  ) %>%
  ungroup()

# Visualize window performance
ggplot(window_analysis, aes(x = months_since_signup, y = cum_revenue, color = cohort)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = window_length, linetype = "dashed") +
  labs(title = "Cumulative Revenue Within Performance Window",
       x = "Months Since Signup",
       y = "Cumulative Revenue",
       color = "Cohort") +
  theme_minimal()

## Choosing Optimal Window Length
# Test different window lengths
window_lengths <- 3:12
window_results <- map_df(window_lengths, ~{
  data %>%
    mutate(months_since_signup = interval(signup_date, activity_date) %/% months(1)) %>%
    filter(months_since_signup <= .x) %>%
    group_by(cohort) %>%
    summarize(
      window_length = .x,
      total_revenue = sum(revenue),
      avg_revenue_per_customer = sum(revenue)/n_distinct(customer_id),
      .groups = "drop"
    )
})

# Find inflection point where returns diminish
optimal_window <- window_results %>%
  group_by(window_length) %>%
  summarize(avg_revenue = mean(total_revenue)) %>%
  mutate(revenue_growth = avg_revenue - lag(avg_revenue)) %>%
  filter(revenue_growth < 0.1 * avg_revenue) %>% # When growth slows to <10%
  slice(1) %>%
  pull(window_length)

