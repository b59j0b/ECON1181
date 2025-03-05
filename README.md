java cSUMMATIVE ASSIGNMENT  Overall word limit: 2000 words SUBMISSION INSTRUCTIONS Your completed assignment must be uploaded to Ultra 
no later than 02 May 2025 12 midday A penalty will be applied for work uploaded after 12:00 midday as detailed in the Student Information Hub.    You must leave sufficient time to fully complete the upload process before the deadline and check that you have received a receipt. At peak periods, it can take up to 30 minutes for a receipt to be generated. Assignments should be typed, using 1.5 spacing and an easy-to-read 12-point font. Assignments and dissertations/business projects must not exceed the word count indicated in the module handbook/assessment brief.The word count should:· Include all the text, including title, preface, introduction, in-text citations, quotations, footnotes and any other items not specifically excluded below.· Exclude diagrams, tables (including tables/lists of contents and figures), equations, executive summary/abstract, acknowledgements, declaration, bibliography/list of references and appendices. However, it is not appropriate to use diagrams or tables merely as a way of circumventing the word limit. If a student uses a table or figure as a means of presenting his/her own words, then this is included in the word count.Examiners will stop reading once the word limit has been reached, and work beyond this point will not be assessed. Checks of word counts will be carried out on submitted work, including any assignments or dissertations/business projects that appear to be clearly over-length. Checks may take place manually and/or with the aid of the word count provided via an electronic submission. Where a student has intentionally misrepresented their word count, the School may treat this as an offence under Section IV of the General Regulations of the University. Extreme cases may be viewed as dishonest practice under Section IV, 5 (a) (x) of the General Regulations.Very occasionally it may be appropriate to present, in an appendix, material which does not properly belong in the main body of the assessment but which some students wish to provide for the sake of completeness. Any appendices will not have a role in the assessment - examiners are under no obligation to read appendices and they do not form. part of the word count. Material that students wish to be assessed should always be included in the main body of the text.Guidance on referencing can be found on Durham University website and in the Student Information Hub.MARKING GUIDELINES Performance in the summative assessment for this module is judged against the following criteria:·   Relevance to question(s)·   Organisation, structure and presentation·   Depth of understanding·   Analysis and discussion·   Use of sources and referencing·   Overall conclusions
ECON1181 Mastering Data and Computation Individual Project Question 1 
1.   (a) Explain the code below on two levels:–   Explain each code block e.g., the first block recalls the relevant libraries. (40 marks)–   Explain the overall purpose of the code                                                                                                                         (20 marks)(b)    Furthermore, please make corrections if you run an error when executing the code.                   (10 marks)(c)       Finally identify and discuss the improvements to the code.                                                 (30 marks)library(tidyverse)
library(lubridate)
library(scales)
library(zoo)create_sample_data <- function() {
dates <- seq.Date(from = as.Date("2019-01-01"), 
to = as.Date("2021-12-31"), 
by =   "day")
set.seed(123)
n <- length(dates)
data <- data.frame(
date = rep(dates, 3),
income_group = factor(rep(c("low", "middle", "high"), each =   n)),
consumer_spending = rnorm(n * 3, mean =   100, sd =   10),
employment_rate = rnorm(n * 3, mean =   95, sd =   5),
business_revenue = rnorm(n * 3, mean =   100, sd =   15)
)
start <- which(dates == as.Date("2020-03-15"))
period <- start: (start + 90)apply_impact <- function(data, dates, impact_factors) {
data %>%
group_by(income_group) %>%
mutate(
consumer_spending = case_when(
date %in% dates[period]  income_group == "low" ~ 
consumer_spending * impact_factors$spending[1],
date %in% dates[period]  income_group == "middle" ~ 
consumer_spending * impact_factors$spending[2],
date %in% dates[period]  income_group == "high" ~ 
consumer_spending * impact_factors$spending[3],
TRUE ~ consumer_spending
),
employment_rate = case_when(
date %in% dates[period]  income_group == "low" ~ 
employment_rate * impact_factors$employment[1],
date %in% dates[period]  income_group == "middle" ~ 
employment_rate * impact_factors$employment[2],
date %in% dates[period]  income_group == "high" ~ 
employment_rate * impact_factors$employment[3],
TRUE ~ employment_rate
),
business_revenue = case_when(
date %in% dates[period]  income_group == "low" ~ 
business_revenue * impact_factors$revenue[1],
date %in% dates[period]  income_group == "middle" ~ 
business_revenue * impact_factors$revenue[2],
date %in% dates代 写ECON1181 Mastering Data and Computation Individual ProjectC/C++
代做程序编程语言[period]  income_group == "high" ~ 
business_revenue * impact_factors$revenue[3],
TRUE ~ business_revenue
)
) %>%
ungroup()
}
impact_factors <- list(
spending = c(0.6, 0.7, 0.8), 
employment = c(0.7, 0.8, 0.9), 
revenue = c(0.5, 0.6, 0.7) 
)
data <- apply_impact(data, dates, impact_factors)
return(data)
} calculate_rolling_avg <- function(data, window_size =   7) {
data %>%
group_by(income_group) %>%
arrange(date) %>%
mutate(
spending_ma = rollmean(consumer_spending, k =   window_size, fill =   NA),
employment_ma = rollmean(employment_rate, k =   window_size, fill =   NA),
revenue_ma = rollmean(business_revenue, k =   window_size, fill =   NA)
) %>%
ungroup()
}
calculate_yoy_changes <- function(data) {
data %>%
group_by(income_group) %>%
arrange(date) %>%
mutate(
spending_yoy =   (consumer_spending / lag(consumer_spending, 365) - 1) * 100,
employment_yoy =   (employment_rate / lag(employment_rate, 365) - 1) * 100,
revenue_yoy =   (business_revenue / lag(business_revenue, 365) - 1) * 100
) %>%
ungroup()
}impact_factors <- list(
spending = c(0.6, 0.7, 0.8), 
employment = c(0.7, 0.8, 0.9), 
revenue = c(0.5, 0.6, 0.7)  )data <- apply_covid_impact(data, dates, impact_factors)
return(data)
}calculate_rolling_avg <- function(data, window_size =   7) {
data %>%
group_by(income_group) %>%
arrange(date) %>%
mutate(
spending_ma = rollmean(consumer_spending, k =   window_size, fill =   NA),
employment_ma = rollmean(employment_rate, k =   window_size, fill =   NA),
revenue_ma = rollmean(business_revenue, k =   window_size, fill =   NA)
) %>%
ungroup()
}calculate_yoy_changes <- function(data) {
data %>%
group_by(income_group) %>%
arrange(date) %>%
mutate(
spending_yoy =   (consumer_spending / lag(consumer_spending, 365) - 1) * 100,
employment_yoy =   (employment_rate / lag(employment_rate, 365) - 1) * 100,
revenue_yoy =   (business_revenue / lag(business_revenue, 365) - 1) * 100
) %>%
ungroup()
}plot_indicator <- function(data, indicator, title) {
ggplot(data, aes(x =   date, y = !!sym(indicator), color =   income_group)) +
geom_line() +
labs(
title =   title,
x =   "Date",
y =   "Index (Base = 100)",
color =   "Income Group"
) +
theme_minimal() +
scale_y_continuous(labels =   comma) +
scale_color_brewer(palette =   "Set1")
}plot_yoy_changes <- function(data, indicator, title) {
ggplot(data, aes(x =   date, y = !!sym(indicator), color =   income_group)) +
geom_line() +
geom_hline(yintercept =   0, linetype =   "dashed") +
labs(
title =   title,
x =   "Date",
y =   "Year-over-Year Change (%)",
color =   "Income Group"
) +
theme_minimal() +
scale_y_continuous(labels = percent_format(scale =   1)) +
scale_color_brewer(palette =   "Set1")
}
analyze_trends <- function(data) {
# Calculate summary statistics for pre and post COVID periods
covid_analysis <- data %>%
mutate(period = ifelse(date < as.Date("2020-03-15"), "Pre-COVID", "Post-COVID")) %>%
group_by(period, income_group) %>%
summarize(
avg_spending = mean(consumer_spending, na.rm =   TRUE),
avg_employment = mean(employment_rate, na.rm =   TRUE),
avg_revenue = mean(business_revenue, na.rm =   TRUE),
spending_volatility = sd(consumer_spending, na.rm =   TRUE),
employment_volatility = sd(employment_rate, na.rm =   TRUE),
revenue_volatility = sd(business_revenue, na.rm =   TRUE),
.groups =   "drop"
)
return(covid_analysis)
}generate_plots <- function(data) {
p1 <- plot_indicator(data, "spending_ma", "Consumer Spending During COVID-19")
p2 <- plot_indicator(data, "employment_ma", "Employment Rates During COVID-19")
p3 <- plot_indicator(data, "revenue_ma", "Business Revenue During COVID-19")
return(list(spending_plot =   p1, employment_plot =   p2, revenue_plot =   p3))
}run_analysis <- function() {
data <- create_sample_data()
data_processed <- data %>%
calculate_rolling_avg() %>%
calculate_yoy_changes()
trend_analysis <- analyze_trends(data_processed)
plots <- generate_plots(data_processed)
return(list(
data =   data_processed,
analysis =   trend_analysis,
plots =   plots
))
}2.   The website https://ourworldindata.org/ “Our World in Data”’s mission is to publish the “research and data to make progress against the world’s largest problems”. One of the major contemporary global health problem was the Covid-19 pandemic. I. It was a viral infection that affected almost everyone in some way or the other. However, the effects have been felt differently depending on various factors.(a)   Choose two countries which had been differently affected. Which countries did you choose and why? (10 marks)(b)   Choosing indicators on economic activity which of the countries was most affected economically and why. (30 marks)(c)   Using graphs and descriptive statistics you produced in parts a) and b), formulate a policy question on how to mitigate such a health emergency. (10 marks)(maximum 500 words)






         
加QQ：99515681  WX：codinghelp  Email: 99515681@qq.com
