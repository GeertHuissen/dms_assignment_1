# ============================================================
# DMS - Assignment
# ============================================================

# ----------------------------
# 0) Setup
# ----------------------------
library(tidyverse)
library(readxl)
library(scales)

# ----------------------------
# 1) Load + Prepare Data
# ----------------------------
data <- readxl::read_xlsx("A2_Happy_Tooth_data_2026.xlsx") %>%
	rename(
		id = Transaction_id,
		new_customer = New_customer,
		position = Position,
		channel = Channel,
		channel_position = Channel_position,
		time_to_convert_days = Time_to_Convert_Days,
		brand_keyword = Brand_keyword,
		sale = Sale
	)

# Quick check: variable names
print(names(data))

# ----------------------------
# QUESTION 1.1
# Channel distribution across channel positions
# ----------------------------

plot_q1_1 <- ggplot(data, aes(x = channel_position, fill = channel)) +
	geom_bar(position = "fill") +
	scale_y_continuous(labels = label_percent(accuracy = 1)) +
	labs(
		x = "Channel Position",
		y = "Distribution",
		fill = "Channel"
	) +
	theme_classic() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	scale_fill_brewer(palette = "Pastel1")

plot_q1_1

ggsave(
	filename = "plot_q1_1.png",
	plot = plot_q1_1,
	width = 8,
	height = 6,
	dpi = 300
)

# ----------------------------
# QUESTION 1.2
# Create first/last subsets + summary tables
# ----------------------------

data_first <- data %>% filter(channel_position == "First")
data_last  <- data %>% filter(channel_position == "Last")

# Summary tables (means + N) by channel
summary_first <- data_first %>%
	group_by(channel) %>%
	summarise(
		n = n(),
		avg_time_to_convert = mean(time_to_convert_days, na.rm = TRUE),
		avg_sales = mean(sale, na.rm = TRUE),
		.groups = "drop"
	)

summary_last <- data_last %>%
	group_by(channel) %>%
	summarise(
		n = n(),
		avg_time_to_convert = mean(time_to_convert_days, na.rm = TRUE),
		avg_sales = mean(sale, na.rm = TRUE),
		.groups = "drop"
	)

summary_first
summary_last

# Appendix A: Sales boxplots for First vs Last (stacked by channel-role)
df_fl <- bind_rows(
	data_first %>% mutate(touch_role = "First"),
	data_last  %>% mutate(touch_role = "Last")
) %>%
	mutate(channel_role = paste(touch_role, channel, sep = " - "))

# Force ordering: First-Channel then Last-Channel
df_fl$channel_role <- factor(
	df_fl$channel_role,
	levels = as.vector(rbind(
		paste("First", unique(df_fl$channel), sep = " - "),
		paste("Last",  unique(df_fl$channel), sep = " - ")
	))
)

appendix_a <- ggplot(df_fl, aes(x = sale, y = channel_role, fill = channel)) +
	geom_boxplot() +
	theme_minimal() +
	labs(x = "Sale", y = "Channel and Role")

appendix_a

ggsave(
	filename = "appendix_a.png",
	plot = appendix_a,
	width = 8,
	height = 6,
	dpi = 300
)

# Additional analysis: Journey length comparison when last channel is Affiliate vs Display
touchpoints_by_id <- data %>%
	group_by(id) %>%
	summarise(
		n_touchpoints = n(),
		last_channel = channel[channel_position == "Last"][1],
		.groups = "drop"
	)

avg_touchpoints_last <- touchpoints_by_id %>%
	filter(last_channel %in% c("Affiliate", "Display_advertising")) %>%
	group_by(last_channel) %>%
	summarise(
		n_transactions = n(),
		avg_touchpoints = mean(n_touchpoints),
		median_touchpoints = median(n_touchpoints),
		.groups = "drop"
	)

avg_touchpoints_last

# Additional analysis: Journey length by FIRST channel
journey_length <- data %>%
	group_by(id) %>%
	summarise(
		total_touchpoints = n(),
		first_channel = channel[channel_position == "First"][1],
		.groups = "drop"
	)

avg_touchpoints_first <- journey_length %>%
	group_by(first_channel) %>%
	summarise(
		n_transactions = n(),
		avg_touchpoints = mean(total_touchpoints),
		median_touchpoints = median(total_touchpoints),
		.groups = "drop"
	)

avg_touchpoints_first

# ----------------------------
# QUESTION 1.3
# Customer type: conversion time + sales + channel differences
# ----------------------------

# Conversion time per transaction = max(time_to_convert_days)
old_new_conversion_time <- data %>%
	group_by(id) %>%
	summarise(
		old_new = first(new_customer),
		conversion_time = max(time_to_convert_days, na.rm = TRUE),
		.groups = "drop"
	)

t_test_conversion <- t.test(conversion_time ~ old_new, data = old_new_conversion_time)
t_test_conversion

# Sales per transaction (assumes sale is constant within id)
old_new_sales <- data %>%
	group_by(id) %>%
	summarise(
		old_new = first(new_customer),
		sales = first(sale),
		.groups = "drop"
	)

t_test_sales <- t.test(sales ~ old_new, data = old_new_sales)
t_test_sales

# Channel distribution differences (First touchpoint): full and filtered
table_channels_first <- table(data_first$new_customer, data_first$channel)
table_channels_first

# Filter to sufficiently large channels (explicit selection)
table_channels_first_filtered <- table_channels_first[, c("Affiliate", "Display_advertising", "Search_engine")]
table_channels_first_filtered
chisq.test(table_channels_first_filtered)
prop.table(table_channels_first_filtered, margin = 1)

# Channel distribution differences (Last touchpoint)
table_channels_last <- table(data_last$new_customer, data_last$channel)
table_channels_last

table_channels_last_filtered <- table_channels_last[, c("Affiliate", "Display_advertising", "Search_engine")]
table_channels_last_filtered
chisq.test(table_channels_last_filtered)
prop.table(table_channels_last_filtered, margin = 1)

# ----------------------------
# QUESTION 2.1
# Attribution models: First-click, Last-click, 50/50 First-Last
# ----------------------------

# First-click: credit full sale to the first channel
first_click <- data %>%
	filter(channel_position == "First") %>%
	group_by(channel) %>%
	summarise(
		conversions = n(),
		total_sales = sum(sale, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(sales_share = total_sales / sum(total_sales, na.rm = TRUE))

first_click

# Last-click: credit full sale to the last channel
last_click <- data %>%
	filter(channel_position == "Last") %>%
	group_by(channel) %>%
	summarise(
		conversions = n(),
		total_sales = sum(sale, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(sales_share = total_sales / sum(total_sales, na.rm = TRUE))

last_click

# 50/50 model: credit half sale to first channel, half to last channel
even_click <- data %>%
	group_by(id) %>%
	summarise(
		first_channel = channel[channel_position == "First"][1],
		last_channel  = channel[channel_position == "Last"][1],
		sale = first(sale),
		.groups = "drop"
	)

even_attr <- bind_rows(
	even_click %>% transmute(channel = first_channel, sales = sale * 0.5),
	even_click %>% transmute(channel = last_channel,  sales = sale * 0.5)
)

even_result <- even_attr %>%
	group_by(channel) %>%
	summarise(
		total_sales = sum(sales, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(sales_share = total_sales / sum(total_sales, na.rm = TRUE))

even_result

# ----------------------------
# QUESTION 2.3
# Custom model (sales): time decay + fixed first-touch weight (25%)
# ----------------------------

half_life_sales <- 1
lambda_sales <- log(2) / half_life_sales

time_decay_sales <- data %>%
	group_by(id) %>%
	mutate(
		w_raw   = exp(-lambda_sales * time_to_convert_days),
		w_decay = w_raw / sum(w_raw, na.rm = TRUE),
		# 25% guaranteed to First touchpoint + 75% time-decay allocation
		w = 0.75 * w_decay + 0.25 * (channel_position == "First"),
		attributed_sales = w * first(sale)
	) %>%
	ungroup() %>%
	group_by(channel) %>%
	summarise(
		total_sales = sum(attributed_sales, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(sales_share = total_sales / sum(total_sales, na.rm = TRUE))

time_decay_sales

# ----------------------------
# QUESTION 2.4
# Custom model (acquisition): same weights as 2.3, but outcome = 1 per new customer
# ----------------------------

half_life_acq <- 1
lambda_acq <- log(2) / half_life_acq

time_decay_acq <- data %>%
	filter(new_customer == "Yes") %>%
	group_by(id) %>%
	mutate(
		conversion = 1,
		w_raw   = exp(-lambda_acq * time_to_convert_days),
		w_decay = w_raw / sum(w_raw, na.rm = TRUE),
		w = 0.75 * w_decay + 0.25 * (channel_position == "First"),
		attributed_acquisitions = w * first(conversion)
	) %>%
	ungroup() %>%
	group_by(channel) %>%
	summarise(
		total_acquisitions = sum(attributed_acquisitions, na.rm = TRUE),
		.groups = "drop"
	) %>%
	mutate(acquisition_share = total_acquisitions / sum(total_acquisitions, na.rm = TRUE))

time_decay_acq