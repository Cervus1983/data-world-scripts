# upload file to data.world (you'll need your own account)
library(data.world)
set_config(cfg_env(auth_token_var = "DW_API_TOKEN"))

library(tidyverse)

# CSV file columns
columns <- c("rikishi1", "odds1", "rikishi2", "odds2", "ts")

# list CSV files that may contaon odds
list.files(
	path = "~/",
	pattern = "[0-9]{6}\\.csv$",
	full.names = TRUE,
	recursive = TRUE
) %>% 
	grep("[bet|sumo]", ., value = TRUE) %>% 
	# merge files of the right format
	sapply(
		function(fn) {
			df <- read_csv(fn)
			
			if (
				df %>% 
					names() %>% 
					setdiff(columns, .) %>% 
					length() %>% 
					`==`(0)
			) df %>% select(one_of(columns))
		},
		simplify = FALSE
	) %>% 
	do.call(rbind, .) %>% 
	unique() %>% 
	# filter in _changes_ in odds
	group_by(
		format(ts, "%Y-%m"),
		rikishi1, rikishi2
	) %>% 
	arrange(ts) %>% 
	mutate(
		odds1_lag = lag(odds1),
		odds1_lead = lead(odds1),
		odds2_lag = lag(odds2),
		odds2_lead = lead(odds2)
	) %>% 
	ungroup() %>% 
	filter(is.na(odds1_lag) | is.na(odds2_lag) | is.na(odds1_lead) | is.na(odds2_lead) | odds1 != odds1_lag | odds2 != odds2_lag) %>% 
	select(one_of(columns)) %>% 
	# upload to data.world
	upload_data_frame(
		dataset = paste(Sys.getenv("DW_USER"), "sumo-betting-odds", sep = "/"),
		data_frame = .,
		file_name = "marathonbet.csv"
	)
