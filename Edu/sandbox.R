
# Libraries & Custom Functions --------------------------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(harrypotter)
library(gridExtra)
library(scales)


get_numbers <- function(y){
	matches <- regmatches(y, gregexpr("[[:digit:]]+", y))
	for(i in 1:length(matches)){
		matches[[i]] <- paste(matches[[i]], collapse = "")
	}
	return(as.numeric(unlist(matches)))
}

get_bacteri <- function(y){
	y <- gsub("[[:digit:]]+", "", y)
	y <- gsub("\\.", "", y)
	y <- gsub("\\,", "", y)
	y <- gsub("[{}]", "", y)
	y <- gsub("\\s*\\([^\\)]+\\)", "", y)
	y <- str_trim(y)
	y[which(word(y,1)=="r")] <- gsub("[[:alpha:]]+", "", y[which(word(y,1)=="r")])
	y <- str_trim(y)
	y <- ifelse(y == "", "NA", y)
	for(k in 1:length(y)) {
		if(grepl("Arial", y[[k]])) y[[k]] <- "NA"
		if(grepl("contam", y[[k]])) y[[k]] <- "contaminated"
		if(grepl("CONTAM", y[[k]])) y[[k]] <- "contaminated"
	}
	return(y)

	grepl(y, "contam")
}


# Retrieve Data -----------------------------------------------------------

df <- fread("Edu/data/general.csv")

# There is a duplicate
df$ID %>% unique() %>% length()
df$ID %>% length()

duplicated_id <- df %>%
	group_by(ID) %>%
	summarise(n = n()) %>%
	filter(n > 1) %>%
	.$ID

df[ID == duplicated_id,]

df$MUESTRA %>% length()
df$MUESTRA %>% unique() %>% length()

# Preprocessing -----------------------------------------------------------

df[, FECHA := dmy(FECHA)]
df[, CP    := ifelse(CP == "#N/A", NA, CP)]
df[, EDAD  := ifelse(EDAD == "NC", NA, as.numeric(EDAD))]
df[, animal := ifelse(ESPECIES == 1, "dog", "cat")]


x <- df$RESULTADO
x <- gsub("UFC/ml", "", x)
x <- gsub("UFC/m", "", x)
x <- gsub(">", "", x)
y <- gsub("\\n", ",", x)
y <- gsub(",,", ",",y)
y <- gsub(",,", ",",y)
y <- gsub(",,", ",",y)
y <- gsub(",,", ",",y)
y <- gsub(",,", ",",y)
y <- gsub(",,", ",",y)
df$RESULTADO <- y
max_resultados <- (str_count(y , ",")) %>% max()
setDT(df)[, paste0("resultado", 1:max_resultados) := tstrsplit(RESULTADO, ",")]

for(j in 1:max_resultados){
	df[[paste0("conc_resultado",j)]] <- get_numbers(df[[paste0("resultado",j)]])
	df[[paste0("bacteri_resultado",j)]] <- get_bacteri(df[[paste0("resultado",j)]])
	# df[[paste0("bacteri_resultado",j)]] <- ifelse(df[[paste0("bacteri_resultado",j)]] == "", NA, df[[paste0("bacteri_resultado",j)]])
	df[[paste0("resultado",j)]] <- NULL
}

df$`RESULTADO` <- NULL
df$`ESPECIE 1` <- NULL
df$`ESPECIE 2` <- NULL
df$`ESPECIE 3` <- NULL
df$CRECIMIENTO <- NULL



# Visualizations ----------------------------------------------------------

df$bacteri_resultado1 %>%
	table() %>%
	as_tibble() %>%
	set_names("bacteri","n") %>%
	filter(bacteri != "NA") %>%
	arrange(-n) %>%
	top_n(10) %>%
	ggplot(aes(x = reorder(bacteri,-n), y = n)) +
	geom_col(colour = "black", fill = hp(10, house = "Slytherin")[[9]]) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Bacteri") +
	ylab("N")

gg1 <- df %>%
	group_by(bacteri_resultado1,animal) %>%
	summarise(n = n()) %>%
	rename(bacteri = bacteri_resultado1) %>%
	group_by(bacteri) %>%
	mutate(n_total = sum(n)) %>%
	ungroup() %>%
	arrange(-n_total) %>%
	top_n(20,n_total) %>%
	ggplot(aes(x = reorder(bacteri,-n), y = n, fill = animal)) +
	geom_col(colour = "black") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Bacteri") +
	ylab("N")+
	scale_fill_hp(discrete = TRUE, house = "Ravenclaw")

gg2 <- df %>%
	group_by(bacteri_resultado1,animal) %>%
	summarise(n = n()) %>%
	rename(bacteri = bacteri_resultado1) %>%
	group_by(bacteri) %>%
	mutate(n_total = sum(n)) %>%
	ungroup() %>%
	arrange(-n_total) %>%
	top_n(20,n_total) %>%
	ggplot(aes(x = reorder(bacteri,-n), y = n, fill = animal)) +
	geom_col(colour = "black", position="fill") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Bacteri") +
	ylab("Proportion") +
	scale_fill_hp(discrete = TRUE, house = "Ravenclaw")

gg1
gg2


df %>%
	group_by(bacteri_resultado1,ESTACIÓN) %>%
	summarise(n = n()) %>%
	rename(bacteri = bacteri_resultado1) %>%
	filter(bacteri != "NA") %>%
	group_by(bacteri) %>%
	mutate(n_total = sum(n)) %>%
	ungroup() %>%
	arrange(-n_total) %>%
	top_n(20,n_total) %>%
	ggplot(aes(x = reorder(ESTACIÓN,-n), y = n, fill = bacteri)) +
	geom_col(colour = "black") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Bacteri") +
	ylab("N")

df %>%
	group_by(bacteri_resultado1,ESTACIÓN) %>%
	summarise(n = n()) %>%
	rename(bacteri = bacteri_resultado1) %>%
	filter(bacteri != "NA") %>%
	group_by(bacteri) %>%
	mutate(n_total = sum(n)) %>%
	ungroup() %>%
	arrange(-n_total) %>%
	top_n(20,n_total) %>%
	ggplot(aes(x = reorder(ESTACIÓN,-n), y = n, fill = bacteri)) +
	geom_col(colour = "black", position="fill") +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Bacteri") +
	ylab("Proportion")


df %>%
	group_by(animal,ESTACIÓN) %>%
	summarise(prop = mean(`POS/NEG`)) %>%
	ggplot(aes(x = ESTACIÓN, y = prop, fill = animal)) +
	geom_col(position = "dodge") +
	xlab("Season") +
	ylab("Proportion")

df %>%
	mutate(month = month(FECHA)) %>%
	group_by(animal,month) %>%
	summarise(prop = mean(`POS/NEG`)) %>%
	ungroup() %>%
	ggplot(aes(x = (month), y = prop, colour = animal)) +
	geom_path(size = 1) +
	xlab("Month") +
	ylab("Proportion") +
	scale_colour_hp(discrete = TRUE, house = "Ravenclaw") +
	scale_x_continuous() +
	scale_x_continuous(breaks = scales::pretty_breaks(n = 12))
