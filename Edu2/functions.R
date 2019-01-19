complete_cleaning <- . %>%
	clean_names() %>%
	clean_na() %>%
	select_columns() %>%
	clean_conditions() %>%
	clean_clinical_signs() %>%
	clean_PCV1() %>%
	clean_USG1() %>%
	clean_diagnosis_age() %>%
	clean_outcome() %>%
	clean_bp() %>%
	mutate(UPCR1    = UPCR1    %>% clean_number()) %>%
	mutate(Crea_BQ1 = Crea_BQ1 %>% clean_number()) %>%
	mutate(Urea1    = Urea1    %>% clean_number()) %>%
	mutate(Phos1    = Phos1    %>% clean_number()) %>%
	mutate(Date_1 = Date_1 %>%  dmy()) %>%
	mutate(Date_of_death = Date_of_death %>%  dmy()) %>%
	mutate(protein = ifelse(UPCR1 < 0.2, "Normal", ifelse(UPCR1 < 0.4, "Borderline", "Proteinuria"))) %>%
	# mutate(protein = ifelse(is.na(protein), "Normal", protein)) %>%
	mutate(protein = factor(protein, levels = c("Normal", "Borderline", "Proteinuria"))) %>%
	# mutate(death = !is.na(Date_of_death)) %>%
	mutate(death = (Outcome == "dead")) %>%
	return()


clean_bp <-	. %>%
	mutate(BP1 = ifelse(BP1 == "HEMATURIA", NA, BP1)) %>%
	mutate(BP1 = as.numeric(BP1)) %>%
	as_tibble() %>%
	return()


trim <- function(x) {
	gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

select_columns <- function(raw_data) {
	raw_data <- raw_data %>% as.data.frame()
	raw_data <- raw_data[, 1:38]
	raw_data <- raw_data %>%
		select(Name, Breed, DOB, Sex, Neuter, CKD, Cause_CKD, Age_at_diagnosis_CKD, Outcome, Date_of_death, Date_1, Weight_1, UPCR1, Crea_BQ1, Crea_HCV1, Urea1, K1, Phos1, TP1, Alb_1, Biochem1, PCV1, CBC1, BP1, USG1, UA1, Clinical_signs1, Ttmt_1, Concurrent_conditions)
	return(raw_data)
}

clean_names <- function(raw_data) {
	x <- colnames(raw_data)
	x <- gsub(" ", "_", x)
	x <- gsub("\\?", "", x)
	colnames(raw_data) <- x
	return(raw_data)
}

clean_conditions <- function(df) {
	x <- df %>%
		.$Concurrent_conditions %>%
		tolower() %>%
		str_replace_all("fiv \\+", "fiv") %>%
		str_replace_all("fiv\\+", "fiv") %>%
		str_replace_all("\\-", ", ") %>%
		str_replace_all("\\?", ",") %>%
		str_replace_all("\\.", ",") %>%
		str_replace_all("and", ",") %>%
		str_replace_all(",,", ",") %>%
		str_replace_all(",,", ",") %>%
		str_replace_all(", ,", ",") %>%
		str_replace_all(",  ,", ",") %>%
		str_replace_all(" ,", ",") %>%
		str_replace_all("\n", ",") %>%
		str_replace_all("sucpected", "") %>%
		str_replace_all("suspected", "") %>%
		str_replace_all("chronic", "") %>%
		str_replace_all("cronic", "") %>%
		str_replace_all("later development of ", "") %>%
		str_replace_all("later development of ", "") %>%
		trim()

	max_results <- (str_count(x, ",")) %>% max(na.rm = T)
	df <- as.data.table(df)
	df <- df[, paste0("condition", 1:(max_results + 1)) := tstrsplit(x, ",")]
	df$Concurrent_conditions <- NULL
	df <- as.data.frame(df)
	# df[, "Concurrent_conditions" := NULL]
	return(df)
}

clean_PCV1 <- . %>%
		as_tibble() %>%
		mutate(PCV1 = ifelse(PCV1 == "FPLI POSITIVE", NA, PCV1)) %>%
		mutate(PCV1 = PCV1 %>% str_replace("\\%", "") %>% as.numeric()) %>%
		return()

clean_USG1 <- . %>%
		as_tibble() %>%
		mutate(USG1 = USG1 %>% str_replace("\\,", "") %>% as.numeric()) %>%
		return()

clean_number <- . %>%
	str_replace("\\<", "") %>%
	str_replace("\\>", "") %>%
	str_replace("\\,", ".") %>%
	str_replace("\\%", "") %>%
	as.numeric() %>%
	return()


clean_diagnosis_age <- function(raw_data){
	raw_data <- raw_data %>%
		as_tibble() %>%
		mutate(Age_at_diagnosis_CKD = Age_at_diagnosis_CKD %>% str_replace("\\?", ""))
	row_location <- which(grepl("months",raw_data$Age_at_diagnosis_CKD))
	raw_data$Age_at_diagnosis_CKD[[row_location]] %<>%
		str_replace("months", "") %>%
		trim()

	raw_data %<>% mutate(Age_at_diagnosis_CKD = as.numeric(Age_at_diagnosis_CKD))

	raw_data$Age_at_diagnosis_CKD[[row_location]] <- raw_data$Age_at_diagnosis_CKD[[row_location]] / 12
	return(raw_data)
}

clean_clinical_signs <- function(raw_data, threshold_importance = 5) {
	x <- raw_data %>%
		.$Clinical_signs1 %>%
		map_chr(tolower) %>%
		str_replace_all("pu", "polyuria") %>%
		str_replace_all("pd", "polydipsia") %>%
		str_replace_all("pf", "polyfagia") %>%
		str_replace_all("vomits", "vomiting") %>%
		str_replace_all("\\-", ", ") %>%
		str_replace_all("\\?", ",") %>%
		str_replace_all("\\.", ",") %>%
		str_replace_all("and", ",") %>%
		str_replace_all(",,", ",") %>%
		str_replace_all(",,", ",") %>%
		str_replace_all(", ,", ",") %>%
		str_replace_all(",  ,", ",") %>%
		str_replace_all(" ,", ",") %>%
		str_replace_all("detected on bloods for gingivitis", "gingivitis") %>%
		str_replace_all("crisis d'astma", "astma") %>%
		str_replace_all("slight", "") %>%
		str_replace_all("suspita ", "") %>%
		str_replace_all("despite good diabetic control", "") %>%
		str_replace_all("found on routine blood work", "") %>%
		str_replace_all("occasional ", "") %>%
		str_replace_all("acute ", "") %>%
		str_replace_all("chronic vomiting", "vomiting") %>%
		str_replace_all("mild chronic", "") %>%
		str_replace_all("v\\/d", "vomiting, diarrhea") %>%
		str_replace_all("chronic v", "vomiting") %>%
		str_replace_all("including gingivitis worsening", "gingivitis") %>%
		str_replace_all("lethargy 1d", "lethargy") %>%
		str_replace_all("weightloss", "weight loss") %>%
		str_replace_all("drinking more water", "polydipsia") %>%
		str_replace_all("vomiting due to intestinal fb", "none") %>%
		str_replace_all("weight loss", "weight_loss") %>%

		# Behaviour
		str_replace_all("lethargy", "behaviour") %>%
		str_replace_all("weird behaviour", "behaviour") %>%
		str_replace_all("excitement", "behaviour") %>%
		str_replace_all("discouraging", "behaviour") %>%
		str_replace_all("decayed", "behaviour") %>%

		# Reagrouping
		str_replace_all("polyuria", "pupd") %>%
		str_replace_all("polydipsia", "pupd") %>%
		trim()

	x[is.na(x)] <- "none"
	x[x == "<NA>"] <- "none"
	x[x == ""] <- "none"


	main_categories <<- strsplit(x, ",") %>%
		unlist() %>%
		trim() %>%
		table() %>%
		data.frame() %>%
		filter(Freq > threshold_importance) %>%
		.$. %>%
		as.vector()

	other_categories <- strsplit(x, ",") %>%
		unlist() %>%
		trim() %>%
		table() %>%
		data.frame() %>%
		filter(Freq <= threshold_importance) %>%
		.$. %>%
		as.vector() %>%
		paste(collapse = "|")

	x <- x %>%
		str_replace_all(other_categories, "others")

	main_categories <<- c(main_categories, "others")

	df <- matrix(nrow = length(x), ncol = length(main_categories)) %>%
		as_tibble()

	colnames(df) <- main_categories

	for (i in seq_along(x)) {
		for (j in seq_along(main_categories)) {
			df[i, j] <- grepl(main_categories[[j]], x[[i]])
		}
	}

	raw_data <- cbind(raw_data, df)

	return(raw_data)
}

clean_na <- function(raw_data) {
	raw_data[raw_data == ""] <- NA
	return(raw_data)
}

generate_ratio_ckd <- function(clean_data) {
	m <- clean_data %>%
		# filter(UPCR1 < 2) %>%
		filter(CKD %in% c("N", "Y")) %>%
		ggplot(aes(x = UPCR1, fill = CKD)) +
		geom_density(alpha = 0.75)

	p <- ggplot_build(m)

	no_ckd <- p %>%
		.$data %>%
		.[[1]] %>%
		filter(fill == "#F8766D") %>%
		.$density

	yes_ckd <- p %>%
		.$data %>%
		.[[1]] %>%
		filter(fill != "#F8766D") %>%
		.$density

	upc_expanded <- p %>%
		.$data %>%
		.[[1]] %>%
		filter(fill != "#F8766D") %>%
		.$x

	prop_ckd <- yes_ckd / (yes_ckd + no_ckd)

	return(list(prop_ckd = prop_ckd, upc_expanded = upc_expanded))
}


prevalence_ckd <- function(df){
	nrows <- length(df$CKD)
	max_upc <- max(df$UPCR1, na.rm=T)
	path_upc <- seq(from = 0, to = 1, by = 0.01)

	result <- c()

	for(i in seq_along(path_upc)){
		n_total <- df %>%
			# filter(UPCR1 <= 1) %>%
			filter(UPCR1 >= path_upc[[i]]) %>%
			.$UPCR1 %>%
			length()

		n_ckd <- df %>%
			# filter(UPCR1 <= 1) %>%
			filter(UPCR1 >= path_upc[[i]]) %>%
			filter(CKD == "Y") %>%
			.$UPCR1 %>%
			length()

		result[[i]] <- n_ckd / n_total
	}

	tibble(upc = path_upc, rate_ckd = result) %>%
		# filter(upc < 3) %>%
		ggplot(aes(x = upc, y = rate_ckd)) +
		geom_path()
}

clean_outcome <- function(raw_data){
	raw_data %>%
		as_tibble() %>%
		mutate(Outcome = tolower(Outcome)) %>%
		mutate(Outcome = ifelse(grepl("pts", Outcome), "dead", Outcome)) %>%
		mutate(Outcome = ifelse(grepl("died", Outcome), "dead", Outcome)) %>%
		mutate(Outcome = ifelse(grepl("euthanasia", Outcome), "dead", Outcome)) %>%
		mutate(Outcome = ifelse(grepl("no improvement", Outcome), "dead", Outcome)) %>%
		mutate(Outcome = ifelse(!grepl("dead", Outcome), "alive", Outcome)) %>%
		as_tibble() %>%
		return()
}


