

# Packages & Functionss ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(harrypotter)
library(gameofthrones)
library(ggridges)
library(magrittr)

library(randomForest)

theme_set(theme_bw())

source("functions.R")
source("bootstrap_functions.R")
source("rf_functions.R")

raw_data <- fread("upc.csv")

# Cleaning Data -----------------------------------------------------------

clean_data <- raw_data %>%
  complete_cleaning()


# UPC ---------------------------------------------------------------------
clean_data %>%
  group_by(protein) %>%
  summarise(
    n = n() / nrow(clean_data)
  ) %>%
  na.omit() %>%
  ggplot(aes(x = protein, y = n, fill = protein)) +
  geom_col(colour = "black") +
  scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("") +
  ylab("") +
  scale_fill_got(option = "Martell",
  							 discrete = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Proteinuria Levels")

clean_data %>%
  filter(UPCR1 < 1.5) %>%
  ggplot(aes(x = UPCR1, y = stat(width * density))) +
  geom_histogram(
    binwidth = 0.1,
    colour = "black",
    fill = got(10, option = "Tully")[[3]]
  ) +
  geom_vline(
    xintercept = 0.2,
    linetype = "dashed",
    colour = "orange",
    size = 0.8
  ) +
  annotate(
    "rect",
    ymin = 0,
    ymax = 0.25,
    xmin = 0.2,
    xmax = 0.4,
    alpha = 0.25,
    fill = "orange"
  ) +
  geom_vline(
    xintercept = 0.4,
    linetype = "dashed",
    colour = "red",
    size = 0.8
  ) +
  annotate(
    "rect",
    ymin = 0,
    ymax = 0.25,
    xmin = 0.4,
    xmax = 1.5,
    alpha = 0.1,
    fill = "red"
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 0.23,
    label = "High Proteinuria",
    colour = "red",
    size = 5
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 0.212,
    label = "> 0.4 ",
    colour = "red",
    size = 5
  ) +
  scale_y_continuous(labels = scales::percent) +
  xlab("UPC") +
  ylab("") +
  ggtitle("Frequency Histogram of UPC")





# UPC vs Sex --------------------------------------------------------------

# Investigate More, maybe Bootstrap testing
clean_data %>%
  filter(Sex != "H") %>%
  ggplot(aes(x = UPCR1, fill = Sex)) +
  geom_density(alpha = 0.7, position = "identity")

clean_data %>%
	filter(Sex != "H") %>%
	ggplot(aes(x = Sex, y = UPCR1, fill = Sex)) +
	geom_boxplot() +
	ylab("UPC") +
	xlab("Sex") +
	theme(legend.position = "none") +
	ggtitle("UPC vs Sex") +
	scale_y_continuous(limits = c(0,2))

bootstrap_test( x = clean_data[clean_data$Sex == "M", ]      %>% .$UPCR1,
								y = clean_data[clean_data$Sex == "F", ] %>% .$UPCR1)




# UPC vs Weight -----------------------------------------------------------

clean_data %>%
  filter(UPCR1 < 2) %>%
  ggplot(aes(x = UPCR1, y = Weight_1)) +
  geom_point() +
  geom_smooth() +
  xlab("UPC") +
  ylab("Weight (Kg)") +
  ggtitle("Relation between Weight and UPC")

clean_data %>%
  group_by(protein) %>%
  mutate(upc = mean(UPCR1) %>% sqrt() %>% sqrt()) %>%
  ggplot(aes(x = protein, y = Weight_1, fill = upc)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
  ylab("Weight (Kg)") +
  xlab("") +
  scale_fill_got(option = "Martell") +
  theme(legend.position = "none") +
  ggtitle("Weight vs Proteinuria")

bootstrap_test( x = clean_data[clean_data$protein == "Normal", ]      %>% .$Weight_1,
								y = clean_data[clean_data$protein == "Proteinuria", ] %>% .$Weight_1)


# UPC vs CKD --------------------------------------------------------------

clean_data %>%
  filter(UPCR1 < 2) %>%
  filter(CKD %in% c("N", "Y")) %>%
  ggplot(aes(x = CKD, y = UPCR1, fill = CKD)) +
  geom_boxplot(alpha = 0.75) +
  scale_fill_got(discrete = TRUE, option = "Tully") +
  theme(legend.position = "none") +
  xlab("CKD") +
  ylab("UPC") +
  ggtitle("Observed UPC on Patients with diagnosed CKD (or not)")

bootstrap_test( x = clean_data[clean_data$CKD == "Y", ]      %>% .$UPCR1,
								y = clean_data[clean_data$CKD == "N", ] %>% .$UPCR1)


prop_ckd <- generate_ratio_ckd(clean_data) %>% .$prop_ckd
upc_expanded <- generate_ratio_ckd(clean_data) %>% .$upc_expanded

tibble(
  prop_ckd = prop_ckd,
  upc_expanded = upc_expanded
) %>%
  filter(upc_expanded < 1) %>%
  ggplot(aes(x = upc_expanded, y = prop_ckd)) +
  geom_path(colour = hp(10, house = "Ravenclaw")[[1]], size = 1) +
  geom_vline(
    xintercept = 0.4,
    colour = got(10, option = "Martell")[[10]],
    linetype = "dashed"
  ) +
  xlab("Proeinuria (UPCR)") +
  ylab("") +
  ggtitle("Prevalence of CKD depending on UPCR") +
  scale_y_continuous(labels = scales::percent)


clean_data %>%
  filter(!is.na(CKD)) %>%
  filter(CKD != "?") %>%
  filter(!is.na(protein)) %>%
  group_by(protein, CKD) %>%
  summarise(n = n(), upc = mean(UPCR1) %>% sqrt()) %>%
  group_by(protein) %>%
  mutate(perc = n / sum(n), upc = mean(upc) %>% sqrt()) %>%
  filter(CKD == "Y") %>%
  ggplot(aes(x = protein, y = perc, fill = upc)) +
  geom_col(colour = "black") +
  scale_fill_got(option = "Martell") +
  scale_y_continuous(labels = scales::percent) +
  xlab("") +
  ylab("CKD Proportion") +
  ggtitle("Presence of CKD on different UPC levels.") +
  scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
  theme(legend.position = "none")

clean_data %>%
  select(Age_at_diagnosis_CKD) %>%
  mutate(Age_at_diagnosis_CKD = Age_at_diagnosis_CKD %>% as.numeric()) %>%
  na.omit() %>%
  ggplot(aes(x = Age_at_diagnosis_CKD, y = stat(width * density))) +
  geom_histogram(
    binwidth = 1.05,
    fill = hp(10, house = "Hufflepuff")[[9]],
    colour = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Age") +
  ylab("") +
  ggtitle("Age at CKD diagnosis")


# Age vs Proteinuria ---------------------------------------------------------------------

clean_data %>%
	ggplot(aes(x = protein, y = Age_at_diagnosis_CKD, fill = protein)) +
	geom_boxplot() +
	scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
	ylab("Age (Years)") +
	xlab("") +
	scale_fill_got(discrete = TRUE, option = "Martell") +
	theme(legend.position = "none") +
	ggtitle("Age vs Proteinuria")

bootstrap_test( x = clean_data[clean_data$protein == "Normal", ] %>% .$Age_at_diagnosis_CKD,
								y = clean_data[clean_data$protein != "Normal", ] %>% .$Age_at_diagnosis_CKD)

# Creatinin, Urea & UPC ---------------------------------------------------

clean_data %>%
  filter(UPCR1 < 1) %>%
  ggplot(aes(x = Crea_BQ1, y = UPCR1)) +
  geom_point()

clean_data %>%
  group_by(protein) %>%
  mutate(upc = mean(UPCR1) %>% sqrt() %>% sqrt()) %>%
  # filter(UPCR1 < 2) %>%
  ggplot(aes(x = protein, y = Crea_BQ1, fill = upc)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
  ylab("Creatinine") +
  xlab("") +
  scale_fill_got(option = "Martell") +
  theme(legend.position = "none") +
  ggtitle("Creatinine vs Proteinuria")

bootstrap_test( x = clean_data[clean_data$protein == "Normal", ]      %>% .$Crea_BQ1,
								y = clean_data[clean_data$protein == "Proteinuria", ] %>% .$Crea_BQ1)

clean_data %>%
  group_by(protein) %>%
  mutate(upc = mean(UPCR1) %>% sqrt() %>% sqrt()) %>%
  ggplot(aes(x = protein, y = Urea1, fill = upc)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
  ylab("Urea") +
  xlab("") +
  scale_fill_got(option = "Martell") +
  theme(legend.position = "none") +
  ggtitle("Urea vs Proteinuria")

bootstrap_test( x = clean_data[clean_data$protein == "Normal", ]      %>% .$Urea1,
								y = clean_data[clean_data$protein == "Proteinuria", ] %>% .$Urea1)


# Clinical Signs ----------------------------------------------------------

fn <- function(x) {
  return(x / sum(x))
}

clean_data %>%
  filter(!is.na(protein)) %>%
  group_by(protein) %>%
  select(!!c(main_categories)) %>%
  summarise_all(sum) %>%
  mutate_if(is.numeric, fn) %>%
  ungroup() %>%
  melt(id.vars = c("protein")) %>%
  group_by(variable, protein) %>%
  arrange(-value) %>%
  ungroup() %>%
  as_tibble() -> tmp

tmp$variable <- factor(tmp$variable, levels = (tmp %>% top_n(tmp$variable %>% unique() %>% length()) %>% .$variable))

tmp %>%
  ggplot(aes(x = variable, y = value, fill = protein)) +
  geom_col(
    colour = "black",
    position = "dodge"
  ) +
  scale_fill_got(option = "Martell", discrete = TRUE, name = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Clinical Sign") +
  ylab("") +
  ggtitle("Prevalence of Proteinuria Levels")

clean_data %>%
  filter(!is.na(protein)) %>%
  group_by(protein) %>%
  select(!!c(main_categories)) %>%
  summarise_all(sum) %>%
  mutate_if(is.numeric, fn) %>%
  ungroup() %>%
  as_tibble() ->
tmp

control <- tmp$none
for (c in colnames(tmp)) {
  if (is.numeric(tmp[[c]])) tmp[[c]] <- tmp[[c]] / control
}

tmp %>%
  melt(id.vars = "protein") %>%
  filter(protein == "Proteinuria") %>%
  ggplot(
    aes(
      x = reorder(variable, -value),
      y = value
    )
  ) +
  geom_col(
    colour = "black",
    fill = got(10, option = "Martell")[[4]]
  ) +
  xlab("Clinical Sign") +
  ylab("") +
  ggtitle("Relative Prevalence of Proteinuria with respect to the control") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


clean_data %>%
  select(!!c(main_categories, c("UPCR1"))) %>%
  rename(CONTROL = none) %>%
  melt(id.vars = "UPCR1") %>%
  mutate(control = (variable == "CONTROL")) %>%
  filter(value) %>%
  select(-value) %>%
  na.omit() %>%
  group_by(variable) %>%
  mutate(m = mean(UPCR1)) %>%
  ungroup(m) %>%
  arrange(-UPCR1) %>%
  ggplot(
    aes(
      x = reorder(variable, -m),
      y = UPCR1
    )
  ) +
  geom_boxplot(
    alpha = 0.8,
    aes(fill = control)
  ) +
  geom_hline(
    yintercept = 0.4,
    linetype = "dashed",
    colour = "red",
    size = 0.8
  ) +
  annotate(
    "rect",
    ymin = 0.4,
    ymax = 2,
    xmin = 0,
    xmax = 10,
    alpha = 0.1,
    fill = "red"
  ) +
  annotate(
    "text",
    x = 9.25,
    y = 1,
    label = "High Proteinuria",
    colour = "red",
    size = 5
  ) +
  xlab("Clinical Signs") +
  ylab("UPCR") +
  ggtitle("Measured UPC based on presented Clinical Signs") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  coord_flip() +
  scale_fill_hp(
    discrete = TRUE,
    house = "Ravenclaw",
    begin = 0.25,
    end = 0.65
  ) +
	scale_y_continuous(limits = c(0,2))

clean_data %>%
	select(!!c(main_categories, c("UPCR1"))) %>%
	rename(CONTROL = none) %>%
	melt(id.vars = "UPCR1") %>%
	mutate(control = (variable == "CONTROL")) ->
	clinical_signs

bootstrap_test(x = clinical_signs[clinical_signs$variable != "CONTROL" & clinical_signs$value == TRUE,] %>% .$UPCR1,
							 y = clinical_signs[clinical_signs$variable == "CONTROL"  & clinical_signs$value == TRUE,] %>%  .$UPCR1)

variables <- clinical_signs$variable %>% unique() %>% as.vector()
pvalues <- c()
for(i in seq_along(variables)){
	pvalues[[i]] <- bootstrap_test(x = clinical_signs[clinical_signs$variable == variables[[i]] & clinical_signs$value == TRUE,] %>% .$UPCR1,
																 y = clinical_signs[clinical_signs$variable == "CONTROL"  & clinical_signs$value == TRUE,] %>%  .$UPCR1)
}

col_scales <- c(hp(10)[[8]], hp(10, house = "Ravenclaw")[[1]])
tibble(variables, pvalue = pvalues) %>%
	filter(variables != "CONTROL") %>%
	mutate(colour = ifelse(pvalue < 0.05, hp(10)[[8]], hp(10, house = "Ravenclaw")[[1]])) %>%
	ggplot(aes(x = reorder(variables,pvalue), y = pvalue, fill = colour)) +
	geom_col(colour = "black") +
	geom_hline(yintercept = 0.05, linetype = "dashed", size = 0.85) +
	annotate(
		"rect",
		ymin = 0.05,
		ymax = 1,
		xmin = 0,
		xmax = length(pvalues) - 0.5,
		alpha = 0.25,
		fill = hp(10)[[8]]
	) +
	xlab("") +
	ylab("P-value") +
	scale_fill_manual(values = col_scales, aesthetics = "fill") +
	ggtitle("Results from Testing the difference between \n Clinical Signs and the Control Group") +
	theme(legend.position = "none") +
	theme(axis.text.x = element_text(angle = 30, hjust = 1))


clean_data %>%
	group_by(protein, none) %>%
	summarise(n = n()) %>%
	group_by(protein) %>%
	mutate(n = round(n/sum(n), 2)) %>%
	na.omit() %>%
	ggplot(aes(x = protein, y = n, fill = none)) +
	geom_col(position = "fill", colour = "black") +
	scale_y_continuous(labels = scales::percent) +
	# geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25, colour = got(10, option = "Daenerys")[[1]]) +
	xlab("") +
	ylab("") +
	scale_fill_got(discrete = TRUE, option = "Daenerys", name = "Presented any Sign?", labels = c("Yes", "No"), direction = - 1)



# Survival ----------------------------------------------------------------

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = Alb_1, fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Albumina") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
  facet_grid(. ~ proteinuria)

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = UPCR1, fill = death)) +
	geom_boxplot() +
	scale_x_discrete(limits = c("Normal", "Borderline", "Proteinuria")) +
	ylab("Weight (Kg)") +
	xlab("") +
	scale_fill_got(option = "Martell") +
	theme(legend.position = "none") +
	ggtitle("Weight vs Proteinuria")


clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = Urea1, fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Urea") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
  facet_grid(. ~ proteinuria)

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = K1, fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Potassium") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
  facet_grid(. ~ proteinuria)

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = as.numeric(Phos1), fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Phosphate") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
  facet_grid(. ~ proteinuria)

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = as.numeric(Crea_BQ1), fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Creatinina") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
  facet_grid(. ~ proteinuria)

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(UPCR1)) %>%
  ggplot(aes(x = as.numeric(Crea_BQ1), fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("UPC") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No"))

clean_data %>%
  select(death, Alb_1, Urea1, K1, Phos1, Crea_BQ1, UPCR1, Weight_1, protein) %>%
  mutate(proteinuria = ifelse(protein == "Proteinuria", "Proteinuria", "No Proteinuria")) %>%
  filter(!is.na(proteinuria)) %>%
  ggplot(aes(x = as.numeric(Weight_1), fill = death)) +
  geom_density(alpha = 0.75) +
  xlab("Weight (Kg)") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No"))

# Survival & UPCR1 --------------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = UPCR1, fill = death)) +
	geom_boxplot() +
	ylab("UPC") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("UPC and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$UPCR1,
								y = clean_data[!clean_data$death, ]     %>% .$UPCR1)


# Survival & Weight -------------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = Weight_1, fill = death)) +
	geom_boxplot() +
	ylab("Weight (Kg)") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Weight and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$Weight_1,
								y = clean_data[!clean_data$death, ]     %>% .$Weight_1)


# Survival & Albumina -----------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = Alb_1, fill = death)) +
	geom_boxplot() +
	ylab("Albumina") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Albumina and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$Alb_1,
								y = clean_data[!clean_data$death, ]     %>% .$Alb_1)



# Survival & Urea ---------------------------------------------------------


clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = Urea1, fill = death)) +
	geom_boxplot() +
	ylab("Urea") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Urea and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$Urea1,
								y = clean_data[!clean_data$death, ]     %>% .$Urea1)

# Survival & Potassium ----------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = K1, fill = death)) +
	geom_boxplot() +
	ylab("Potassium") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Potassium and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$K1,
								y = clean_data[!clean_data$death, ]     %>% .$K1)



# Survival & Phosphorus ---------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = Phos1, fill = death)) +
	geom_boxplot() +
	ylab("Phosphorus") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Phosphorus and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$Phos1,
								y = clean_data[!clean_data$death, ]     %>% .$Phos1)



# Survival & Creatinina ---------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = Crea_BQ1, fill = death)) +
	geom_boxplot() +
	ylab("Creatinina") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("Creatinina and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$Crea_BQ1,
								y = clean_data[!clean_data$death, ]     %>% .$Crea_BQ1)


# Survival & PCV ----------------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = PCV1, fill = death)) +
	geom_boxplot() +
	ylab("PCV") +
	xlab("Survived") +
	scale_fill_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("PCV and Mortality") +
	coord_flip()

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$PCV1,
								y = clean_data[!clean_data$death, ]     %>% .$PCV1)


# Survival & TP -----------------------------------------------------------

clean_data %>%
	group_by(protein) %>%
	ggplot(aes(x = death, y = TP1, colour = death)) +
	geom_boxplot(size = 1) +
	geom_jitter() +
	ylab("TP") +
	xlab("Survived") +
	scale_colour_hp(discrete = TRUE, house = "Slytherin", name = "Survived", labels = c("Yes", "No")) +
	scale_x_discrete(labels = c("Yes", "No")) +
	theme(legend.position = "none") +
	ggtitle("TP and Mortality")

bootstrap_test( x = clean_data[clean_data$death, ]      %>% .$TP1,
								y = clean_data[!clean_data$death, ]     %>% .$TP1)




# All survival tests ------------------------------------------------------

variables <- c("UPCR1", "PCV1", "Urea1", "K1", "Crea_BQ1", "Phos1", "Weight_1", "Alb_1", "TP1", "USG1", "Age_at_diagnosis_CKD", "BP1")

clean_data %>%
	select(noquote(c("death", variables))) %>%
	melt(id.vars = "death") %>%
	ggplot(aes(x = death, y = value, colour = death)) +
	geom_boxplot(size = 0.8) +
	# geom_jitter() +
	xlab("") +
	ylab("") +
	scale_x_discrete(labels = c("Survived", "Died")) +
	theme(legend.position = "none") +
	facet_wrap(.~variable, scales = "free") +
	scale_colour_got_d(option = "Tully")

pvalues <- c()
for(i in seq_along(variables)){
	pvalues[[i]] <- bootstrap_test( x = clean_data[clean_data$death, ][[variables[[i]]]],
									y = clean_data[!clean_data$death, ][[variables[[i]]]])
}

col_scales <- c(hp(10)[[8]], hp(10, house = "Ravenclaw")[[1]])
tibble(variables, pvalue = pvalues) %>%
	mutate(colour = ifelse(pvalue < 0.05, hp(10)[[8]], hp(10, house = "Ravenclaw")[[1]])) %>%
	ggplot(aes(x = reorder(variables,pvalue), y = pvalue, fill = colour)) +
	geom_col(colour = "black") +
	geom_hline(yintercept = 0.05, linetype = "dashed", size = 0.85) +
	annotate(
		"rect",
		ymin = 0.05,
		ymax = 0.7,
		xmin = 0,
		xmax = length(pvalues) + 0.5,
		alpha = 0.25,
		fill = hp(10)[[8]]
	) +
	xlab("") +
	ylab("P-value") +
	scale_fill_manual(values = col_scales, aesthetics = "fill") +
	ggtitle("Results from Hypothesis Testing") +
	theme(legend.position = "none") +
	theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Random Forest -----------------------------------------------------------

numerise_data <- function(data, numeric_columns){
	features <- colnames(data)

	data[is.na(data)] <- 0

	for(f in features) {
		if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
			levels <- unique(data[[f]]) %>% sort()
			data[[f]] <- (factor(data[[f]], levels=levels)) %>% as.numeric()
		}else{
			data[[f]] <- data[[f]] %>% as.numeric()
		}
	}
	data[is.na(data)] <- 0
	return(data)
}

rf_data <- clean_data %>% data_for_rf()


size <- nrow(rf_data)
all_ids <- 1:size
tr_ids <- all_ids %>% sample(size * 0.9)

train <- rf_data[tr_ids,]
test  <- rf_data[-tr_ids,]

rf_model <- randomForest(factor(Outcome) ~ ., data = train)

rf_model %>% plot_importance()


cv <- data.frame(prediction = predict(rf_model, test) %>% as.vector() %>% as.numeric(),
								 reality    = test$Outcome,
								 probability = predict(rf_model, test, type = "prob") %>% as.vector() %>% as.numeric())

caret::confusionMatrix(data      = cv$prediction %>% as.factor(),
											 reference = cv$reality    %>% as.factor())

pROC::roc(cv$reality, cv$prediction)




# Conditions --------------------------------------------------------------

clean_data %>%
	mutate(any_condition = ifelse(is.na(condition1), "No", "Yes")) %>%
	ggplot(aes(x = any_condition, y = UPCR1, fill = any_condition)) +
	geom_boxplot(size = 0.8) +
	scale_fill_got_d(option = "Margaery", begin = 0.3, end = 0.7) +
	theme(legend.position = "none") +
	ggtitle("UPC Levels Based on Previous Conditions") +
	xlab("Did they have previous conditions?") +
	ylab("UPC") +
	scale_y_continuous(limits = c(0,2))


# Treatments --------------------------------------------------------------

clean_data$Ttmt_1
