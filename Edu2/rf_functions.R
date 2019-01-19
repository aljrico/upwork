data_for_rf <- . %>%
	select(Outcome, Age_at_diagnosis_CKD, Weight_1, UPCR1, Crea_BQ1, Urea1, K1, Phos1, TP1, Alb_1, PCV1, USG1) %>%
	mutate_if(is.logical, as.numeric) %>%
	replace(is.na(.), 0) %>%
	mutate(Outcome = (Outcome %>% as.factor() %>% as.numeric()) - 1) %>%
	numerise_data() %>%
	as_tibble() %>%
	return()

plot_importance <- function(rf_model){
	rf_model %>%
		importance() %>%
		as.data.frame() %>%
		rownames_to_column() %>%
		rename(Variables = rowname) %>%
		rename(Importance = MeanDecreaseGini) %>%
		as_tibble() %>%
		ggplot(aes(x = reorder(Variables, Importance),
							 y = Importance, fill = Importance)) +
		geom_bar(stat='identity') +
		labs(x = 'Variables') +
		coord_flip() +
		scale_fill_got(option = "wildfire", begin = 0.1, end = 0.8) +
		theme(legend.position = "none") ->
		gg

	return(gg)
}
