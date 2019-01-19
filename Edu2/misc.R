
communicate_result <- function(pvalue){
	if(pvalue < 0.05){
		interpretation <- "El que ens diu que la diferencia és estadísticament significativa."
	}else{
		interpretation <- "Per tant la diferencia NO és estadísticament significativa."
	}

	msg1 <- paste0("El resultat del test estadístic surt amb un p-value de ", pvalue,". ")
	msg2 <- paste0(interpretation)

	list(msg1 = msg1, msg2 = msg2) %>% return()
}
