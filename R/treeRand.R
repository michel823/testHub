#' Arbre decision sur une matrice aleatoire
#'
#' @param nbLig nombre de lignes
#' @param nbCol nombre de colonnes
#' @return Le resume de modele arbre decisin sur \code{nbLig} ligne et \code{nbCol} colonne
#' @examples
#' \dontrun{
#' treeFunKeyrus(10, 2)
#' }

treeFunKeyrus<-function(nbLig,nbCol){
	myData = data.frame(y=sample(c(0,1),size=nbLig, replace = TRUE),matrix(rnorm(nbLig*nbCol) , ncol=nbCol))
	m = rpart(y~.,data=myData)
	return(summary(m))
}