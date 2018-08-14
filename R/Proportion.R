#APERTURA ARCHIVO
readFile<- function(fileName) {

  data <- read.delim(fileName, stringsAsFactors = FALSE)

  return(data)
}

#CÁLCULO DE LAS PROPORCIONES DE HÁBITATS Y GRUPOS FUNCIONALES
pH <- function(data) {

  prop <- data %>% group_by(Name,Habitat) %>% summarize(n= n()) %>% mutate(propH=n/sum(n))

  return(prop)
}

tablapH<- function(pH){

  return(kable(pH, caption = "Habitat by Site"))
}

pFG <- function(data) {

  prop <- data %>% group_by(Name,FunctionalGroup) %>% summarize(n= n()) %>% mutate(propFG=n/sum(n))

  return(prop)
}

tablapFG <- function(pFG) {

  return(kable(pFG, caption = "Functional Group by Site"))
}


#GRÁFICO DE LAS PROPORCIONES DE HÁBITATS Y GRUPOS FUNCIONALES
plotpHabitat <- function(pH) {

  plot <- ggplot(pH, aes(x=factor(Habitat), y=propH, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}

plotpFG <- function(pFG) {

  plot <- ggplot(pFG, aes(x=factor(FunctionalGroup), y=propFG, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}
