#' APERTURA ARCHIVO

#' Lee archivos que contengan el habitat y grupo funcional de las especies de uno o varios ecosistemas en formato .csv o separados por tabulaciones en columnas
#'
#' @param fileName un archivo con el habitat y grupo funcional de cada especie
#'
#' @return un data frame
#' @export
#'
#' @examples
#'
#'# Lee archivo con ecosistema, habitat y grupo funcional separado en columnas por tabulaciones
#'
#'fileName <- system.file("exdata", "traits.txt", package = "Proporciones")
#'tabla <- readFile(fileName)

readFile<- function(fileName) {

  data <- read.delim(fileName, stringsAsFactors = FALSE)

  return(data)
}

#' CÁLCULO DEL TOTAL DE ESPECIES Y PROPORCIONES POR HÁBITATS Y GRUPOS FUNCIONALES

#' @param data un data frame
#'
#' @return una tabla con los siguientes campos:
#'         Name: nombre del ecosistema
#'         Habitat/FunctionalGroup: categorias dentro de habitat/grupo funcional
#'         n: total de especies por categoría
#'         propH/propFG: proporción de especies por categoria
#'
#' @export
#' @importFrom dplyr    group_by %>% summarise mutate
#'
#' @examples
#'
#' Habitat <- pH(tabla)
#' FunctionalGroup <- pFG(tabla)

#' PARA HÁBITATS
pH <- function(data) {

  prop <- data %>% group_by(Name,Habitat) %>% summarize(n= n()) %>% mutate(propH=n/sum(n))

  return(prop)
}

#' PARA GRUPOS FUNCIONALES
pFG <- function(data) {

  prop <- data %>% group_by(Name,FunctionalGroup) %>% summarize(n= n()) %>% mutate(propFG=n/sum(n))

  return(prop)
}


#'GRÁFICO DE LAS PROPORCIONES DE HÁBITATS Y GRUPOS FUNCIONALES

#'@param pH un data frame
#'@param pFG un data frame
#'
#' @return un gráfico
#'
#' @export
#' @importFrom ggplot2    ggplot geom_bar theme_bw scale_fill_brewer
#'
#' @examples
#'
#' plotpHabitat(Habitat)
#' plotpFG(FunctionalGroup)

#' PARA HÁBITATS
plotpHabitat <- function(pH) {

  plot <- ggplot(pH, aes(x=factor(Habitat), y=propH, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}

#' PARA GRUPOS FUNCIONALES
plotpFG <- function(pFG) {

  plot <- ggplot(pFG, aes(x=factor(FunctionalGroup), y=propFG, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}
