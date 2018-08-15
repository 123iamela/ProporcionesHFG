#' Lectura archivo datos
#'
#' Lee archivos que contengan el habitat y grupo funcional de las especies de uno o varios ecosistemas en formato .csv o separados por tabulaciones en columnas
#'
#' @param fileName un archivo con el habitat y grupo funcional de cada especie
#'
#' @return un data frame
#' @export
#'
#' @examples
#'# Lee archivo con ecosistema, habitat y grupo funcional separado en columnas por tabulaciones
#'
#'fileName <- system.file("exdata", "traits.txt", package = "Proporciones")
#'tabla <- readFile(fileName)

readFile<- function(fileName) {

  data <- read.delim(fileName, stringsAsFactors = FALSE)

  return(data)
}

#' Total de especies y proporciones por habitat
#'
#' @param data un data frame
#'
#' @return una tabla con los siguientes campos:
#'         Name: nombre del ecosistema
#'         Habitat: categorias dentro de habitat/grupo funcional
#'         n: total de especies por categoría
#'         propH: proporción de especies por categoria
#'
#' @export
#' @importFrom dplyr    group_by %>% summarise mutate
#'
#' @examples
#' Habitat <- proporcionH(tabla)

proporcionH <- function(data) {

  prop <- data %>% group_by(Name,Habitat) %>% summarize(n= n()) %>% mutate(propH=n/sum(n))

  return(prop)
}

#' Total de especies y proporciones por grupo funcional
#'
#' @param data un data frame
#'
#' @return una tabla con los siguientes campos:
#'         Name: nombre del ecosistema
#'         FunctionalGroup: categorias dentro de habitat/grupo funcional
#'         n: total de especies por categoría
#'         propFG: proporción de especies por categoria
#'
#' @export
#' @importFrom dplyr    group_by %>% summarise mutate
#'
#' @examples
#' FunctionalGroup <- proporcionFG(tabla)

proporcionFG <- function(data) {

  prop <- data %>% group_by(Name,FunctionalGroup) %>% summarize(n= n()) %>% mutate(propFG=n/sum(n))

  return(prop)
}


#'Grafico de las proporciones de habitat
#'
#'@param proporcionH un data frame
#'
#' @return un gráfico
#'
#' @export
#' @importFrom ggplot2    ggplot geom_bar theme_bw scale_fill_brewer
#'
#' @examples
#' plotpHabitat(Habitat)

plotpHabitat <- function(proporcionH) {

  plot <- ggplot(proporcionH, aes(x=factor(Habitat), y=propH, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}

#'Grafico de las proporciones de grupos funcionales
#'
#'@param proporcionFG un data frame
#'
#' @return un gráfico
#'
#' @export
#' @importFrom ggplot2    ggplot geom_bar theme_bw scale_fill_brewer
#'
#' @examples
#' plotpFG(FunctionalGroup)

plotpFG <- function(proporcionFG) {

  plot <- ggplot(proporcionFG, aes(x=factor(FunctionalGroup), y=propFG, fill=Name)) + geom_bar(stat="identity", position=position_dodge()) + theme_bw() + scale_fill_brewer(palette = "Pastel1")

  return(plot)
}
