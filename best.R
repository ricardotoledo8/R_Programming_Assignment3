##      A função Best tem dois argumentos: Estado (state) e Resultado (outcome).
## Ela lê o arquivo outcome-of-care-measures.csv e retorna um vector de caracteres
## com o nome do hospital que tem a melhor taxa de mortalidade em 30 dias. O nome
## do hospital é a variável Hospital.Name, e os resultados somente podem ser:
## "heart attack", "heart failure" ou "pneumonia". Os hospitais que não possuem
## dados são excluídos dos resultados.
##      Se existir um empate de hospitais no resultado, será exibido o primeiro
## em ordem alfabética.
##      Se for solicitado um Estado (state) inválido, a função retornará a
## mensagem "invalid state", e se um valor de resultado solicitado na função for
## inválido, a mensagem "invalid outcome" será exibida.


best <- function(state, outcome) {
        ## ler o arquivo outcome
        data <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        
        ## Verifica se Estado é válido, caso contrário retorna
        ## erro "Invalid State"
        if (state %in% data$State) {
                ## manter a base somente com os dados do Estado (state)
                data <- subset(data, State == state)
                
                ## Verifica se trata de Heart Attack
                if (outcome == "heart attack") {
                        ## converter variável de tx de heart attack [, 11] para 
                        ## números e marcar os casos que não são NA
                        data[, 11] <- as.numeric(data[, 11])
                        good <- complete.cases(as.numeric(data[, 11]))
                        
                        ## manter apenas os casos que não são nulos NA's
                        lista <- data[good, ]
                        
                        ## calcular o valor mínimo de heart attack
                        min_value <- min(lista[, 11])
                        
                        ## seleciona os Hospitais com mínimo valor
                        hospitais_ha <- subset(data, data[11] <= min_value, 
                                            select = Hospital.Name)
                        
                        ## ordena os nomes por ordem alfabética
                        hospitais_ha <- sort(hospitais_ha$Hospital.Name)
                        
                        ## seleciona o primeiro da lista
                        resultado <- hospitais_ha[[1]]
                }
                
                else {
                        ## Verifica se trata de Heart Failure
                        if (outcome == "heart failure") {
                                ##      converte variável de tx de heart failure 
                                ## [, 17] para números e marcar os casos que
                                ## não são NA
                                data[, 17] <- as.numeric(data[, 17])
                                good <- complete.cases(as.numeric(data[, 17]))
                                
                                ## manter apenas os casos que não são nulos NA's
                                lista <- data[good, ]
                                
                                ## calcular o valor mínimo de heart failure
                                min_value <- min(lista[, 17])
                                
                                ## seleciona os Hospitais com mínimo valor
                                hospitais_hf <- subset(data, 
                                                       data[17] <= min_value, 
                                                    select = Hospital.Name)
                                
                                ## ordena os nomes por ordem alfabética
                                hospitais_hf <- sort(hospitais_hf$Hospital.Name)
                                
                                ## seleciona o primeiro da lista
                                resultado <- hospitais_hf[[1]]
                           
                        }
                        else {
                                ## Verifica se trata de Pneumonia
                                if (outcome == "pneumonia") {
                                        ##      converte variável de tx de 
                                        ## pneumonia [, 23] para números e marca 
                                        ## os casos que não são NA
                                        data[, 23] <- as.numeric(data[, 23])
                                        good <- complete.cases(
                                                as.numeric(data[, 23]))
                                        
                                        ##      manter apenas os casos que não 
                                        ## são nulos NA's
                                        lista <- data[good, ]
                                        
                                        ## calcular o valor mínimo de pneumonia
                                        min_value <- min(lista[, 23])
                                        
                                        ## seleciona os Hospitais com mín valor
                                        hospitais_pn <- subset(
                                                data, 
                                                data[23] <= min_value, 
                                                select = Hospital.Name)
                                        
                                        ## ordena os nomes por ordem alfabética
                                        hospitais_pn <- sort(
                                                hospitais_pn$Hospital.Name)
                                        
                                        ## seleciona o primeiro da lista
                                        resultado <- hospitais_pn[[1]]
                                
                                }
                                else {
                                        ## Não é Outcome válido
                                        resultado <- "Invalid Outcome"
                                }
                        }
                }
        }
        else { ## retorna o Estado Inválido
                resultado <- "Invalid State"
        }
        
        ## exibe o resultado da função
        resultado
        
}