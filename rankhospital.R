##      A função chamada rankhospital tem três argumentos: o nome abreviado de 2
## caracteres de um estado (state), um resultado (outcome) e a classificação de 
## um hospital naquele estado para esse resultado (num). 
##      Ela lê o arquivo outcome-of-care-measures.csv e retorna um vetor de 
## caracteres com o nome do hospital que possui a classificação especificada 
## pelo argumento num. 
##      Por exemplo, a chamada rankhospital ("MD", "insuficiência cardíaca", 5)
## retornaria um vetor de caracteres contendo o nome do hospital com a 5ª menor 
## taxa de mortalidade em 30 dias por insuficiência cardíaca.
##      O argumento num pode assumir os valores “melhor”, “pior” ou um número 
## inteiro indicando a classificação (números menores são melhores). Se o número
## dado por num for maior que o número de hospitais naquele estado, a função 
## deve retornar NA. 
##      Os hospitais que não possuem dados sobre um determinado resultado devem 
## ser excluídos do conjunto de hospitais ao decidir as classificações.
##      Se existir um empate de hospitais no resultado, será exibido o primeiro
## em ordem alfabética.
##      Se for solicitado um Estado (state) inválido, a função retornará a
## mensagem "invalid state", e se um valor de resultado solicitado na função for
## inválido, a mensagem "invalid outcome" será exibida.

rankhospital <- function(state, outcome, num = "best" ) {
        ## ler o arquivo outcome
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        ## Verifica se Estado é válido, caso contrário retorna
        ## erro "Invalid State"
        if (state %in% data$State) {
                ## manter a base somente com os dados do Estado (state)
                data <- subset(data, 
                               State == state)
                lista <- as.character(0)
                
                ## Verifica se trata de Heart Attack
                if (outcome == "heart attack") {
                        ## converter variável de tx de heart attack [, 11] para 
                        ## números e marcar os casos que não são NA
                        ## manter apenas os casos que não são nulos NA's
                        data[, 11] <- as.numeric(data[, 11])
                        good <- complete.cases(data[, 11])
                        lista <- data[good, ]
                        
                        ## ordena os nomes por ordem alfabética
                        lista <- lista[order(lista[11]), ]
                        lista <- lista$Hospital.Name
                }
                else {
                        ## Verifica se trata de Heart Failure
                        if (outcome == "heart failure") {
                                ##      converte variável de tx de heart failure 
                                ## [, 17] para números e marcar os casos que
                                ## não são NA
                                ## manter apenas os casos que não são nulos NA
                                data[, 17] <- as.numeric(data[, 17])
                                good <- complete.cases(data[, 17])
                                lista <- data[good, ]
                                
                                ## ordena os nomes por ordem alfabética
                                lista <- lista[order(lista$Hospital.Name), ]
                                lista <- lista[order(lista[17]), ]
                                lista <- lista$Hospital.Name
                        }
                        else {
                                ## Verifica se trata de Pneumonia
                                if (outcome == "pneumonia") {
                                        ##      converte variável de tx de 
                                        ## pneumonia [, 23] para números e marca 
                                        ## os casos que não são NA
                                        ##      manter apenas os casos que não 
                                        ## são nulos NA's
                                        data[, 23] <- as.numeric(data[, 23])
                                        good <- complete.cases(
                                                as.numeric(data[, 23]))
                                        lista <- data[good, ]
                                        
                                        ## ordena os nomes por ordem alfabética
                                        lista <- lista[order(lista[23]), ]
                                        lista <- lista$Hospital.Name
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
        
        ## calculo final da função se estado e outcome são válidos
        if (length(lista)>1) {
                ## seleciona o hospital na posição do argumento num
                if (num == "best") {
                        resultado <- lista[[1]]
                }
                else {
                        if (num == "worst"){
                                resultado <- lista[[length(lista)]]
                        }
                        else{
                                if (num <= length(lista)) {
                                        resultado <- lista[[num]]
                                }
                                else {
                                        resultado <- "NA"
                                }
                        }
                }
        }
        
        ## exibe o resultado final da função
        resultado
        
}