##      A função rankall tem dois argumentos: um nome de resultado (outcome) e 
## uma classificação de hospital (num). A função lê o arquivo
## outcome-of-care-measures.csv e retorna um quadro de dados de 2 colunas
## contendo o hospital de cada estado que possui a classificação dada em num. 
## Por exemplo, rankall ("heart attack", "best") retorna um quadro de dados com
## os nomes dos hospitais que são os melhores em seus respectivos estados para 
## as taxas de mortalidade por ataque cardíaco em 30 dias. 
##      A função deve retornar um valor para cada estado (alguns podem ser NA). 
##      A primeira coluna no quadro de dados é chamada de hospital, que contém
## o nome do hospital, e a segunda coluna é chamada de estado, que contém a 
## abreviação de 2 caracteres para o nome do estado.
##      Hospitais que não possuem dados sobre um determinado resultado devem ser
## excluídos do conjunto de hospitais ao decidir as classificações.
##      Se existir um empate de hospitais no resultado, será exibido o primeiro
## em ordem alfabética.
##      Se for solicitado um Estado (state) inválido, a função retornará a
## mensagem "invalid state", e se um valor de resultado solicitado na função for
## inválido, a mensagem "invalid outcome" será exibida.

rankall <- function(outcome, num = "best") {
        
}