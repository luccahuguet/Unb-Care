module Trabalho1.UnBCare where

-- Descomentar linha abaixo quando o import funcionar
import Trabalho1.ModeloDados

import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Maybe ()

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento mComp qComp [] = [(mComp, qComp)]
comprarMedicamento mComp qComp e
  | constaNoEstoque mComp e = somaNoEstoque mComp qComp e
  | otherwise = (mComp, qComp) : e

constaNoEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
constaNoEstoque _ [] = False
constaNoEstoque mComp ((mE, _) : tail)
  | mComp == mE = True
  | otherwise = constaNoEstoque mComp tail

temNoEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
temNoEstoque _ [] = False
temNoEstoque mComp ((mE, qE) : tail)
  | mComp == mE && qE > 0 = True
  | otherwise = temNoEstoque mComp tail

somaNoEstoque :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
somaNoEstoque _ _ [] = []
somaNoEstoque mComp qComp ((mE, qE) : tail)
  | mComp == mE = (mE, qE + qComp) : somaNoEstoque mComp qComp tail
  | otherwise = (mE, qE) : somaNoEstoque mComp qComp tail

{-
   QUESTÃO 2, VALOR: 1,0 pontoee

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Just []
tomarMedicamento mComp e
  | temNoEstoque mComp e = Just (decrementaEstoque mComp e)
  | otherwise = Nothing

decrementaEstoque :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
decrementaEstoque _ [] = []
decrementaEstoque mComp ((mE, qE) : tail)
  | mComp == mE = (mE, qE - 1) : decrementaEstoque mComp tail
  | otherwise = (mE, qE) : decrementaEstoque mComp tail

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento mComp ((mE, qE) : tail)
  | mComp == mE = qE
  | otherwise = consultarMedicamento mComp tail

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((mE, qList) : tail) = (mE, length qList) : demandaMedicamentos tail

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido r
  | recMedsOrdenados r && recMedsDistintos r && recHorsOrdenados r && recHorsDistintos r = True
  | otherwise = False

-- Funções de apoio para a função ReceituarioValido: begin

recMedsOrdenados :: Receituario -> Bool
recMedsOrdenados [] = True
recMedsOrdenados r
  | r == ordenarMedsRec r = True
  | otherwise = False

ordenarMedsRec :: Receituario -> Receituario
ordenarMedsRec [] = []
ordenarMedsRec r = sortBy (compare `on` fst) r

recMedsDistintos :: Receituario -> Bool
recMedsDistintos [] = True
recMedsDistintos ((mR, hList) : tail)
  | receituarioNumMedsIguais mR ((mR, hList) : tail) > 1 = False
  | otherwise = recMedsDistintos tail

receituarioNumMedsIguais :: Medicamento -> Receituario -> Int
receituarioNumMedsIguais _ [] = 0
receituarioNumMedsIguais mR r = length (filter ((== mR) . fst) r)

recHorsDistintos :: Receituario -> Bool
recHorsDistintos [] = True
recHorsDistintos ((_, hMed : hMedTail) : tail)
  | not (receituarioNumHorsIguais (hMed : hMedTail)) = False
  | otherwise = recHorsDistintos tail

receituarioNumHorsIguais :: [Horario] -> Bool
receituarioNumHorsIguais [] = True
receituarioNumHorsIguais (hMed : hMedTail)
  | length (filter (== hMed) (hMed : hMedTail)) > 1 = False
  | otherwise = receituarioNumHorsIguais hMedTail

recHorsOrdenados :: Receituario -> Bool
recHorsOrdenados [] = True
recHorsOrdenados r
  | r == ordenarHorsRec r = True
  | otherwise = False

ordenarHorsRec :: Receituario -> Receituario
ordenarHorsRec [] = []
ordenarHorsRec ((mR, hList) : tail) = (mR, sort hList) : ordenarHorsRec tail

-- Funções de apoio para a função ReceituarioValido: end

-- Funções de apoio para a função planoValido: begin

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido p
  | planoHorsOrdenado p && planoHorsDistintos p && planoMedsDistintos p && planoMedsOrdenados p = True
  | otherwise = False

planoHorsOrdenado :: PlanoMedicamento -> Bool
planoHorsOrdenado [] = True
planoHorsOrdenado p
  | p == ordenarHorsPlano p = True
  | otherwise = False

ordenarHorsPlano :: PlanoMedicamento -> PlanoMedicamento
ordenarHorsPlano [] = []
ordenarHorsPlano p = sortBy (compare `on` fst) p

planoHorsDistintos :: PlanoMedicamento -> Bool
planoHorsDistintos [] = True
planoHorsDistintos ((hP, mList) : tail)
  | planoNumHorsIguais hP ((hP, mList) : tail) > 1 = False
  | otherwise = planoHorsDistintos tail

planoNumHorsIguais :: Horario -> PlanoMedicamento -> Int
planoNumHorsIguais _ [] = 0
planoNumHorsIguais hP p = length (filter ((== hP) . fst) p)

planoMedsDistintos :: PlanoMedicamento -> Bool
planoMedsDistintos [] = True
planoMedsDistintos ((_, med : medTail) : tail)
  | not (planoNumMedsIguais (med : medTail)) = False
  | otherwise = planoMedsDistintos tail

planoNumMedsIguais :: [Medicamento] -> Bool
planoNumMedsIguais [] = True
planoNumMedsIguais (med : medTail)
  | length (filter (== med) (med : medTail)) > 1 = False
  | otherwise = planoNumMedsIguais medTail

planoMedsOrdenados :: PlanoMedicamento -> Bool
planoMedsOrdenados [] = True
planoMedsOrdenados p
  | p == ordenarMedsPlano p = True
  | otherwise = False

ordenarMedsPlano :: PlanoMedicamento -> PlanoMedicamento
ordenarMedsPlano [] = []
ordenarMedsPlano ((hP, medList) : tail) = (hP, sort medList) : ordenarMedsPlano tail

-- Funções de apoio para a função planoValido: end

{-

QUESTÃO 6  VALOR: 1,0 ponto,

  Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

  1. Os horários da lista são distintos e estão em ordem crescente; ok
  2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
  3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

  Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

-}

plantaoValido :: Plantao -> Bool
plantaoValido = undefined -- plantaoHorsOrdenados && plantaoHorsDistintos && plantaoSemConflito && plantaoMedicarOrdenados

-- ITEM 1: Beggining ------------------------------------------------------------------------------------------------

plantaoHorsOrdenados :: Plantao -> Bool
plantaoHorsOrdenados [] = True
plantaoHorsOrdenados [(h, c), (h2, c2)]
  | h >= h2 = False
  | otherwise = True
plantaoHorsOrdenados ((h, _) : (h2, c2) : pt)
  | h >= h2 = False
  | otherwise = plantaoHorsOrdenados ( (h2, c2) : pt)

{-
ordenarHorsPlantao :: Plantao -> Plantao
ordenarHorsPlantao [] = []
ordenarHorsPlantao plantao = sortBy (compare `on` fst) plantao
-}

plantaoHorsDistintos :: Plantao -> Bool
plantaoHorsDistintos [] = True
plantaoHorsDistintos ((hPlantao, cList) : tail)
  | plantaoNumHorsIguais hPlantao ((hPlantao, cList) : tail) > 1 = False
  | otherwise = plantaoHorsDistintos tail

plantaoNumHorsIguais :: Horario -> Plantao -> Int
plantaoNumHorsIguais _ [] = 0
plantaoNumHorsIguais hPlantao plantao = length (filter ((== hPlantao) . fst) plantao)

-- ITEM 1: End | ITEM 2: Beggining ------------------------------------------------------------------------------------------------

plantaoSemConflito :: Plantao -> Bool
plantaoSemConflito [] = True

plantaoSemConflito [(h, c), (h2, c2)]

-- ITEM 2: End | ITEM 3: Beggining  ------------------------------------------------------------------------------------------------

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario = undefined

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = undefined

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz = undefined

{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined
