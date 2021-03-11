module Trabalho1.Exemplos where

import Data.Maybe (isNothing)
import Trabalho1.ModeloDados
import Trabalho1.UnBCare

-- Declarações

m1 :: Medicamento
m1 = "Lactulona"

m2 :: Medicamento
m2 = "Pantoprazol"

m3 :: Medicamento
m3 = "Patz"

m4 :: Medicamento
m4 = "Quetiapina"

m5 :: Medicamento
m5 = "Mirtazapina"

m6 :: Medicamento
m6 = "Adera"

m7 :: Medicamento
m7 = "Donepezila"

m8 :: Medicamento
m8 = "Xarelto"

m9 :: Medicamento
m9 = "Alprazolam"

estoque1 :: EstoqueMedicamentos
estoque1 = [(m1, 10), (m2, 5), (m3, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(m1, 10), (m2, 5), (m3, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(m1, 10), (m2, 50), (m3, 10), (m4, 20)]

receituario1 :: Receituario
receituario1 = [(m1, [8, 17]), (m2, [6]), (m3, [22])]

receituario2 :: Receituario
receituario2 = [(m1, [8, 17]), (m2, [6]), (m3, [22]), (m4, [8, 22, 23])]

plano1 :: PlanoMedicamento
plano1 = [(6, [m2]), (8, [m1]), (17, [m1]), (22, [m3])]

plano2 :: PlanoMedicamento
plano2 = [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m3, m4]), (23, [m4])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1]),
    (17, [Medicar m1]),
    (22, [Medicar m3])
  ]

plantao2 :: Plantao
plantao2 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1]),
    (17, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]

plantaoValido0 :: Plantao
plantaoValido0 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (22, [Medicar m3]),
    (17, [Medicar m1, Comprar m3 30])
  ]

plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar m8, Medicar m2]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar m2]),
    (8, [Comprar m1 20, Medicar m1, Medicar m4]),
    (17, [Medicar m1]),
    (22, [Medicar m3, Medicar m4]),
    (23, [Medicar m4])
  ]

-- testando comprarMedicamento
casoTeste1 :: Bool
casoTeste1 = comprarMedicamento m3 30 estoque1 == [(m1, 10), (m2, 5), (m3, 30)]

casoTeste2 :: Bool
casoTeste2 = comprarMedicamento m6 20 estoque1 == [(m6, 20), (m1, 10), (m2, 5), (m3, 0)]

casoTeste3 :: Bool
casoTeste3 = comprarMedicamento m2 2 estoque1 == [(m1, 10), (m2, 7), (m3, 0)]

casoTeste4 :: Bool
casoTeste4 = comprarMedicamento m8 20 [] == [(m8, 20)]

conjuntoCasosTeste1 :: Bool
conjuntoCasosTeste1 = and [casoTeste1, casoTeste2, casoTeste3, casoTeste4]

--testando tomarMedicamento
casoTeste5 :: Bool
casoTeste5 = tomarMedicamento m1 estoque1 == Just [(m1, 9), (m2, 5), (m3, 0)]

casoTeste6 :: Bool
casoTeste6 = isNothing (tomarMedicamento m3 estoque1)

conjuntoCasosTeste2 :: Bool
conjuntoCasosTeste2 = casoTeste5 && casoTeste6

--testando consultarMedicamento
casoTeste7 :: Bool
casoTeste7 = consultarMedicamento m2 estoque1 == 5

casoTeste8 :: Bool
casoTeste8 = consultarMedicamento "Aas" estoque1 == 0

conjuntoCasosTeste3 :: Bool
conjuntoCasosTeste3 = casoTeste5 && casoTeste6

-- testando demandaMedicamentos
casoTeste9 :: Bool
casoTeste9 = demandaMedicamentos receituario1 == [(m1, 2), (m2, 1), (m3, 1)]

conjuntoCasosTeste4 :: Bool
conjuntoCasosTeste4 = and [casoTeste9]

--testando geraPlanoReceituario
casoTeste10 :: Bool
casoTeste10 = geraPlanoReceituario receituario1 == [(6, [m2]), (8, [m1]), (17, [m1]), (22, [m3])]

casoTeste11 :: Bool
casoTeste11 = geraPlanoReceituario receituario2 == [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m3, m4]), (23, [m4])]

conjuntoCasosTeste5 :: Bool
conjuntoCasosTeste5 = casoTeste10 && casoTeste11

-- testando geradores de plano e receituário
casoTeste12 :: Bool
casoTeste12 = geraReceituarioPlano (geraPlanoReceituario receituario1) == receituario1

casoTeste13 :: Bool
casoTeste13 = geraReceituarioPlano (geraPlanoReceituario receituario2) == receituario2

conjuntoCasosTeste6 :: Bool
conjuntoCasosTeste6 = casoTeste12 && casoTeste13

-- testando executaPlantao
casoTeste14 :: Bool
casoTeste14 = isNothing (executaPlantao plantao1 estoque1)

casoTeste15 :: Bool
casoTeste15 = executaPlantao plantao1 estoque2 == Just [(m1, 8), (m2, 4), (m3, 9)]

casoTeste16 :: Bool
casoTeste16 = executaPlantao plantao2 estoque1 == Just [(m1, 8), (m2, 4), (m3, 29)]

conjuntoCasosTeste7 :: Bool
conjuntoCasosTeste7 = casoTeste14 && casoTeste15 && casoTeste16

-- testando satisfaz
casoTeste17 :: Bool
casoTeste17 = not (satisfaz plantao1 plano1 estoque1)

casoTeste18 :: Bool
casoTeste18 = satisfaz plantao1 plano1 estoque2

casoTeste19 :: Bool
casoTeste19 = satisfaz plantao2 plano1 estoque1

conjuntoCasosTeste8 :: Bool
conjuntoCasosTeste8 = casoTeste17 && casoTeste18 && casoTeste19

-- testando plantaoCorreto
casoTeste21 :: Bool
casoTeste21 = satisfaz plantao plano1 estoque1
  where
    plantao = plantaoCorreto plano1 estoque1

conjuntoCasosTeste9 :: Bool
conjuntoCasosTeste9 = and [casoTeste21]

-- resultado global dos testes
resultadoGlobalTestes :: Bool
resultadoGlobalTestes =
  and
    [ conjuntoCasosTeste1,
      conjuntoCasosTeste2,
      conjuntoCasosTeste3,
      conjuntoCasosTeste4,
      conjuntoCasosTeste5,
      conjuntoCasosTeste6,
      conjuntoCasosTeste7,
      conjuntoCasosTeste8,
      conjuntoCasosTeste9,
      conjuntoCasosTestePlantaoValido
    ]

-- testando plantaoValido
conjuntoCasosTestePlantaoValido :: Bool
conjuntoCasosTestePlantaoValido =
  and
    [ plantaoValido plantao1,
      plantaoValido plantao2,
      plantaoValido plantaoValido0,
      not (plantaoValido plantaoInvalido1),
      not (plantaoValido plantaoInvalido2),
      not (plantaoValido plantaoInvalido3)
    ]


---- My Tests -----

---- Quase igual ao original kkk

-- Questão 1 -> conjuntoCasosTeste1
-- Questão 2 -> conjuntoCasosTeste2
-- Questão 3 -> conjuntoCasosTeste3
-- Questão 4 -> conjuntoCasosTeste4
-- Questão 5 -> conjuntoCasosTeste5' (receituarioValido e planoValido)
-- Questão 6 -> conjuntoCasosTeste6' (plantaoValido)
-- Questão 7 -> conjuntoCasosTeste5
-- Questão 8 -> conjuntoCasosTeste6
-- Questão 9 -> conjuntoCasosTeste7

-- Medicamento fora de ordem lexográfica
receituarioInvalido1 :: Receituario
receituarioInvalido1 = [(m2, [8, 17]), (m1, [6]), (m3, [22]), (m4, [8, 22, 23])]

-- Medicamento Repetido
receituarioInvalido2 :: Receituario
receituarioInvalido2 = [(m1, [8, 17]), (m2, [6]), (m2, [7]), (m4, [8, 22, 23])]

-- Horário fora de ordem crescente
receituarioInvalido3 :: Receituario
receituarioInvalido3 = [(m1, [8, 17]), (m2, [11, 7]), (m4, [8, 22, 23])]

-- Horário repetido
receituarioInvalido4 :: Receituario
receituarioInvalido4 = [(m1, [8, 17]), (m2, [6, 6]), (m4, [8, 22, 23])]

-- Horário fora de ordem crescente
plano3 :: PlanoMedicamento
plano3 = [(6, [m2]), (17, [m1]), (8, [m1]), (22, [m3])]

-- Horário repetido
plano4 :: PlanoMedicamento
plano4 = [(6, [m2]), (17, [m1]), (17, [m1]), (22, [m3])]

-- Medicamento fora de ordem lexográfica
plano5 :: PlanoMedicamento
plano5 = [(6, [m2]), (8, [m3, m1]), (17, [m1]), (22, [m3])]

-- Medicamento Repetido
plano6 :: PlanoMedicamento
plano6 = [(6, [m2]), (8, [m1, m1]), (17, [m1]), (22, [m3])]

planoValidoOK :: Bool
planoValidoOK =
  planoValido plano1 && planoValido plano2 && not (planoValido plano3)
    && not (planoValido plano4)
    && not (planoValido plano5)
    && not (planoValido plano6)

plantaoInvalido5 :: Plantao
plantaoInvalido5 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (8, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]


plantaoValidoOK :: Bool
plantaoValidoOK =
  plantaoValido plantao1 && plantaoValido plantao2 && plantaoValido plantaoValido0
    && not (plantaoValido plantaoInvalido1)
    && not (plantaoValido plantaoInvalido2)
    && not (plantaoValido plantaoInvalido3)
    && not (plantaoValido plantaoInvalido4)

receituarioValidoOK :: Bool
receituarioValidoOK =
  receituarioValido receituario1 && receituarioValido receituario2
    && not (receituarioValido receituarioInvalido1)
    && not (receituarioValido receituarioInvalido2)
    && not (receituarioValido receituarioInvalido3)
    && not (receituarioValido receituarioInvalido4)

conjuntoCasosTeste5' :: Bool
conjuntoCasosTeste5' = planoValidoOK && receituarioValidoOK

conjuntoCasosTeste6' :: Bool
conjuntoCasosTeste6' = plantaoValidoOK

-- plantaoInvalido1: Horário fora de ordem crescente
-- plantaoInvalido2: Conflito entre Medicar e comprar [Medicar m1, Comprar m1 30]
-- plantaoInvalido3: conflito entre medicar e comprar [Medicar m1, Comprar m1 30]
-- plantaoInvalido4: Conflito entre Medicar e comprar [Comprar m1 20, Medicar m1, Medicar m4]
-- plantaoInvalido4: Horários repetidos

-- Ordem lexicográfica: m6, m9, m7, m1, m5, m2, m3, m4, m8



