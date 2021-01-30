module Exemplos where

import ModeloDados
    ( Cuidado(Comprar, Medicar),
      Plantao,
      PlanoMedicamento,
      Receituario,
      EstoqueMedicamentos,
      Medicamento )

import UnBCare
    ( comprarMedicamento,
      tomarMedicamento,
      consultarMedicamento,
      demandaMedicamentos,
      plantaoValido,
      geraPlanoReceituario,
      geraReceituarioPlano,
      executaPlantao,
      satisfaz,
      plantaoCorreto )

import Data.Maybe (isNothing)

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

-- Receituario válido
receituario1 :: Receituario
receituario1 = [(m1, [8, 17]), (m2, [6]), (m3, [22])]

-- TODO: Destrocar P 1 e 2 (med trocado)
receituario2 :: Receituario
receituario2 = [(m2, [8, 17]), (m1, [6]), (m3, [22]), (m4, [8, 22, 23])]

-- TODO: deletar P 3 depois (med repetido)
receituario3 :: Receituario
receituario3 = [(m1, [8, 17]), (m2, [6]), (m2, [7]), (m4, [8, 22, 23])]

-- TODO: deletar (hor trocado)
receituario4 :: Receituario
receituario4 = [(m1, [8, 17]), (m2, [11, 7]), (m4, [8, 22, 23])]

-- TODO: deletar (hor repetido)
receituario5 :: Receituario
receituario5 = [(m1, [8, 17]), (m2, [6, 6]), (m4, [8, 22, 23])]

-- Plano válido
plano1 :: PlanoMedicamento
plano1 = [(6, [m2]), (8, [m1]), (17, [m1]), (22, [m3])]

-- Plano válido
plano2 :: PlanoMedicamento
plano2 = [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m3, m4]), (23, [m4])]

-- TODO: deletar (hor trocado)
plano3 :: PlanoMedicamento
plano3 = [(6, [m2]), (17, [m1]), (8, [m1]), (22, [m3])]

-- TODO: deletar (hor repetido)
plano4 :: PlanoMedicamento
plano4 = [(6, [m2]), (17, [m1]), (17, [m1]), (22, [m3])]

-- TODO: deletar (med trocado)
plano5 :: PlanoMedicamento
plano5 = [(6, [m2]), (8, [m3, m1]), (17, [m1]), (22, [m3])]

-- TODO: deletar (med repetido)
plano6 :: PlanoMedicamento
plano6 = [(6, [m2]), (8, [m1, m1]), (17, [m1]), (22, [m3])]

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

-- hors fora de ordem
plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (22, [Medicar m3]),
    (17, [Medicar m1, Comprar m3 30])
  ]

-- conflito medicar e comprar
plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar m2, Medicar m8]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

-- meds fora da ordem e conflito medicar e comprar
plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar m8, Medicar m2]),
    (8, [Medicar m9, Medicar m1]),
    (17, [Medicar m1, Comprar m1 30]),
    (22, [Medicar m3])
  ]

-- hors repetidos
plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1]),
    (8, [Medicar m1, Comprar m3 30]),
    (22, [Medicar m3])
  ]

-- conflito medicar e comprar
plantaoInvalido5 :: Plantao
plantaoInvalido5 =
  [ (6, [Medicar m2]),
    (8, [Comprar m1 20, Medicar m1, Medicar m4]),
    (17, [Medicar m1]),
    (22, [Medicar m3, Medicar m4]),
    (23, [Medicar m4])
  ]

-- medicar e medicar
plantaoInvalido6 :: Plantao
plantaoInvalido6 =
  [ (6, [Medicar m2]),
    (8, [Medicar m1, Medicar m1, Medicar m4]),
    (17, [Medicar m1]),
    (22, [Medicar m3, Medicar m4]),
    (23, [Medicar m4])
  ]

-- comprar e comprar
plantaoInvalido7 :: Plantao
plantaoInvalido7 =
  [ (6, [Medicar m2]),
    (8, [Comprar m1 20, Comprar m1 30, Medicar m4]),
    (17, [Medicar m1]),
    (22, [Medicar m3, Medicar m4]),
    (23, [Medicar m4])
  ]

-- testando comprarMedicamento
casoTeste1 = comprarMedicamento m3 30 estoque1 == [(m1, 10), (m2, 5), (m3, 30)]

casoTeste2 = comprarMedicamento m6 20 estoque1 == [(m6, 20), (m1, 10), (m2, 5), (m3, 0)]

casoTeste3 = comprarMedicamento m2 2 estoque1 == [(m1, 10), (m2, 7), (m3, 0)]

casoTeste4 = comprarMedicamento m8 20 [] == [(m8, 20)]

conjuntoCasosTeste1 = and [casoTeste1, casoTeste2, casoTeste3, casoTeste4]

--testando tomarMedicamento
casoTeste5 = tomarMedicamento m1 estoque1 == Just [(m1, 9), (m2, 5), (m3, 0)]

casoTeste6 = isNothing (tomarMedicamento m3 estoque1)

conjuntoCasosTeste2 = casoTeste5 && casoTeste6

--testando consultarMedicamento
casoTeste7 = consultarMedicamento m2 estoque1 == 5

casoTeste8 = consultarMedicamento "Aas" estoque1 == 0

conjuntoCasosTeste3 = casoTeste5 && casoTeste6

-- testando demandaMedicamentos
casoTeste9 = demandaMedicamentos receituario1 == [(m1, 2), (m2, 1), (m3, 1)]

conjuntoCasosTeste4 = and [casoTeste9]

--testando geraPlanoReceituario
casoTeste10 = geraPlanoReceituario receituario1 == [(6, [m2]), (8, [m1]), (17, [m1]), (22, [m3])]

casoTeste11 = geraPlanoReceituario receituario2 == [(6, [m2]), (8, [m1, m4]), (17, [m1]), (22, [m3, m4]), (23, [m4])]

conjuntoCasosTeste5 = casoTeste10 && casoTeste11

-- testando geradores de plano e receituário
casoTeste12 = geraReceituarioPlano (geraPlanoReceituario receituario1) == receituario1

casoTeste13 = geraReceituarioPlano (geraPlanoReceituario receituario2) == receituario2

conjuntoCasosTeste6 = casoTeste12 && casoTeste13

-- testando executaPlantao
casoTeste14 = isNothing (executaPlantao plantao1 estoque1)

casoTeste15 = executaPlantao plantao1 estoque2 == Just [(m1, 8), (m2, 4), (m3, 9)]

casoTeste16 = executaPlantao plantao2 estoque1 == Just [(m1, 8), (m2, 4), (m3, 29)]

conjuntoCasosTeste7 = casoTeste14 && casoTeste15 && casoTeste16

-- testando satisfaz
casoTeste17 = not (satisfaz plantao1 plano1 estoque1)

casoTeste18 = satisfaz plantao1 plano1 estoque2

casoTeste19 = satisfaz plantao2 plano1 estoque1

conjuntoCasosTeste8 = casoTeste17 && casoTeste18 && casoTeste19

-- testando plantaoCorreto
casoTeste21 = satisfaz plantao plano1 estoque1
  where
    plantao = plantaoCorreto plano1 estoque1

conjuntoCasosTeste9 = and [casoTeste21]

-- resultado global dos testes
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
conjuntoCasosTestePlantaoValido =
  and
    [ plantaoValido plantao1,
      plantaoValido plantao2,
      plantaoValido plantaoValido0,
      not (plantaoValido plantaoInvalido1),
      not (plantaoValido plantaoInvalido2),
      not (plantaoValido plantaoInvalido3)
    ]
