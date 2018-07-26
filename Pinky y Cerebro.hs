import Text.Show.Functions

data Animal = Animal {coeficienteIntelectual :: Int, especie::String,capacidades::[String] } deriving (Show)
type Transformacion = Animal->Animal
--PUNTO 1

vacaLista = Animal {coeficienteIntelectual=200 , especie = "vaca",capacidades=["Hablar","Comer pasto mientras hace tecniletra","muuuuuudarse a un nivel vaca"]}
pajaroLoco= Animal {coeficienteIntelectual=100 , especie ="pajaro", capacidades=["Reirse","salirse con la suya","picotear"]}

--PUNTO 2
inteligenciaSuperior :: Int->Transformacion
inteligenciaSuperior incrementoDeCoeficiente unAnimal = unAnimal{coeficienteIntelectual = (coeficienteIntelectual unAnimal) + incrementoDeCoeficiente}

pinkificar :: Transformacion
pinkificar unAnimal = unAnimal{capacidades = []}

superpoderes::Transformacion
superpoderes unAnimal
  | (especie unAnimal) == "Elefante" = capacitar "No tenerle miedo a los ratones" unAnimal
  | (especie unAnimal) == "Raton" && compararCoeficiente 100 unAnimal = capacitar "Hablar" unAnimal


compararCoeficiente:: Int->CriterioDeExito
compararCoeficiente coeficienteASuperar unAnimal = (coeficienteIntelectual unAnimal) > coeficienteASuperar

capacitar::String->Transformacion
capacitar capacidad unAnimal = unAnimal {capacidades = (capacidades unAnimal) ++ [capacidad]}

--PUNTO 3
type CriterioDeExito = Animal->Bool
antropomorfico::CriterioDeExito
antropomorfico unAnimal = sabeHablar unAnimal && compararCoeficiente 60 unAnimal

sabeHablar::Animal->Bool
sabeHablar unAnimal = any (=="Hablar") (capacidades unAnimal)

noTanCuerdo::CriterioDeExito
noTanCuerdo unAnimal = (>2).length.(filter pinkiesco) (capacidades unAnimal)

--PUNTO 4
data Experimento = Experimento{transformaciones::[String],criterioDeExito::CriterioDeExito}

--experimentoExitoso experimento unAnimal = (criterioDeExito experimento) (aplicarTransformaciones unAnimal transformaciones)

--aplicarTransformaciones unAnimal transformaciones = map (aplicarTransformacion unAnimal) (transformaciones experimento)

--aplicarTransformacion unAnimal transformacion = transformacion unAnimal

--Necesito una función que hicimos llamada pam que es como map pero le aplica una lista de cosas a un único elemento.



--"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados,
-- hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes.
-- Como criterio de éxito, ver si quedó antropomórfico" 

raton = Animal{especie = "Raton", coeficienteIntelectual=17, capacidades=["Destruenglonir el mundo","Hacer planes desalmados"]}

consulta:: CriterioDeExito
consulta unAnimal = antropomorfico (hacerExperimento unAnimal)

hacerExperimento :: Transformacion
hacerExperimento = superpoderes.(inteligenciaSuperior 10).pinkificar

--PUNTO 5
type Reporte = (Ord a) =>[Animal]->[String]->[String]->[a] --Esta firma es lo que pide el ejercicio?