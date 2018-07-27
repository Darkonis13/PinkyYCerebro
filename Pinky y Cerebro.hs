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
noTanCuerdo unAnimal = tieneMasDeDosPinkiescas.capacidades unAnimal

tieneMasDeDosPinkiescas :: [String]->Bool
tieneMasDeDosPinkiescas = (>2).length.(filter pinkiesco)

--PUNTO 4
data Experimento = Experimento{transformaciones::[Transformacion],criterioDeExito::CriterioDeExito}

experimentoExitoso :: Experimento->CriterioDeExito
experimentoExitoso unExperimento = all (criterioDeExito unExperimento).aplicar (transformaciones unExperimento)

aplicar :: [Transformacion]->Animal->[Animal]
aplicar transformaciones unAnimal = unificar.map ($ unAnimal) transformaciones

--"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados,
-- hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes.
-- Como criterio de éxito, ver si quedó antropomórfico" 

raton = Animal{especie = "Raton", coeficienteIntelectual=17, capacidades=["Destruenglonir el mundo","Hacer planes desalmados"]}

consulta:: CriterioDeExito
consulta unAnimal = antropomorfico (hacerExperimento unAnimal)

hacerExperimento :: Transformacion
hacerExperimento = superpoderes.(inteligenciaSuperior 10).pinkificar

--PUNTO 6
--Los experimentos que pueden hacerse son aquellos que no dependan de la lista de habilidades completa del animal, puesto que nunca se terminará de generar.