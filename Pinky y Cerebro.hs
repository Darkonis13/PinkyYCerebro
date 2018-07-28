import Text.Show.Functions
import Data.List

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
  | otherwise = unAnimal

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

--noTanCuerdo::CriterioDeExito
--noTanCuerdo unAnimal = tieneMasDeDosPinkiescas.capacidades unAnimal

--tieneMasDeDosPinkiescas :: [String]->Bool
--tieneMasDeDosPinkiescas = (>2).length.(filter pinkiesco)

--PUNTO 4
data Experimento = Experimento{transformaciones::[Transformacion],criterioDeExito::CriterioDeExito}
experimentoExitoso :: Experimento->CriterioDeExito
experimentoExitoso unExperimento = criterioDeExito unExperimento . aplicarTransformaciones unExperimento

aplicarTransformaciones :: Experimento->Transformacion
aplicarTransformaciones unExperimento = foldl1 (.) (transformaciones unExperimento)

--"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados,
-- hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes.
-- Como criterio de éxito, ver si quedó antropomórfico" 

raton = Animal{especie = "Raton", coeficienteIntelectual=17, capacidades=["Destruenglonir el mundo","Hacer planes desalmados"]}
experimentoConRaton = Experimento{transformaciones=[superpoderes,inteligenciaSuperior 10,pinkificar], criterioDeExito=antropomorfico}

--Como resultado, el criterio de éxito no se cumple.

--PUNTO 5
listaDeCoeficientes :: [String]->Experimento->[Animal]->[Int]
listaDeCoeficientes = generarListaPedida coeficienteIntelectual (>0)

listaDeEspecies :: [String]->Experimento->[Animal]->[String]
listaDeEspecies unasCapacidades = generarListaPedida especie (==(length unasCapacidades)) unasCapacidades

listaDeCantidadDeCapacidades :: [String]->Experimento->[Animal]->[Int]
listaDeCantidadDeCapacidades = generarListaPedida (length.capacidades) (==0)

generarListaPedida :: (Animal->a)->(Int->Bool)->[String]->Experimento->[Animal]->[a]
generarListaPedida elementoPedido elementoDeComparacion unasCapacidades unExperimento = map (elementoPedido) .filter (compararListas elementoDeComparacion unasCapacidades) . aplicarExperimentoAAnimales unExperimento

aplicarExperimentoAAnimales :: Experimento->[Animal]->[Animal]
aplicarExperimentoAAnimales unExperimento = map (aplicarTransformaciones unExperimento)  

compararListas :: (Int->Bool)->[String]->Animal->Bool
compararListas elementoDeComparacion unasCapacidades unosAnimales = elementoDeComparacion.length.intersect unasCapacidades $ (capacidades unosAnimales)

--PUNTO 6
--Los experimentos que pueden hacerse son aquellos que no dependan de la lista de habilidades completa del animal, puesto que nunca se terminará de generar.