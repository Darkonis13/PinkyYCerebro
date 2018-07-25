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

superPoderes :: Transformacion
superPoderes unAnimal
 | (especie unAnimal) == "Elefante" = unAnimal {capacidades = (capacidades unAnimal) ++ ["no tenerle miedo a los ratones"]}
 | (especie unAnimal) == "Raton" && (coeficienteIntelectual unAnimal) > 100 = unAnimal{capacidades = (capacidades unAnimal) ++ ["Hablar"]}

--PUNTO 3
type CriterioDeExito = Animal->Bool

--antropomorfico::CriterioDeExito
antropomorfico unAnimal = any (=="Hablar") (capacidades unAnimal) && (coeficienteIntelectual unAnimal) > 60

--noTanCuerdo::CriterioDeExito
--noTanCuerdo unAnimal = (length.(filter pinkiesco) (capacidades unAnimal)) > 2

--PUNTO 4
data Experimento = Experimento{listaDeTransformaciones::[Transformacion], criterioDeExito::CriterioDeExito}

--experimentoExitoso:: Experimento -> Transformacion
experimentoExitoso unExperimento unAnimal = all (==True) (map (criterioDeExito unExperimento) (aplicandoTransformaciones unExperimento) unAnimal)

--aplicandoTransformaciones::Experimento->Transformacion
aplicandoTransformaciones unExperimento unAnimal= map (transformar unAnimal) (listaDeTransformaciones unExperimento)

--transformar:: Transformacion
transformar unAnimal unaTransformacion = unaTransformacion unAnimal