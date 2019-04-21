type Restaurante = (String,Int, [Comida])
type Comida = (Int,Int,Int)
type RestauranteAux = (Bool,(String,Int,[Comida]))
get1 (a,b,c)=a
get2 (a,b,c)=b
get3rd (_,_,a) = a
cantidadSal :: [Comida]->Int
cantidadSal comida = sum (map (get1) comida)

aprobadoAntonBool :: Restaurante -> Bool
aprobadoAntonBool restaurante = cantidadSal (get3rd restaurante) <30
listaAprobadosAntonBool ::[Restaurante]->[Bool]
listaAprobadosAntonBool restaurantes = map (aprobadoAntonBool) restaurantes
listaAprobadosAntonBool2:: [Restaurante]-> [RestauranteAux]
listaAprobadosAntonBool2 restaurantes= zip (listaAprobadosAntonBool restaurantes)  restaurantes
listaAprobadosAnton:: [Restaurante]->[Restaurante]
listaAprobadosAnton restaurantes= map (snd) (filter ((==True).fst) (listaAprobadosAntonBool2 restaurantes))

aprobadoColleteBoolTemp :: [Comida]-> Bool
aprobadoColleteBoolTemp comida = all (>40) (map(get1) comida)
aprobadoColleteBoolSal :: [Comida]-> Bool
aprobadoColleteBoolSal  comida = all (>10) (map (get3rd) comida)
aprobadoColleteBool :: Restaurante -> Bool
aprobadoColleteBool restaurante = aprobadoColleteBoolTemp (get3rd restaurante) && aprobadoColleteBoolSal (get3rd restaurante)
aprobadoColleteBoolTot :: [Restaurante]-> [Bool]
aprobadoColleteBoolTot restaurante = map (aprobadoColleteBool) restaurante
aprobadoColleteTot :: [Restaurante]-> [RestauranteAux]
aprobadoColleteTot restaurante = zip (aprobadoColleteBoolTot restaurante)(restaurante)
aprobadoCollete :: [Restaurante]->[Restaurante]
aprobadoCollete restaurante= map (snd)(filter ((==True).fst)(aprobadoColleteTot restaurante))

aprobadoSkinnerBool :: Restaurante ->Bool
aprobadoSkinnerBool restaurante = all (> (get2 restaurante)) (map (get2) (get3rd restaurante))
listaAprobadoSkinnerBool1 :: [Restaurante]-> [Bool]
listaAprobadoSkinnerBool1 restaurantes = map (aprobadoSkinnerBool) restaurantes
listaAprobadoSkinnerBool2 :: [Restaurante]-> [RestauranteAux]
listaAprobadoSkinnerBool2 restaurantes =zip (listaAprobadoSkinnerBool1 restaurantes)(restaurantes)
aprobadoSkinner1 :: [Restaurante]->[Restaurante]
aprobadoSkinner1 restaurantes =map (snd) (filter ((==True).fst)(listaAprobadoSkinnerBool2 restaurantes))