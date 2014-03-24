-- сумма всех чисел деленная на произведение косинусов

func' :: Floating a => a -> a -> [a] -> a
func' sum prod [] = sum / prod
func' sum prod (x:xs) = func' (sum + x) (prod * cos x) xs

func = func' 0 1
