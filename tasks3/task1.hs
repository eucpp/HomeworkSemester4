-- функция func x l = map (\y -> y*x) l в point-free стиле

func x l = map (\y -> y*x) l

-- эта-редукция по l
func1 x = map (\y -> y*x)

-- переписываем умножение
func2 x = map (\y -> (*) x y)

-- эта-редукция по y
func3 x = map . ( (*) x)

-- композиция 
func4 x = (map . (*)) x

-- эта-редукция по x
func5 = map . (*)
