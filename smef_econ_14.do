clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem14.log

* Spatial Econometrics

net search spatwmat // imports or generates several kinds of spatialweights matrices 
net search spatdiag // spatial diagnostics were developed mainly to test whether data analyzed via OLS regression exhibit spatial correlation
net search spatlsa // computes local indices of spatial autocorrelation
net search spatgsa // computes global indices of spatial autocorrelation
net search spatreg // estimates spatial lag and spatial error models

use EU27.dta, clear

* Создадим переменные: средний темп роста ВВП на душу за период 1995-2004 и натуральный логарифм ВВП на душу в 1995 году
gen y9504 = (1/9)*ln(y04/y95)
gen lny95 = ln(y95)

* Сравним средний уровень среднего темпа роста ВВП на душу и ВВП на душу в 1995 году для новых и старых стран-членов Европейского союза.
* Что можно сказать относительно гипотезы абсолютной конвергенции для стран из разных групп на основании описательных статистик?
sort nms_dummy
by nms_dummy: sum y9504 lny95 

* Вернем исходный порядок регионов
sort region_id

* Оценим модель абсолютной бета-конвергенции для ВВП на душу (без учета пространственного расположения)
reg y9504 lny95

* С учетом возможной гетероскедастичности рассчитаем робастные стандартные отклонения
reg y9504 lny95, vce(robust)

* Рассчитаем скорость сходимости
scalar beta = -ln(1-9*(0.0175997))/9
display beta

* Рассчитаем время, необходимое для сокращения различий между регионами в два раза
scalar t = -ln(0.5)/beta
display t

* Включим в модель дамми-переменную на новых членов ЕС
reg y9504 lny95 nms_dummy, vce(robust)

* Оценим две модели по подвыборкам
sort nms_dummy
by nms_dummy: reg y9504 lny95, vce(robust)

* Вернем исходный порядок регионов
sort region_id

* Протестируем наличие пространственной автокорреляции для зависимой и независимой переменных
* Используем в качестве матрицы весов стандартизированную матрицу обратных расстояний, измеренных во времени
set matsize 5000
spatwmat using inverse_travel_time_EU27.dta, name(W1) standardize
matrix list W1

* Проверим, есть ли в данных пространственная корреляция
* Рассчитаем глобальный I-индекс Морана для зависимой переменной и для остатков модели
spatgsa y9504, w(W1) moran 

quietly: reg y9504 lny95, vce(robust)
predict e1, res
spatgsa e1, w(W1) moran

* Рассчитаем локальный индекс Морана I
* Диаграмма рассеяния Морана: по оси Ox откладывается значение переменной в каждой территориальной единице, в по оси Oy — пространственный лаг, который представляет собой средневзвешенное значение переменной по всем соседям
* На диаграмме рассеяния Морана линиями отмечаются средние значения по обеим осям, а наклонной линией представляется линейная регрессия этих значений, при этом тангенс угла наклона прямой равен значению индекса Морана
spatlsa y9504, weights(W1) moran graph(moran) symbol(id) id(region_id) savegraph(moran_y.wmf, replace)
spatlsa e1, weights(W1) moran graph(moran) symbol(id) id(region_id) savegraph(moran_e1.wmf, replace)


* Оценим модели условной конвергенции с пространственными лагами (SAR и SEM) методом максимального правдоподобия
* Передаем весовую матрицу, стандартизируем ее и вычисляем вектор собственных значений
spatwmat using inverse_travel_time_EU27.dta, name(W1) eigenval(E1) standardize

* spatreg требует задания матрицы пространственных весов и вектора собственных значений этой матрицы
* SAR-модель
spatreg y9504 lny95, weights(W1) eigenval(E1) model(lag)
* SEM-модель
spatreg y9504 lny95, weights(W1) eigenval(E1) model(error)

* Оценим модель условной конвергенции, включив дамми переменные для стран (Германия является базовой)
reg y9504 lny95 at be cz dk ee es fi fr gr hu ie it lt lu nl pl pt se si sk uk, ro

* Исследуем пространственную корреляцию между остатками последней оцененной модели регрессии (reg)
spatdiag, weights(W1)

* Посторим диаграмму Морана для остатков модели
quietly: reg y9504 lny95 at be cz dk ee es fi fr gr hu ie it lt lu nl pl pt se si sk uk, ro
predict e2, res
spatlsa e2, weights(W1) moran graph(moran) symbol(id) id(region_id) savegraph(moran_e2.wmf, replace)

* Оценим модели условной бета-конвергенции с пространственными лагами (SAR и SEM) методом максимального правдоподобия с включенными странновыми дамми переменными
* SAR-модель (Модель с пространственным авторегрессионным лагом)
spatreg y9504 lny95 at be cz dk ee es fi fr gr hu ie it lt lu nl pl pt se si sk uk, weights(W1) eigenval(E1) model(lag)

* SEM-модель (Модель с пространственным взаимодействием в ошибках)
spatreg y9504 lny95 at be cz dk ee es fi fr gr hu ie it lt lu nl pl pt se si sk uk, weights(W1) eigenval(E1) model(error)

* Качество подгонки и сравнение моделей осуществляется с помощью Log-likelihood, информационных критериев (AIC и BIC), квадрата корреляции между наблюдаемыми и предсказанными по модели значениями. R2 нельзя использовать.


log close
