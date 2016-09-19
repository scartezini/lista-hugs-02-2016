---------------------------- PARTE 1 ------------------------------------------

--1
-- A lista tem que ter 4 elementos e todos os valores da calda tem que ser
-- igual a cabeca da lista
equals (x:xs) = length (x:xs) == 4 && all (==x) xs

--2
-- Usando a função filter passando como predicado uma funçao anonima, cujo a
-- qual vai verificar quais os elementos de x pertecem a y e retornar as lista
-- dos elementos filtrados
x /\ y = filter (\a -> elem a y) x


--3
-- Usando a matematica, o resto é o dividendo sublitraido da multiplicação da
-- parte inteira da divisão com o divisor
modulo x y = x - (x `div` y)*y

--4
-- uso o fato de que 2^n = 2^(n-1) ... e quando for 2^0 = 1
exp2 0 = 1
exp2 n = 2 * exp2 (n-1)

--5
-- É basicamente a função de soma da lista vista em sala,
-- ou seja pega a cabeça e soma com, a chamada da função de soma com a calda,
-- quando a lista é vasia soma com zero
-- Porem para pegar o dobra uso o fato
-- de que 2*(a+b+c+...) = 2*a + 2*b + 2*c + 2*(...)
doubleSumList [] =  0
doubleSumList (x:xs) =  2*x + doubleSumList xs

--7
-- Essa estrutura ( keep elements (matching)) foi vista na parte de expressao
-- case em sala de aula.
-- entao "retorna" a lista l que é uma lista dos inteiros de 1 ate x, dos quais
-- correspondem a verificação logica de que o x modulo l == 0
divs x = [l |l<-[1..x], x`mod`l == 0]

---------------------------- PARTE 2 ------------------------------------------
--8
-- Se as listas forem de tamanho diferente, ja não contem os mesmos elementos
-- se forem de tamanhos iguais verifico se os elementos de x que esta contido
-- em y eh igual a x,se for todos os elementos de x pertencem a y e como elas
-- possuem o mesmo tamanho a reciproca eh verdadeira, tudo é feito
-- por meio da filter passando como predicado uma função anonima.
listsEquals x y = (length x == length y) && (x == filter (\a -> elem a y) x)


-- 10
-- Por compreção de lista crio uma lista com os elementos n da lista x
-- pegando o tamanho dessa lista tenho quantas vezes que n aparece em x
rep n x = length [l | l <- x, l == n]

-- 11
-- Primeiro controi uma lista n que recebe os elementos de x que satisfazem a
-- condicao de ser maior que a soma de x dividido pelo tamanho de x, ou seja
-- os elementos que estao em x e que estao acima da media, pegando o tamanho
-- desta lista temos o numero de elementos que esta acima da media
nElemUpMean x =  length ([n |n <- x, n > ((sum x) `div` (length x))])


-- 13
-- Se a cabeça da lista for diferente do elemento faz 1 mais a chamada da função,
-- com o elemento e a calda caso nao for retorna 0, desse modo ele vai
-- percorrendo a lista ate achar o elemento e cada vez somando 1 pois esta,
-- passando para a proxima posicao
-- se nao tiver o elemento lanca um erro, quando casa com lista vazia nao vai
-- ter o elemento independente de qual seja
posicao _ [] = error "Não possui o elemento"
posicao n (x:xs)
	| n /= x = 1 + posicao n xs
	| otherwise = 0


---------------------------- PARTE 3 ------------------------------------------
--16
-- Cria uma lista l com os elementos de 1 ate n que satisfazem a condicao, de
-- que os divisores deste numero são somente 1 e o proprio numero,
-- estou usando a funcao divs implementada na questao 7
primos n = [l | l <- [1..n], (divs l) == [1,l]]

--20
-- Vai creiando uma lista com a aplicacao de f em cada elemento da tripla
-- e o proximo elemento da lista é o map com a f e as caldas da lista,
-- quando a menor lista acabar para a funçao
map3 f (x:xs) (y:ys) (z:zs)= (f x, f y, f z) : map3 f xs ys zs
map3 _ _ _ _ = []
