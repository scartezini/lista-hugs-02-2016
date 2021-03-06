﻿---------------------------- PARTE 1 ------------------------------------------

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

--17
-- Verifica se a lista x eh igual a lista x de tras para frente
polindrome x = x == reverse x

--18
-- pega os n primeiros elementos da string coloca na parte 1 da tupla,
-- joga fora os n+1 primeira elenetos coloca na parte 2 da tupla,
-- onde n é o numerdo de elementos ate o '/', ou seja pega a string
-- ate o '/' e a string retirando a parte antes do '/' e o '/'
split x = (take n x, drop (n+1) x) where n = length(takeWhile(/= '/') x)

--20
-- Vai criando uma lista com a aplicacao de f em cada elemento da tripla
-- e o proximo elemento da lista é o map com a f e as caldas da lista,
-- quando a menor lista acabar para a funçao
map3 f (x:xs) (y:ys) (z:zs)= (f x, f y, f z) : map3 f xs ys zs
map3 _ _ _ _ = []


---------------------------- PARTE 4 ------------------------------------------

-- 22
-- Funcao com where que calcula a potencia mesmo sendo o expoente negativo.
pow x y | y >= 0 = x^y
	| otherwise = np x where np = (1/) . (^(-y))

-- 24
-- A função somaDez mapeia cada elemento da lista xs 
-- Aplicando a função lambda adicionando 10 a cada elemento
-- O resultado é uma lista de cada elemento de xs adicionado 10.
-- By Rafael da Silva Rocha - 12/0133229
somaDez xs = map(\x -> x+10) xs

-- 25
-- A função somadivide usa composição de funções pegando os 5 primeiros elementos
-- da lista xs e somando-os, esse resultado é dividido pela soma dos 3 primeiros
-- elementos de xs. Se a lista for vazia o resultado é zero.
-- A lista se aplica tanto para uma lista de Int e uma lista de Float, ou seja,
-- é polimórfica.
-- By Rafael da Silva Rocha - 12/0133229
somadivide [] = 0;
somadivide xs = ((sum . take 5) xs) / n where n = (sum . take 3) xs

-- 26
-- A função usando guarda pega os z primeiros pares se z estiver for maior igual a zero
-- ou menor que 10
-- Se z for maior que 10, pega os z primeiros número ímpares.
-- Para pegar os z números ímpares é usando a lista infinita[1,3..] para não estourar a memoria
-- é usado avaliação preguiçosa onde só é pegado a lista após saber a quantidade de elementos
-- que serão necessários.
-- By Rafael da Silva Rocha - 12/0133229
pegaNprimeirosParesOuImpares z
					| z >= 0 && z < 10  =  take z [2,4..10]
	           		| z >= 10 = take z [1,3..] 

---------------------------- PARTE 5 ------------------------------------------


-- 28
desconhecido = do putStr "Impossível identificar. Animal desconhecido."
		return ()
		
mamifero = do putStr "É carnívoro (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then carnivoro
		else putStr "É ungulado (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then ungulado
		else desconhecido
	
ave = do putStr "voa (s/n)?"
		if carac=='S' || carac=='s' then voa
		else putStr "Tem pescoço comprido (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Avestruz"
		else putStr "Nada (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then nada
		else desconhecido
	
carnivoro = do putStr "Tem cor amarelada (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then amarelado
		else desconhecido
		
ungulado = do putStr "Tem pescoço grande (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then pescoco
		else putStr "Tem listras pretas (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Zebra"
		else desconhecido

amarelado = do putStr "Tem manchas pretas (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Guepardo"
		else putStr "Tem listras pretas (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Tigre"
		else desconhecido

pescoco = do putStr "Tem pernas grandes (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Girafa"
		else desconhecido

voa = do putStr "Tem asas longas (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Albatroz"
		else desconhecido
		
nada = do putStr "É preto e branco (s/n)?"
		carac <- getChar
		if carac=='S' || carac=='s' then putStr "Pinguim"
		else desconhecido

Main>
putStr "É mamífero (s/n)?"
carac <- getChar
if carac=='S' || carac=='s' then mamifero
else putStr "É ave (s/n)?"
carac <- getChar
if carac=='S' || carac=='s' then ave
else desconhecido
