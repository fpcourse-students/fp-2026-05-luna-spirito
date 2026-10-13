{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- | Домашка 5. Задачи для уверенности.
module Level2 where

import MetaUtils (todo)

-- 2.1. Унарные числа.
--
-- makeNat строит число из n конструкторов Suc и завершающего Zero.
-- Для отрицательного аргумента выбросьте error. foldNat применяет s ровно n раз
-- к z: foldNat (+ 1) 0 (Suc (Suc Zero)) == 2.
-- Напишите обе функции с помощью сопоставления и рекурсии.

data Nat = Zero | Suc Nat
  deriving (Eq, Show)

makeNat :: Int -> Nat
makeNat = \case
  0 -> Zero
  x
    | x < 0 -> error "negative"
    | otherwise -> Suc $ makeNat $ x - 1

foldNat :: (a -> a) -> a -> Nat -> a
foldNat s z = go where
  go = \case
    Zero -> z
    Suc n -> s $ go n

-- 2.2. Другое бинарное дерево.
--
-- Empty не содержит значения. У BinNode есть два поддерева и одно значение.
-- Реализуйте mapBinTree, сохраняя форму и меняя содержимое по функции.
-- Затем напишите сигнатуру и тело bangAll: она превращает BinTree (BinTree Int)
-- в BinTree (BinTree String); после каждого числа в строке стоит "!".

data BinTree a = Empty | BinNode (BinTree a) a (BinTree a)
  deriving (Eq, Show)

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f = go where
  go = \case
    Empty -> Empty
    BinNode a b c -> BinNode (go a) (f b) (go c)

bangAll :: BinTree (BinTree Int) -> BinTree (BinTree String)
bangAll = mapBinTree (mapBinTree \x -> show x ++ "!")


-- 2.3. Лексикографическое сравнение.
--
-- lexLess xs ys истинно, если xs строго меньше ys.
-- Собственный префикс меньше списка; при первом различии решает пара элементов.
-- Равные списки не меньше друг друга. Для элементов можно использовать compare
-- и операторы сравнения; списки сравнивайте собственной рекурсией,
-- не передавайте целые списки стандартному (<) или compare.

lexLess :: Ord a => [a] -> [a] -> Bool
lexLess _ [] = False
lexLess [] _ = True
lexLess (x:xs) (y:ys) =
  x < y || (x == y && lexLess xs ys)

-- 2.4. Цифры.
--
-- Верните цифры неотрицательного Int от старшей к младшей.
-- digits 0 == [0], digits 120 == [1, 2, 0].
-- Отрицательный аргумент — error. Используйте div/mod или quotRem,
-- без перевода числа в строку.

digits :: Int -> [Int]
digits = \case
  0 -> [0]
  x
    | x < 0 -> error "< 0"
    | otherwise -> go [] x
  where
    go acc = \case
      0 -> acc
      n ->
        let (a, b) = quotRem n 10
        in go (b:acc) a


-- 2.5. Числа Фибоначчи и списки.
--
-- Бесконечный список fibs уже дан; в make repl смотрите take 10 fibs.
-- isFib проверяет принадлежность последовательности, в том числе 0 и 1.
-- Отрицательные числа не принадлежат ей.
-- countFibs считает элементы входного списка, являющиеся числами Фибоначчи;
-- повторения считаются отдельно. Например, countFibs [0, 1, 1, 4] == 3.
-- Используйте список fibs, без формулы через квадратный корень.
-- Integer выбран, чтобы последовательность не переполнялась.

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

isFib :: Integer -> Bool
isFib x = go fibs where
  go = \case
    [] -> error "impossible"
    (f:fs) -> x == f || (f < x && go fs)
    -- можно ещё через takeWhile + elem красиво

countFibs :: [Integer] -> Int
countFibs = countFibs 0 where
  countFibs acc = \case
    [] -> acc
    x:xs -> countFibs (if isFib x then acc+1 else acc) xs


-- 2.6. Скользящее окно.
--
-- [1..6] `withWindow` 3 == [[1,2,3],[2,3,4],[3,4,5],[4,5,6]].
-- Длина каждого окна равна n; неполное последнее окно не включается.
-- Если n больше длины входа, результат []; если n <= 0, выбросьте error.
-- Функция должна позволять получить первые окна бесконечного списка.
-- Сигнатуру не меняйте.

withWindow :: [a] -> Int -> [[a]]
withWindow _ n | n <= 0 = error "n <= 0"
withWindow [] _ = []
withWindow (x0:xs0) n0 = case myTake (x0:xs0) n0 of
  Nothing -> []
  Just curr -> curr:withWindow xs0 n0
  where
  myTake _ 0 = Just []
  myTake [] _ = Nothing
  myTake (x:xs) n = case myTake xs (n-1) of
    Nothing -> Nothing
    Just r -> Just $ x:r

-- 2.7. Сохраните проверенные опции в типе.
--
-- RawOptions пришёл с границы программы. True означает запрос справки:
-- формат и путь должны отсутствовать. False означает запуск:
-- формат обязателен, отсутствие пути означает Stdout.
-- Верните Just Options, если комбинация допустима, иначе Nothing.
-- После этого потребитель Options уже не проверяет сочетания полей.
-- Корректность содержимого FilePath в этом задании не проверяется.

data Format = Plain | JSON
  deriving (Eq, Show)

data Target = Stdout | File FilePath
  deriving (Eq, Show)

data Options = Help | Run Format Target
  deriving (Eq, Show)

data RawOptions = RawOptions Bool (Maybe Format) (Maybe FilePath)
  deriving (Eq, Show)

parseOptions :: RawOptions -> Maybe Options
parseOptions = \case
  RawOptions True Nothing Nothing -> Just Help
  RawOptions False (Just fmt) mfp -> Just $ Run fmt case mfp of
    Just fp -> File fp
    Nothing -> Stdout
  _ -> Nothing
