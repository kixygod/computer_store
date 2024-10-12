{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad          (unless, void)
import           Data.Char              (toLower)
import           Data.String            (fromString)
import           Database.SQLite.Simple
import           DatabaseInit           (initializeDB)
import           System.Process         (system)

-- Главное меню
mainMenu :: Connection -> Int -> IO ()
mainMenu conn userId = do
  putStrLn "Главное меню:"
  putStrLn "1. Выбор материнской платы"
  putStrLn "2. Выбор процессора"
  putStrLn "3. Выбор оперативной памяти"
  putStrLn "4. Выбор видеокарты"
  putStrLn "5. Выбор блока питания"
  putStrLn "6. Выбор накопителя"
  putStrLn "7. Посмотреть корзину"
  putStrLn "8. Выход"
  putStrLn "Введите номер действия:"
  choice <- readLn
  case choice of
    1 -> selectMotherboard conn userId
    2 -> selectProcessor conn userId
    3 -> selectRam conn userId
    4 -> selectGraphicsCard conn userId
    5 -> selectPowerSupply conn userId
    6 -> selectStorage conn userId
    7 -> viewCart conn userId
    8 -> putStrLn "Выход..."
    _ -> putStrLn "Неверный выбор, попробуйте снова." >> mainMenu conn userId

-- Выбор процессора
selectProcessor :: Connection -> Int -> IO ()
selectProcessor conn userId = do
  processors <- query_ conn (fromString "SELECT id, name, socket, cores, clock_speed, price FROM processors") :: IO [(Int, String, String, Int, Double, Int)]
  putStrLn "Список доступных процессоров:"
  mapM_ (\(processorId, name, socket, cores, clock_speed, price) ->
            putStrLn $ show processorId ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show cores ++ " ядер, "
                      ++ show clock_speed ++ " ГГц) - "
                      ++ show price ++ " руб.") processors
  putStrLn "Введите номер процессора, который хотите выбрать:"
  processorId <- readLn
  processor <- query conn (fromString "SELECT name, price FROM processors WHERE id = ?") (Only processorId) :: IO [(String, Int)]
  case processor of
    [(name, price)] -> addToCartWithCheck conn userId 2 processorId price
    _               -> putStrLn "Процессор не найден."
  mainMenu conn userId

-- Выбор материнской платы
selectMotherboard :: Connection -> Int -> IO ()
selectMotherboard conn userId = do
  motherboards <- query_ conn (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards") :: IO [(Int, String, String, String, String, Int, Int)]
  putStrLn "Список доступных материнских плат:"
  mapM_ (\(moboId, name, socket, form_factor, supported_ram_type, ram_slots, price) ->
            putStrLn $ show moboId ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show ram_slots ++ "x"
                      ++ supported_ram_type ++ ", "
                      ++ form_factor ++ ") - "
                      ++ show price ++ " руб.") motherboards
  putStrLn "Введите номер материнской платы, которую хотите выбрать:"
  moboId <- readLn
  mobo <- query conn (fromString "SELECT name, price FROM motherboards WHERE id = ?") (Only moboId) :: IO [(String, Int)]
  case mobo of
    [(name, price)] -> addToCartWithCheck conn userId 1 moboId price
    _               -> putStrLn "Материнская плата не найдена."
  mainMenu conn userId

-- Выбор оперативной памяти
selectRam :: Connection -> Int -> IO ()
selectRam conn userId = do
  ram <- query_ conn (fromString "SELECT id, name, size, type, frequency, price FROM ram") :: IO [(Int, String, Int, String, Int, Int)]
  putStrLn "Список доступной оперативной памяти:"
  mapM_ (\(ramId, name, size, ram_type, frequency, price) ->
            putStrLn $ show ramId ++ ": " ++ name ++ " (" ++ show size ++ " ГБ, "
                      ++ ram_type ++ ", "
                      ++ show frequency ++ " МГц) - "
                      ++ show price ++ " руб.") ram
  putStrLn "Введите номер оперативной памяти, которую хотите выбрать:"
  ramId <- readLn
  selectedRam <- query conn (fromString "SELECT name, price FROM ram WHERE id = ?") (Only ramId) :: IO [(String, Int)]
  case selectedRam of
    [(name, price)] -> addToCartWithCheck conn userId 3 ramId price
    _               -> putStrLn "Оперативная память не найдена."
  mainMenu conn userId

-- Выбор видеокарты
selectGraphicsCard :: Connection -> Int -> IO ()
selectGraphicsCard conn userId = do
  gpus <- query_ conn (fromString "SELECT id, name, memory_size, memory_type, core_clock_speed, price FROM graphics_cards") :: IO [(Int, String, Int, String, Double, Int)]
  putStrLn "Список доступных видеокарт:"
  mapM_ (\(gpuId, name, memory_size, memory_type, core_clock_speed, price) ->
            putStrLn $ show gpuId ++ ": " ++ name ++ " (" ++ show memory_size ++ " ГБ, "
                      ++ memory_type ++ ", "
                      ++ show core_clock_speed ++ " ГГц) - "
                      ++ show price ++ " руб.") gpus
  putStrLn "Введите номер видеокарты, которую хотите выбрать:"
  gpuId <- readLn
  selectedGpu <- query conn (fromString "SELECT name, price FROM graphics_cards WHERE id = ?") (Only gpuId) :: IO [(String, Int)]
  case selectedGpu of
    [(name, price)] -> addToCartWithCheck conn userId 4 gpuId price
    _               -> putStrLn "Видеокарта не найдена."
  mainMenu conn userId

-- Выбор блока питания
selectPowerSupply :: Connection -> Int -> IO ()
selectPowerSupply conn userId = do
  psus <- query_ conn (fromString "SELECT id, name, wattage, efficiency_rating, form_factor, price FROM power_supplies") :: IO [(Int, String, Int, String, String, Int)]
  putStrLn "Список доступных блоков питания:"
  mapM_ (\(psuId, name, wattage, efficiency_rating, form_factor, price) ->
            putStrLn $ show psuId ++ ": " ++ name ++ " (" ++ show wattage ++ " Вт, "
                      ++ efficiency_rating ++ ", "
                      ++ form_factor ++ ") - "
                      ++ show price ++ " руб.") psus
  putStrLn "Введите номер блока питания, который хотите выбрать:"
  psuId <- readLn
  selectedPsu <- query conn (fromString "SELECT name, price FROM power_supplies WHERE id = ?") (Only psuId) :: IO [(String, Int)]
  case selectedPsu of
    [(name, price)] -> addToCartWithCheck conn userId 5 psuId price
    _               -> putStrLn "Блок питания не найден."
  mainMenu conn userId

-- Выбор накопителя
selectStorage :: Connection -> Int -> IO ()
selectStorage conn userId = do
  storages <- query_ conn (fromString "SELECT id, name, type, capacity, interface, price FROM storage") :: IO [(Int, String, String, Int, String, Int)]
  putStrLn "Список доступных накопителей:"
  mapM_ (\(storageId, name, stype, capacity, interface, price) ->
            putStrLn $ show storageId ++ ": " ++ name ++ " (" ++ stype ++ ", "
                      ++ show capacity ++ " ГБ, "
                      ++ interface ++ ") - "
                      ++ show price ++ " руб.") storages
  putStrLn "Введите номер накопителя, который хотите выбрать:"
  storageId <- readLn
  selectedStorage <- query conn (fromString "SELECT name, price FROM storage WHERE id = ?") (Only storageId) :: IO [(String, Int)]
  case selectedStorage of
    [(name, price)] -> addToCartWithCheck conn userId 6 storageId price
    _               -> putStrLn "Накопитель не найден."
  mainMenu conn userId

-- Функция для добавления товара в корзину с проверкой дубликатов и запросом на замену
addToCartWithCheck :: Connection -> Int -> Int -> Int -> Int -> IO ()
addToCartWithCheck conn userId categoryId newBoxId newPrice = do
  -- Проверяем, есть ли уже товар этой категории в корзине
  existingItem <- query conn
    (fromString "SELECT products.id, products.box_id, products.price FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = ?")
    (userId, categoryId) :: IO [(Int, Int, Int)]

  -- Получаем название нового товара
  newItemName <- getItemName conn newBoxId categoryId

  case existingItem of
    [(existingProductId, existingBoxId, _)] -> do
      -- Проверяем, совпадают ли box_id текущего товара и нового
      if existingBoxId == newBoxId
        then putStrLn $ "Товар " ++ newItemName ++ " уже добавлен в корзину."
        else do
          -- Получаем название текущего товара
          existingItemName <- getItemName conn existingBoxId categoryId

          -- Если товар этой категории уже есть, спрашиваем, хочет ли пользователь его заменить
          putStrLn $ "Заменить имеющуюся " ++ existingItemName ++ " на " ++ newItemName ++ "? (Да/Нет или Д/Н)"
          answer <- getLine
          if map toLower answer `elem` ["да", "д"]
            then do
              -- Заменяем товар: удаляем старый и добавляем новый
              execute conn (fromString "DELETE FROM cart_items WHERE product_id = ?") (Only existingProductId)
              addProductAndToCart conn userId categoryId newBoxId newPrice
              putStrLn $ "Товар " ++ newItemName ++ " заменён в корзине."
            else putStrLn "Товар не был заменён."
    _ -> do
      -- Если товара этой категории нет, просто добавляем его
      addProductAndToCart conn userId categoryId newBoxId newPrice
      putStrLn $ "Товар " ++ newItemName ++ " добавлен в корзину."

-- Функция для добавления товара в таблицу продуктов и корзину
addProductAndToCart :: Connection -> Int -> Int -> Int -> Int -> IO ()
addProductAndToCart conn userId categoryId boxId price = do
  productId <- addProduct conn categoryId boxId price
  addToCart conn userId productId

-- Функция для добавления товара в таблицу продуктов и возвращения ID товара
addProduct :: Connection -> Int -> Int -> Int -> IO Int
addProduct conn categoryId boxId price = do
  execute conn (fromString "INSERT INTO products (category_id, box_id, price) VALUES (?,?,?)") (categoryId, boxId, price)
  [Only productId] <- query_ conn (fromString "SELECT last_insert_rowid()") :: IO [Only Int]
  return productId

-- Добавление товара в корзину
addToCart :: Connection -> Int -> Int -> IO ()
addToCart conn userId productId = do
  execute conn (fromString "INSERT INTO cart_items (cart_id, product_id) VALUES ((SELECT id FROM carts WHERE user_id = ?), ?)") (userId, productId)
  putStrLn $ "Товар с ID " ++ show productId ++ " добавлен в корзину."
  updateTotalPrice conn userId

-- Обновление общей стоимости корзины
updateTotalPrice :: Connection -> Int -> IO ()
updateTotalPrice conn userId = do
  [Only total] <- query conn (fromString "SELECT SUM(price) FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?)") (Only userId) :: IO [Only (Maybe Int)]
  let newTotal = maybe 0 id total
  execute conn (fromString "UPDATE carts SET total_price = ? WHERE user_id = ?") (newTotal, userId)
  putStrLn $ "Общая стоимость корзины обновлена до " ++ show newTotal ++ " руб."

-- Просмотр корзины
viewCart :: Connection -> Int -> IO ()
viewCart conn userId = do
  -- Очищаем консоль для Windows
  _ <- system "cls"

  putStrLn "----------------"
  putStrLn "КОРЗИНА:"
  putStrLn "----------------"
  -- Получаем все товары из корзины пользователя
  items <- query conn (fromString "SELECT box_id, category_id, price FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?)") (Only userId) :: IO [(Int, Int, Int)]

  -- Получаем список категорий из таблицы categories
  categories <- query_ conn (fromString "SELECT id, name FROM categories") :: IO [(Int, String)]

  -- Проходим по каждой категории и проверяем, добавлен ли компонент в корзину
  mapM_ (checkAndPrintComponent conn items) categories

  -- Рассчитываем и выводим общую стоимость
  totalPrice <- calculateTotalPrice items
  putStrLn "----------------"
  putStrLn $ "Общая стоимость: " ++ show totalPrice ++ " руб."
  putStrLn "----------------"

  -- Возвращаемся в главное меню
  mainMenu conn userId

-- Проверка, добавлен ли компонент в корзину
checkAndPrintComponent :: Connection -> [(Int, Int, Int)] -> (Int, String) -> IO ()
checkAndPrintComponent conn items (categoryId, componentName) = do
  let componentItem = filter (\(_, catId, _) -> catId == categoryId) items
  case componentItem of
    [(boxId, _, price)] -> do
      itemName <- getItemName conn boxId categoryId
      putStrLn $ itemName ++ " - " ++ show price ++ " руб."
    _ -> putStrLn $ componentName ++ " - отсутствует"

-- Расчёт общей стоимости товаров в корзине
calculateTotalPrice :: [(Int, Int, Int)] -> IO Int
calculateTotalPrice items = return $ sum $ map (\(_, _, price) -> price) items

-- Функция для получения названия товара по его категории и ID
getItemName :: Connection -> Int -> Int -> IO String
getItemName conn boxId categoryId = case categoryId of
  1 -> getNameFromTable conn "motherboards" boxId
  2 -> getNameFromTable conn "processors" boxId
  3 -> getNameFromTable conn "ram" boxId
  4 -> getNameFromTable conn "graphics_cards" boxId
  5 -> getNameFromTable conn "power_supplies" boxId
  6 -> getNameFromTable conn "storage" boxId
  _ -> return "Неизвестный товар"

-- Функция для получения названия товара из указанной таблицы
getNameFromTable :: Connection -> String -> Int -> IO String
getNameFromTable conn tableName boxId = do
  let queryStr = "SELECT name FROM " ++ tableName ++ " WHERE id = ?"
  result <- query conn (fromString queryStr) (Only boxId) :: IO [Only String]
  case result of
    [Only name] -> return name
    _           -> return "Товар не найден"

-- Создание корзины для пользователя
createCart :: Connection -> Int -> IO ()
createCart conn userId = do
  execute conn (fromString "INSERT INTO carts (user_id, total_price) VALUES (?, 0)") (Only userId)
  putStrLn "Корзина создана."

-- Основной поток программы
main :: IO ()
main = do
  initializeDB
  conn <- open "computer_store.db"
  let userId = 1  -- Для простоты предполагаем, что userId = 1
  createCart conn userId
  mainMenu conn userId
  close conn
