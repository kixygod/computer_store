{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad          (unless, void, when)
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

selectProcessor :: Connection -> Int -> IO ()
selectProcessor conn userId = do
  _ <- system "cls"
  -- Проверяем, есть ли материнская плата в корзине
  existingMobo <- query conn
    (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 1")
    (Only userId) :: IO [Only Int]

  let hasMobo = not (null existingMobo)

  processors <- if hasMobo then do
    let [Only moboBoxId] = existingMobo
    -- Получаем сокет материнской платы
    [Only moboSocket] <- query conn
      (fromString "SELECT socket FROM motherboards WHERE id = ?")
      (Only moboBoxId) :: IO [Only String]
    -- Выбираем только процессоры с таким же сокетом
    query conn
      (fromString "SELECT id, name, socket, cores, clock_speed, price FROM processors WHERE socket = ?")
      (Only moboSocket) :: IO [(Int, String, String, Int, Double, Int)]
  else do
    -- Если материнская плата не выбрана, показываем все процессоры
    query_ conn (fromString "SELECT id, name, socket, cores, clock_speed, price FROM processors") :: IO [(Int, String, String, Int, Double, Int)]

  putStrLn "Список доступных процессоров:"

  -- Пронумеровываем процессоры с 1
  let numberedProcessors = zip [1..] processors

  -- Если есть материнская плата в корзине, добавляем опцию "Перевыбрать процессор"
  let numberedProcessorsWithOption = if hasMobo
        then numberedProcessors ++ [(length numberedProcessors + 1, (-1, "Перевыбрать процессор", "", 0, 0.0, 0))]
        else numberedProcessors

  -- Отображаем процессоры с новыми номерами
  mapM_ (\(index, (processorId, name, socket, cores, clock_speed, price)) ->
            if processorId == -1
            then putStrLn $ show index ++ ": " ++ name
            else putStrLn $ show index ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show cores ++ " ядер, "
                      ++ show clock_speed ++ " ГГц) - "
                      ++ show price ++ " руб.") numberedProcessorsWithOption

  putStrLn "Введите номер процессора, который хотите выбрать:"
  input <- getLine
  case reads input of
    [(index, "")] -> do
      -- Ищем процессор по введенному индексу
      case lookup index numberedProcessorsWithOption of
        Just (processorId, _, _, _, _, _) -> do
          if processorId == -1
          then do
            -- Пользователь выбрал "Перевыбрать процессор"
            -- Удаляем материнскую плату из корзины, если она есть
            when hasMobo $ do
              removeComponent conn userId 1  -- Удаляем материнскую плату (категория 1)
              putStrLn "Материнская плата была удалена из корзины из-за возможной несовместимости. Пожалуйста, выберите материнскую плату заново."
            -- Показать полный список процессоров
            processorsFull <- query_ conn (fromString "SELECT id, name, socket, cores, clock_speed, price FROM processors") :: IO [(Int, String, String, Int, Double, Int)]
            -- Пронумеровываем процессоры с 1
            let numberedProcessorsFull = zip [1..] processorsFull
            -- Отображаем процессоры с новыми номерами
            mapM_ (\(idx, (procId, name, socket, cores, clock_speed, price)) ->
                      putStrLn $ show idx ++ ": " ++ name ++ " (" ++ socket ++ ", "
                                ++ show cores ++ " ядер, "
                                ++ show clock_speed ++ " ГГц) - "
                                ++ show price ++ " руб.") numberedProcessorsFull
            putStrLn "Введите номер процессора, который хотите выбрать:"
            input2 <- getLine
            case reads input2 of
              [(idx2, "")] -> do
                case lookup idx2 numberedProcessorsFull of
                  Just (procId, _, procSocket, _, _, _) -> do
                    -- Проверяем совместимость с материнской платой (если она есть)
                    incompatibleMobo <- checkIncompatibility conn userId 1 procSocket
                    when incompatibleMobo $ do
                      putStrLn "Выбранный процессор несовместим с материнской платой в вашей корзине. Материнская плата была удалена. Пожалуйста, выберите совместимую материнскую плату."
                      removeComponent conn userId 1
                    -- Добавляем процессор в корзину
                    processor <- query conn (fromString "SELECT name, price FROM processors WHERE id = ?") (Only procId) :: IO [(String, Int)]
                    case processor of
                      [(name, price)] -> addToCartWithCheck conn userId 2 procId price
                      _               -> putStrLn "Процессор не найден."
                  Nothing -> putStrLn "Неверный выбор процессора."
              _ -> putStrLn "Пожалуйста, введите корректный номер."
          else do
            -- Обычный выбор процессора
            processor <- query conn (fromString "SELECT name, price FROM processors WHERE id = ?") (Only processorId) :: IO [(String, Int)]
            case processor of
              [(name, price)] -> addToCartWithCheck conn userId 2 processorId price
              _               -> putStrLn "Процессор не найден."
        Nothing -> putStrLn "Неверный выбор процессора."
    _ -> putStrLn "Пожалуйста, введите корректный номер."
  mainMenu conn userId

selectMotherboard :: Connection -> Int -> IO ()
selectMotherboard conn userId = do
  _ <- system "cls"
  -- Проверяем, есть ли процессор и оперативная память в корзине
  existingProcessor <- query conn
    (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 2")
    (Only userId) :: IO [Only Int]

  existingRam <- query conn
    (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 3")
    (Only userId) :: IO [Only Int]

  let hasProcessor = not (null existingProcessor)
  let hasRam = not (null existingRam)

  motherboards <- case (existingProcessor, existingRam) of
    ([Only processorBoxId], [Only ramBoxId]) -> do
      -- Получаем сокет процессора и тип оперативной памяти
      [Only processorSocket] <- query conn
        (fromString "SELECT socket FROM processors WHERE id = ?")
        (Only processorBoxId) :: IO [Only String]
      [Only ramType] <- query conn
        (fromString "SELECT type FROM ram WHERE id = ?")
        (Only ramBoxId) :: IO [Only String]
      -- Выбираем материнские платы, совместимые с процессором и оперативной памятью
      query conn
        (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards WHERE socket = ? AND supported_ram_type = ?")
        (processorSocket, ramType) :: IO [(Int, String, String, String, String, Int, Int)]
    ([Only processorBoxId], _) -> do
      -- Получаем сокет процессора
      [Only processorSocket] <- query conn
        (fromString "SELECT socket FROM processors WHERE id = ?")
        (Only processorBoxId) :: IO [Only String]
      -- Выбираем материнские платы, совместимые с процессором
      query conn
        (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards WHERE socket = ?")
        (Only processorSocket) :: IO [(Int, String, String, String, String, Int, Int)]
    (_, [Only ramBoxId]) -> do
      -- Получаем тип оперативной памяти
      [Only ramType] <- query conn
        (fromString "SELECT type FROM ram WHERE id = ?")
        (Only ramBoxId) :: IO [Only String]
      -- Выбираем материнские платы, совместимые с оперативной памятью
      query conn
        (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards WHERE supported_ram_type = ?")
        (Only ramType) :: IO [(Int, String, String, String, String, Int, Int)]
    _ -> do
      -- Если ни процессор, ни оперативная память не выбраны, показываем все материнские платы
      query_ conn (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards") :: IO [(Int, String, String, String, String, Int, Int)]

  putStrLn "Список доступных материнских плат:"

  -- Пронумеровываем материнские платы с 1
  let numberedMotherboards = zip [1..] motherboards

  -- Если есть процессор или оперативная память в корзине, добавляем опцию "Перевыбрать материнскую плату"
  let addOption = hasProcessor || hasRam
  let numberedMotherboardsWithOption = if addOption
        then numberedMotherboards ++ [(length numberedMotherboards + 1, (-1, "Перевыбрать материнскую плату", "", "", "", 0, 0))]
        else numberedMotherboards

  -- Отображаем материнские платы с новыми номерами
  mapM_ (\(index, (moboId, name, socket, form_factor, supported_ram_type, ram_slots, price)) ->
            if moboId == -1
            then putStrLn $ show index ++ ": " ++ name
            else putStrLn $ show index ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show ram_slots ++ "x"
                      ++ supported_ram_type ++ ", "
                      ++ form_factor ++ ") - "
                      ++ show price ++ " руб.") numberedMotherboardsWithOption

  putStrLn "Введите номер материнской платы, которую хотите выбрать:"
  input <- getLine
  case reads input of
    [(index, "")] -> do
      -- Ищем материнскую плату по введенному индексу
      case lookup index numberedMotherboardsWithOption of
        Just (moboId, _, _, _, _, _, _) -> do
          if moboId == -1
          then do
            -- Пользователь выбрал "Перевыбрать материнскую плату"
            -- Удаляем процессор и оперативную память из корзины, если они есть
            when hasProcessor $ do
              removeComponent conn userId 2  -- Удаляем процессор (категория 2)
              putStrLn "Процессор был удален из корзины из-за возможной несовместимости. Пожалуйста, выберите процессор заново."
            when hasRam $ do
              removeComponent conn userId 3  -- Удаляем оперативную память (категория 3)
              putStrLn "Оперативная память была удалена из корзины из-за возможной несовместимости. Пожалуйста, выберите оперативную память заново."
            -- Показать полный список материнских плат
            motherboardsFull <- query_ conn (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards") :: IO [(Int, String, String, String, String, Int, Int)]
            -- Пронумеровываем материнские платы с 1
            let numberedMotherboardsFull = zip [1..] motherboardsFull
            -- Отображаем материнские платы с новыми номерами
            mapM_ (\(idx, (moboId', name, socket, form_factor, supported_ram_type, ram_slots, price)) ->
                      putStrLn $ show idx ++ ": " ++ name ++ " (" ++ socket ++ ", "
                                ++ show ram_slots ++ "x"
                                ++ supported_ram_type ++ ", "
                                ++ form_factor ++ ") - "
                                ++ show price ++ " руб.") numberedMotherboardsFull
            putStrLn "Введите номер материнской платы, которую хотите выбрать:"
            input2 <- getLine
            case reads input2 of
              [(idx2, "")] -> do
                case lookup idx2 numberedMotherboardsFull of
                  Just (moboId', _, moboSocket, _, moboRamType, _, _) -> do
                    -- Проверяем совместимость с процессором и оперативной памятью
                    incompatibleProcessor <- checkIncompatibility conn userId 2 moboSocket
                    when incompatibleProcessor $ do
                      removeComponent conn userId 2
                      putStrLn "Выбранная материнская плата несовместима с процессором в вашей корзине. Процессор был удален. Пожалуйста, выберите совместимый процессор."
                    incompatibleRam <- checkRamIncompatibility conn userId moboRamType
                    when incompatibleRam $ do
                      removeComponent conn userId 3
                      putStrLn "Выбранная материнская плата несовместима с оперативной памятью в вашей корзине. Оперативная память была удалена. Пожалуйста, выберите совместимую оперативную память."
                    -- Добавляем материнскую плату в корзину
                    mobo <- query conn (fromString "SELECT name, price FROM motherboards WHERE id = ?") (Only moboId') :: IO [(String, Int)]
                    case mobo of
                      [(name, price)] -> addToCartWithCheck conn userId 1 moboId' price
                      _               -> putStrLn "Материнская плата не найдена."
                  Nothing -> putStrLn "Неверный выбор материнской платы."
              _ -> putStrLn "Пожалуйста, введите корректный номер."
          else do
            -- Обычный выбор материнской платы
            mobo <- query conn (fromString "SELECT name, price FROM motherboards WHERE id = ?") (Only moboId) :: IO [(String, Int)]
            case mobo of
              [(name, price)] -> addToCartWithCheck conn userId 1 moboId price
              _               -> putStrLn "Материнская плата не найдена."
        Nothing -> putStrLn "Неверный выбор материнской платы."
    _ -> putStrLn "Пожалуйста, введите корректный номер."
  mainMenu conn userId

selectRam :: Connection -> Int -> IO ()
selectRam conn userId = do
  _ <- system "cls"
  -- Проверяем, есть ли материнская плата в корзине
  existingMobo <- query conn
    (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 1")
    (Only userId) :: IO [Only Int]

  ramList <- case existingMobo of
    [Only moboBoxId] -> do
      -- Получаем поддерживаемый тип оперативной памяти материнской платы
      [Only supportedRamType] <- query conn
        (fromString "SELECT supported_ram_type FROM motherboards WHERE id = ?")
        (Only moboBoxId) :: IO [Only String]
      -- Выбираем только оперативную память с таким же типом
      query conn
        (fromString "SELECT id, name, size, type, frequency, price FROM ram WHERE type = ?")
        (Only supportedRamType) :: IO [(Int, String, Int, String, Int, Int)]
    _ -> do
      -- Если материнская плата не выбрана, показываем всю оперативную память
      query_ conn (fromString "SELECT id, name, size, type, frequency, price FROM ram") :: IO [(Int, String, Int, String, Int, Int)]

  putStrLn "Список доступной оперативной памяти:"

  -- Пронумеровываем оперативную память с 1
  let numberedRam = zip [1..] ramList

  -- Отображаем оперативную память с новыми номерами
  mapM_ (\(index, (_, name, size, ram_type, frequency, price)) ->
            putStrLn $ show index ++ ": " ++ name ++ " (" ++ show size ++ " ГБ, "
                      ++ ram_type ++ ", "
                      ++ show frequency ++ " МГц) - "
                      ++ show price ++ " руб.") numberedRam

  putStrLn "Введите номер оперативной памяти, которую хотите выбрать:"
  input <- getLine
  case reads input of
    [(index, "")] -> do
      -- Ищем оперативную память по введенному индексу
      case lookup index numberedRam of
        Just (ramId, _, _, _, _, _) -> do
          selectedRam <- query conn (fromString "SELECT name, price FROM ram WHERE id = ?") (Only ramId) :: IO [(String, Int)]
          case selectedRam of
            [(name, price)] -> addToCartWithCheck conn userId 3 ramId price
            _               -> putStrLn "Оперативная память не найдена."
        Nothing -> putStrLn "Неверный выбор оперативной памяти."
    _ -> putStrLn "Пожалуйста, введите корректный номер."
  mainMenu conn userId

-- Выбор видеокарты
selectGraphicsCard :: Connection -> Int -> IO ()
selectGraphicsCard conn userId = do
  _ <- system "cls"
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
  _ <- system "cls"
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
  _ <- system "cls"
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

-- Проверка несовместимости процессора или материнской платы
checkIncompatibility :: Connection -> Int -> Int -> String -> IO Bool
checkIncompatibility conn userId categoryId socket = do
  case categoryId of
    1 -> do  -- Проверка несовместимости материнской платы с процессором
      existingProcessor <- query conn
        (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 2")
        (Only userId) :: IO [Only Int]
      if null existingProcessor
        then return False
        else do
          let [Only processorBoxId] = existingProcessor
          [Only processorSocket] <- query conn
            (fromString "SELECT socket FROM processors WHERE id = ?")
            (Only processorBoxId) :: IO [Only String]
          return (processorSocket /= socket)
    2 -> do  -- Проверка несовместимости процессора с материнской платой
      existingMobo <- query conn
        (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 1")
        (Only userId) :: IO [Only Int]
      if null existingMobo
        then return False
        else do
          let [Only moboBoxId] = existingMobo
          [Only moboSocket] <- query conn
            (fromString "SELECT socket FROM motherboards WHERE id = ?")
            (Only moboBoxId) :: IO [Only String]
          return (moboSocket /= socket)
    _ -> return False

-- Проверка несовместимости оперативной памяти с материнской платой
checkRamIncompatibility :: Connection -> Int -> String -> IO Bool
checkRamIncompatibility conn userId ramType = do
  existingRam <- query conn
    (fromString "SELECT box_id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = 3")
    (Only userId) :: IO [Only Int]
  if null existingRam
    then return False
    else do
      let [Only ramBoxId] = existingRam
      [Only existingRamType] <- query conn
        (fromString "SELECT type FROM ram WHERE id = ?")
        (Only ramBoxId) :: IO [Only String]
      return (existingRamType /= ramType)

-- Функция для удаления компонента из корзины по категории
removeComponent :: Connection -> Int -> Int -> IO ()
removeComponent conn userId categoryId = do
  existingItem <- query conn
    (fromString "SELECT products.id FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?) AND category_id = ?")
    (userId, categoryId) :: IO [Only Int]
  mapM_ (\(Only productId) -> execute conn (fromString "DELETE FROM cart_items WHERE product_id = ?") (Only productId)) existingItem
  updateTotalPrice conn userId


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
