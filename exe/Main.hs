{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad          (void)
import           Data.String            (fromString)
import           Database.SQLite.Simple
import           DatabaseInit           (initializeDB)

-- Show available processors
-- Show available processors
showProcessors :: Connection -> IO ()
showProcessors conn = do
  processors <- query_ conn (fromString "SELECT id, name, socket, cores, clock_speed , price FROM processors") :: IO [(Int, String, String, Int, Float, Int)]
  putStrLn "Список доступных процессоров:"
  mapM_ (\(processorId, name, socket, cores, clock_speed, price) ->
            putStrLn $ show processorId ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show cores ++ " ядер, "
                      ++ show clock_speed ++ " ГГц) - "
                      ++ show price ++ " руб.") processors

-- Select a processor, add to products, and show compatible motherboards
selectProcessor :: Connection -> Int -> IO ()
selectProcessor conn userId = do
  showProcessors conn
  putStrLn "Введите номер процессора, который хотите выбрать:"
  processorId <- readLn
  processor <- query conn (fromString "SELECT name, socket, price FROM processors WHERE id = ?") (Only processorId) :: IO [(String, String, Int)]
  case processor of
    [(name, socket, price)] -> do
      productId <- addProduct conn name 2 processorId price
      addToCart conn userId productId
      putStrLn $ "Товар " ++ name ++ " добавлен в таблицу Product и корзину."
      showCompatibleMotherboards conn userId socket
    _ -> putStrLn "Процессор не найден."

-- Show compatible motherboards with custom numbering and allow the user to add one to the cart
showCompatibleMotherboards :: Connection -> Int -> String -> IO ()
showCompatibleMotherboards conn userId socket = do
  -- Query for compatible motherboards
  motherboards <- query conn (fromString "SELECT id, name, socket, form_factor, supported_ram_type, ram_slots, price FROM motherboards WHERE socket = ?") (Only socket) :: IO [(Int, String, String, String, String, Int, Int)]

  putStrLn "Совместимые материнские платы:"

  -- Assign custom numbering to the motherboards
  let numberedMotherboards = zip [1..] motherboards

  -- Display motherboards with custom numbering
  mapM_ (\(index, (_, name, socket, form_factor, supported_ram_type, ram_slots, price)) ->
            putStrLn $ show index ++ ": " ++ name ++ " (" ++ socket ++ ", "
                      ++ show ram_slots ++ "x"
                      ++ supported_ram_type ++ ", "
                      ++ form_factor ++ ") - "
                      ++ show price ++ " руб.") numberedMotherboards

  -- Ask the user to select a motherboard by number
  putStrLn "Введите номер материнской платы, которую хотите выбрать:"
  moboIndex <- readLn

  -- Find the corresponding motherboard
  case lookup moboIndex numberedMotherboards of
    Just (moboId, name, _, _, _, _, price) -> do
      productId <- addProduct conn name 1 moboId price
      addToCart conn userId productId
      putStrLn $ "Материнская плата " ++ name ++ " добавлена в корзину."
    Nothing -> putStrLn "Неверный выбор материнской платы."


-- Add a product to the database and return the product ID
addProduct :: Connection -> String -> Int -> Int -> Int -> IO Int
addProduct conn productName categoryId boxId price = do
  execute conn (fromString "INSERT INTO products (category_id, box_id, price) VALUES (?,?,?)") (categoryId, boxId, price)
  [Only productId] <- query_ conn (fromString "SELECT last_insert_rowid()") :: IO [Only Int]
  return productId

-- Add a product to the cart
addToCart :: Connection -> Int -> Int -> IO ()
addToCart conn userId productId = do
  execute conn (fromString "INSERT INTO cart_items (cart_id, product_id) VALUES ((SELECT id FROM carts WHERE user_id = ?), ?)") (userId, productId)
  -- putStrLn $ "Товар с ID " ++ show productId ++ " добавлен в корзину."
  updateTotalPrice conn userId

-- Update the total price of the cart
updateTotalPrice :: Connection -> Int -> IO ()
updateTotalPrice conn userId = do
  [Only total] <- query conn (fromString "SELECT SUM(price) FROM products JOIN cart_items ON products.id = cart_items.product_id WHERE cart_id = (SELECT id FROM carts WHERE user_id = ?)") (Only userId) :: IO [Only (Maybe Int)]
  let newTotal = maybe 0 id total
  execute conn (fromString "UPDATE carts SET total_price = ? WHERE user_id = ?") (newTotal, userId)
  putStrLn $ "Общая стоимость корзины обновлена до " ++ show newTotal ++ " руб."

-- Create a shopping cart for a user
createCart :: Connection -> Int -> IO ()
createCart conn userId = do
  execute conn (fromString "INSERT INTO carts (user_id, total_price) VALUES (?, 0)") (Only userId)
  putStrLn "Корзина создана."

-- Main program flow
main :: IO ()
main = do
  initializeDB
  conn <- open "computer_store.db"
  let userId = 1  -- For simplicity, assume user ID is 1
  createCart conn userId
  selectProcessor conn userId
  close conn
  putStrLn "Press Enter to exit..."
  void getLine
