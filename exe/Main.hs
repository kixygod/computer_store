module Main where

import Database

main :: IO ()
main = do
  conn <- connectToDB
  createManufacturersTable conn
  putStrLn "Manufacturers table created."
  putStrLn "Press Enter to exit..."  -- Сообщение пользователю
  _ <- getLine  -- Ожидаем, пока пользователь нажмет Enter
  return ()
