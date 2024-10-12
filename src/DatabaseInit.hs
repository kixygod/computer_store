{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module DatabaseInit where

import           Data.String            (fromString)
import           Database.SQLite.Simple
import           System.Directory       (doesFileExist, removeFile)

initializeDB :: IO ()
initializeDB = do
  -- Проверяем, существует ли база данных
  dbExists <- doesFileExist "computer_store.db"

  -- Если существует, удаляем
  if dbExists
    then do
      -- putStrLn "Database exists. Deleting..."
      removeFile "computer_store.db"
    else return ()

  -- Открытие нового соединения с базой данных (после удаления)
  conn <- open "computer_store.db"

  -- Таблицы для категорий, производителей и пользователей
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY, name TEXT);")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS manufacturers (id INTEGER PRIMARY KEY, name TEXT);")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, email TEXT);")

  -- Таблицы для товаров
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS motherboards (id INTEGER PRIMARY KEY, name TEXT, socket TEXT, form_factor TEXT, supported_ram_type TEXT, ram_slots INTEGER, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS processors (id INTEGER PRIMARY KEY, name TEXT, cores INTEGER, clock_speed REAL, socket TEXT, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS ram (id INTEGER PRIMARY KEY, name TEXT, size INTEGER, type TEXT, frequency INTEGER, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS graphics_cards (id INTEGER PRIMARY KEY, name TEXT, memory_size INTEGER, memory_type TEXT, core_clock_speed REAL, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS power_supplies (id INTEGER PRIMARY KEY, name TEXT, wattage INTEGER, efficiency_rating TEXT, form_factor TEXT, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS storage (id INTEGER PRIMARY KEY, name TEXT, type TEXT, capacity INTEGER, interface TEXT, manufacture_id INTEGER, price INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(id));")

  -- Таблица Товар (Product)
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY, category_id INTEGER, box_id INTEGER, price INTEGER, FOREIGN KEY(category_id) REFERENCES categories(id));")

  -- Таблица Корзина (Cart)
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS cart_items (id INTEGER PRIMARY KEY, cart_id INTEGER, product_id INTEGER, FOREIGN KEY(cart_id) REFERENCES carts(id), FOREIGN KEY(product_id) REFERENCES products(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS carts (id INTEGER PRIMARY KEY, user_id INTEGER, total_price INTEGER, FOREIGN KEY(user_id) REFERENCES users(id));")

  -- Добавление данных
  -- Категории
  execute_ conn (fromString $
    "INSERT INTO categories (id, name) VALUES " ++
    "(1, 'Материнские платы'), " ++
    "(2, 'Процессоры'), " ++
    "(3, 'Оперативная память'), " ++
    "(4, 'Видеокарты'), " ++
    "(5, 'Блоки питания'), " ++
    "(6, 'Накопители данных');")


  -- Производители
  execute_ conn (fromString $
    "INSERT INTO manufacturers (id, name) VALUES " ++
    "(1, 'Intel'), " ++
    "(2, 'AMD'), " ++
    "(3, 'NVIDIA'), " ++
    "(4, 'Gigabyte'), " ++
    "(5, 'ASUS'), " ++
    "(6, 'MSI'), " ++
    "(7, 'Corsair'), " ++
    "(8, 'Crucial'), " ++
    "(9, 'Samsung'), " ++
    "(10, 'Seagate');")

  -- Материнские платы
  execute_ conn (fromString $
    "INSERT INTO motherboards (name, socket, form_factor, supported_ram_type, ram_slots, manufacture_id, price) VALUES " ++
    "('ASUS ROG Maximus XIII Hero', 'LGA1200', 'ATX', 'DDR4', 4, 5, 35000), " ++
    "('MSI MPG Z590 Gaming Carbon WiFi', 'LGA1200', 'ATX', 'DDR4', 4, 6, 27000), " ++
    "('Gigabyte Z590 AORUS Ultra', 'LGA1200', 'ATX', 'DDR4', 4, 4, 32000), " ++
    "('ASUS TUF Gaming X570-Plus', 'AM4', 'ATX', 'DDR4', 4, 5, 22000), " ++
    "('MSI MAG B550 TOMAHAWK', 'AM4', 'ATX', 'DDR4', 4, 6, 19000), " ++
    "('ASRock B450M Steel Legend', 'AM4', 'Micro-ATX', 'DDR4', 4, 4, 12000), " ++
    "('Gigabyte X570 AORUS Master', 'AM4', 'ATX', 'DDR4', 4, 4, 30000), " ++
    "('ASUS Prime Z390-A', 'LGA1151', 'ATX', 'DDR4', 4, 5, 18000), " ++
    "('MSI Z490-A PRO', 'LGA1200', 'ATX', 'DDR4', 4, 6, 15000), " ++
    "('Gigabyte B460M DS3H', 'LGA1200', 'Micro-ATX', 'DDR4', 2, 4, 9000), " ++
    "('ASUS ROG Strix Z490-E Gaming', 'LGA1200', 'ATX', 'DDR4', 4, 5, 26000), " ++
    "('MSI MEG X570 Unify', 'AM4', 'ATX', 'DDR4', 4, 6, 23000), " ++
    "('Gigabyte B550M AORUS Elite', 'AM4', 'Micro-ATX', 'DDR4', 4, 4, 14000), " ++
    "('ASRock X570 Phantom Gaming 4', 'AM4', 'ATX', 'DDR4', 4, 4, 16000), " ++
    "('ASUS Prime B450M-A', 'AM4', 'Micro-ATX', 'DDR4', 2, 5, 8000), " ++
    "('MSI B450 TOMAHAWK MAX', 'AM4', 'ATX', 'DDR4', 4, 6, 11000), " ++
    "('Gigabyte X299X Designare 10G', 'LGA2066', 'ATX', 'DDR4', 8, 4, 50000), " ++
    "('ASUS ROG Zenith II Extreme Alpha', 'sTRX4', 'ATX', 'DDR4', 8, 5, 80000), " ++
    "('MSI TRX40 PRO 10G', 'sTRX4', 'ATX', 'DDR4', 8, 6, 60000), " ++
    "('Gigabyte TRX40 AORUS Master', 'sTRX4', 'ATX', 'DDR4', 8, 4, 62000);")

  -- Процессоры
  execute_ conn (fromString $
    "INSERT INTO processors (name, cores, clock_speed, socket, manufacture_id, price) VALUES " ++
    "('Intel Core i9-12900K', 16, 3.2, 'LGA1200', 1, 53000), " ++
    "('Intel Core i7-12700K', 12, 3.6, 'LGA1200', 1, 43000), " ++
    "('Intel Core i5-12600K', 10, 3.7, 'LGA1200', 1, 32000), " ++
    "('AMD Ryzen 9 5950X', 16, 3.4, 'AM4', 2, 61000), " ++
    "('AMD Ryzen 7 5800X', 8, 3.8, 'AM4', 2, 38000), " ++
    "('AMD Ryzen 5 5600X', 6, 3.7, 'AM4', 2, 27000), " ++
    "('Intel Core i9-11900K', 8, 3.5, 'LGA1200', 1, 48000), " ++
    "('Intel Core i7-11700K', 8, 3.6, 'LGA1200', 1, 39000), " ++
    "('Intel Core i5-11600K', 6, 3.9, 'LGA1200', 1, 27000), " ++
    "('AMD Ryzen 9 5900X', 12, 3.7, 'AM4', 2, 54000), " ++
    "('AMD Ryzen 7 5700G', 8, 3.8, 'AM4', 2, 36000), " ++
    "('Intel Core i5-6500', 4, 3.2, 'LGA1151', 1, 5000), " ++
    "('AMD Ryzen 5 3500X', 6, 3.6, 'AM4', 2, 21000), " ++
    "('AMD Ryzen Threadripper 3960X', 24, 3.8, 'sTRX4', 2, 120000), " ++
    "('Intel Xeon W-2295', 18, 3.0, 'LGA2066', 1, 85000), " ++
    "('AMD Ryzen 5 3400G', 4, 3.7, 'AM4', 2, 18000), " ++
    "('Intel Core i9-10900X', 10, 3.7, 'LGA2066', 1, 62000), " ++
    "('AMD Ryzen 3 3100', 4, 3.6, 'AM4', 2, 13000), " ++
    "('Intel Core i7-10700K', 8, 3.8, 'LGA1200', 1, 37000), " ++
    "('AMD Ryzen Threadripper 3990X', 64, 2.9, 'sTRX4', 2, 300000);")

  -- ОЗУ
  execute_ conn (fromString $
    "INSERT INTO ram (name, size, type, frequency, manufacture_id, price) VALUES " ++
    "('Corsair Vengeance LPX 16GB', 16, 'DDR4', 3200, 7, 7000), " ++
    "('Kingston HyperX Fury 16GB', 16, 'DDR4', 3200, 8, 6500), " ++
    "('G.Skill Trident Z RGB 32GB', 32, 'DDR4', 3600, 7, 14000), " ++
    "('Corsair Dominator Platinum RGB 16GB', 16, 'DDR4', 3600, 7, 12000), " ++
    "('G.Skill Ripjaws V 16GB', 16, 'DDR4', 3200, 7, 7200), " ++
    "('Kingston HyperX Predator 32GB', 32, 'DDR4', 3600, 8, 16000), " ++
    "('Corsair Vengeance RGB Pro 32GB', 32, 'DDR4', 3200, 7, 14500), " ++
    "('G.Skill Trident Z Neo 64GB', 64, 'DDR4', 3600, 7, 34000), " ++
    "('Kingston Fury Beast 16GB', 16, 'DDR4', 3000, 8, 6000), " ++
    "('Corsair Vengeance LPX 8GB', 8, 'DDR4', 3200, 7, 3500), " ++
    "('G.Skill Ripjaws V 32GB', 32, 'DDR4', 3600, 7, 15000), " ++
    "('Kingston Fury Renegade 64GB', 64, 'DDR4', 3600, 8, 33000), " ++
    "('Corsair Dominator Platinum RGB 32GB', 32, 'DDR4', 3200, 7, 17000), " ++
    "('G.Skill Trident Z RGB 16GB', 16, 'DDR4', 3000, 7, 10000), " ++
    "('Kingston Fury Beast 32GB', 32, 'DDR4', 3200, 8, 12500), " ++
    "('Corsair Vengeance RGB Pro 64GB', 64, 'DDR4', 3600, 7, 32000), " ++
    "('G.Skill Ripjaws V 8GB', 8, 'DDR4', 3000, 7, 3000), " ++
    "('Corsair Vengeance LPX 32GB', 32, 'DDR4', 3600, 7, 15000), " ++
    "('Kingston HyperX Fury 8GB', 8, 'DDR4', 2400, 8, 4000), " ++
    "('G.Skill Trident Z Neo 32GB', 32, 'DDR4', 3200, 7, 18000);")


  -- Накопители данных
  execute_ conn (fromString $
    "INSERT INTO storage (name, type, capacity, interface, manufacture_id, price) VALUES " ++
    "('Samsung 970 EVO Plus 1TB', 'SSD', 1024, 'NVMe', 9, 15000), " ++
    "('Seagate Barracuda 2TB', 'HDD', 2048, 'SATA', 10, 4500), " ++
    "('Western Digital Blue 1TB', 'HDD', 1024, 'SATA', 10, 4000), " ++
    "('Samsung 860 EVO 500GB', 'SSD', 500, 'SATA', 9, 6000), " ++
    "('Crucial MX500 1TB', 'SSD', 1024, 'SATA', 8, 9000), " ++
    "('Samsung 980 Pro 1TB', 'SSD', 1024, 'NVMe', 9, 17000), " ++
    "('Western Digital Black 2TB', 'SSD', 2048, 'NVMe', 10, 18000), " ++
    "('Seagate FireCuda 520 1TB', 'SSD', 1024, 'NVMe', 10, 16000), " ++
    "('Crucial P5 1TB', 'SSD', 1024, 'NVMe', 8, 14000), " ++
    "('Samsung 970 EVO Plus 500GB', 'SSD', 500, 'NVMe', 9, 8000), " ++
    "('Seagate Barracuda 1TB', 'HDD', 1024, 'SATA', 10, 3500), " ++
    "('Western Digital Blue 2TB', 'HDD', 2048, 'SATA', 10, 5500), " ++
    "('Crucial MX500 500GB', 'SSD', 500, 'SATA', 8, 5000), " ++
    "('Samsung 980 Pro 500GB', 'SSD', 500, 'NVMe', 9, 11000), " ++
    "('Seagate FireCuda 2TB', 'HDD', 2048, 'SATA', 10, 8500), " ++
    "('Crucial P5 500GB', 'SSD', 500, 'NVMe', 8, 7000), " ++
    "('Western Digital Black 1TB', 'SSD', 1024, 'NVMe', 10, 12000), " ++
    "('Seagate FireCuda 520 2TB', 'SSD', 2048, 'NVMe', 10, 25000), " ++
    "('Crucial MX500 2TB', 'SSD', 2048, 'SATA', 8, 18000), " ++
    "('Samsung 970 EVO Plus 2TB', 'SSD', 2048, 'NVMe', 9, 30000);")


  -- Блоки питания
  execute_ conn (fromString $
    "INSERT INTO power_supplies (name, wattage, efficiency_rating, form_factor, manufacture_id, price) VALUES " ++
    "('Corsair RM850x', 850, '80+ Gold', 'ATX', 7, 12000), " ++
    "('Seasonic Focus GX-750', 750, '80+ Gold', 'ATX', 7, 11000), " ++
    "('EVGA SuperNOVA 850 G5', 850, '80+ Gold', 'ATX', 7, 12500), " ++
    "('Cooler Master V850', 850, '80+ Gold', 'ATX', 7, 11500), " ++
    "('Corsair SF750', 750, '80+ Platinum', 'SFX', 7, 13000), " ++
    "('MSI MPG A850GF', 850, '80+ Gold', 'ATX', 6, 11800), " ++
    "('Be Quiet! Straight Power 11', 850, '80+ Platinum', 'ATX', 7, 14500), " ++
    "('Seasonic Prime TX-850', 850, '80+ Titanium', 'ATX', 7, 16000), " ++
    "('Corsair RM750', 750, '80+ Gold', 'ATX', 7, 10000), " ++
    "('EVGA SuperNOVA 750 G3', 750, '80+ Gold', 'ATX', 7, 10500), " ++
    "('Cooler Master V750', 750, '80+ Gold', 'ATX', 7, 9800), " ++
    "('Corsair HX1200', 1200, '80+ Platinum', 'ATX', 7, 21000), " ++
    "('MSI MPG A750GF', 750, '80+ Gold', 'ATX', 6, 9500), " ++
    "('Seasonic Focus GX-850', 850, '80+ Gold', 'ATX', 7, 12500), " ++
    "('Be Quiet! Dark Power Pro 12', 1200, '80+ Titanium', 'ATX', 7, 25000), " ++
    "('Corsair SF600', 600, '80+ Platinum', 'SFX', 7, 9500), " ++
    "('EVGA SuperNOVA 1000 G5', 1000, '80+ Gold', 'ATX', 7, 18000), " ++
    "('Cooler Master MWE Gold 750', 750, '80+ Gold', 'ATX', 7, 8500), " ++
    "('Seasonic Prime PX-850', 850, '80+ Platinum', 'ATX', 7, 15000), " ++
    "('Corsair HX850', 850, '80+ Platinum', 'ATX', 7, 16000);")

  -- Видеокарты
  execute_ conn (fromString $
    "INSERT INTO graphics_cards (name, memory_size, memory_type, core_clock_speed, manufacture_id, price) VALUES " ++
    "('NVIDIA GeForce RTX 3080', 10, 'GDDR6X', 1.71, 3, 70000), " ++
    "('NVIDIA GeForce RTX 3090', 24, 'GDDR6X', 1.7, 3, 120000), " ++
    "('AMD Radeon RX 6900 XT', 16, 'GDDR6', 1.825, 2, 95000), " ++
    "('AMD Radeon RX 6800 XT', 16, 'GDDR6', 1.815, 2, 75000), " ++
    "('NVIDIA GeForce RTX 3070', 8, 'GDDR6', 1.73, 3, 60000), " ++
    "('NVIDIA GeForce RTX 3060 Ti', 8, 'GDDR6', 1.665, 3, 45000), " ++
    "('AMD Radeon RX 6700 XT', 12, 'GDDR6', 1.815, 2, 55000), " ++
    "('Gigabyte Radeon RX 6800', 16, 'GDDR6', 1.815, 4, 72000), " ++
    "('MSI GeForce RTX 3080 GAMING X TRIO', 10, 'GDDR6X', 1.71, 6, 73000), " ++
    "('ASUS ROG Strix GeForce RTX 3070', 8, 'GDDR6', 1.73, 5, 64000), " ++
    "('MSI Radeon RX 6900 XT GAMING X TRIO', 16, 'GDDR6', 1.825, 6, 98000), " ++
    "('NVIDIA GeForce RTX 3050', 8, 'GDDR6', 1.777, 3, 32000), " ++
    "('AMD Radeon RX 6600 XT', 8, 'GDDR6', 1.83, 2, 40000), " ++
    "('NVIDIA GeForce RTX 3060', 12, 'GDDR6', 1.78, 3, 38000), " ++
    "('Gigabyte GeForce RTX 3090', 24, 'GDDR6X', 1.7, 4, 125000), " ++
    "('ASUS TUF Gaming Radeon RX 6800 XT', 16, 'GDDR6', 1.815, 5, 78000), " ++
    "('MSI GeForce RTX 3090 SUPRIM X', 24, 'GDDR6X', 1.7, 6, 130000), " ++
    "('Gigabyte GeForce RTX 3080 Ti', 12, 'GDDR6X', 1.67, 4, 105000), " ++
    "('AMD Radeon RX 5700 XT', 8, 'GDDR6', 1.755, 2, 45000), " ++
    "('NVIDIA GeForce GTX 1660 Super', 6, 'GDDR6', 1.785, 3, 22000);")

  putStrLn "Курсовой проект выполнен Ерёминым Б.Е. гр. 421-2."
  putStrLn "База данных инициализирована и готова к работе."
  close conn
