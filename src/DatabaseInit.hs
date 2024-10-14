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
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS categories (category_id INTEGER PRIMARY KEY, name TEXT);")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS manufacturers (manufacture_id INTEGER PRIMARY KEY, name TEXT);")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, email TEXT, password TEXT);")

  -- Таблицы для товаров
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS motherboards (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, socket_type TEXT, form_factor TEXT, ram_type TEXT, max_ram INTEGER, ram_slots INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS processors (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, socket_type TEXT, clock_speed REAL, cores INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS ram (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, ram_type TEXT, capacity INTEGER, frequency INTEGER, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS graphics_cards (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, memory_size INTEGER, interface TEXT, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS power_supplies (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, power_output INTEGER, efficiency_rating TEXT, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS storage (component_id INTEGER PRIMARY KEY, name TEXT, manufacture_id INTEGER, storage_type TEXT, capacity INTEGER, interface TEXT, FOREIGN KEY(manufacture_id) REFERENCES manufacturers(manufacture_id));")

  -- Таблица Товар (Product)
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY, cart_id INTEGER, category_id INTEGER, box_id INTEGER, price INTEGER, FOREIGN KEY(cart_id) REFERENCES carts(id), FOREIGN KEY(category_id) REFERENCES categories(category_id));")
  -- Таблица Корзина (Cart)
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS cart_items (id INTEGER PRIMARY KEY, cart_id INTEGER, product_id INTEGER, FOREIGN KEY(cart_id) REFERENCES carts(id), FOREIGN KEY(product_id) REFERENCES products(id));")
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS carts (id INTEGER PRIMARY KEY, user_id INTEGER, total_price INTEGER, FOREIGN KEY(user_id) REFERENCES users(id));")

  -- Добавление данных
  -- Категории
  execute_ conn (fromString $
    "INSERT INTO categories (category_id, name) VALUES " ++
    "(1, 'Материнские платы'), " ++
    "(2, 'Процессоры'), " ++
    "(3, 'Оперативная память'), " ++
    "(4, 'Видеокарты'), " ++
    "(5, 'Блоки питания'), " ++
    "(6, 'Накопители данных');")


  -- Производители
  execute_ conn (fromString $
    "INSERT INTO manufacturers (manufacture_id, name) VALUES " ++
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

  execute_ conn (fromString $
    "INSERT INTO users (id, name, email, password) VALUES " ++
    "(1, 'User User', 'user@example.com', '123'), " ++
    "(2, 'Bogdan Eremin', 'bogdan.eremin@example.com', '123');")

  -- Материнские платы
  execute_ conn (fromString $
    "INSERT INTO motherboards (name, socket_type, form_factor, ram_type, ram_slots, manufacture_id, max_ram) VALUES " ++
    "('ASUS ROG Maximus XIII Hero', 'LGA1200', 'ATX', 'DDR4', 4, 5, 128), " ++
    "('MSI MPG Z590 Gaming Carbon WiFi', 'LGA1200', 'ATX', 'DDR4', 4, 6, 128), " ++
    "('Gigabyte Z590 AORUS Ultra', 'LGA1200', 'ATX', 'DDR4', 4, 4, 128), " ++
    "('ASUS TUF Gaming X570-Plus', 'AM4', 'ATX', 'DDR4', 4, 5, 128), " ++
    "('MSI MAG B550 TOMAHAWK', 'AM4', 'ATX', 'DDR4', 4, 6, 128), " ++
    "('ASRock B450M Steel Legend', 'AM4', 'Micro-ATX', 'DDR4', 4, 4, 64), " ++
    "('Gigabyte X570 AORUS Master', 'AM4', 'ATX', 'DDR4', 4, 4, 128), " ++
    "('ASUS Prime Z390-A', 'LGA1151', 'ATX', 'DDR4', 4, 5, 64), " ++
    "('MSI Z490-A PRO', 'LGA1200', 'ATX', 'DDR4', 4, 6, 128), " ++
    "('Gigabyte B460M DS3H', 'LGA1200', 'Micro-ATX', 'DDR4', 2, 4, 64), " ++
    "('ASUS ROG Strix Z490-E Gaming', 'LGA1200', 'ATX', 'DDR4', 4, 5, 128), " ++
    "('MSI MEG X570 Unify', 'AM4', 'ATX', 'DDR4', 4, 6, 128), " ++
    "('Gigabyte B550M AORUS Elite', 'AM4', 'Micro-ATX', 'DDR4', 4, 4, 128), " ++
    "('ASRock X570 Phantom Gaming 4', 'AM4', 'ATX', 'DDR4', 4, 4, 128), " ++
    "('ASUS Prime B450M-A', 'AM4', 'Micro-ATX', 'DDR4', 2, 5, 64), " ++
    "('MSI B450 TOMAHAWK MAX', 'AM4', 'ATX', 'DDR4', 4, 6, 64), " ++
    "('Gigabyte X299X Designare 10G', 'LGA2066', 'ATX', 'DDR4', 8, 4, 256), " ++
    "('ASUS ROG Zenith II Extreme Alpha', 'sTRX4', 'ATX', 'DDR4', 8, 5, 256), " ++
    "('MSI TRX40 PRO 10G', 'sTRX4', 'ATX', 'DDR4', 8, 6, 256), " ++
    "('Gigabyte TRX40 AORUS Master', 'sTRX4', 'ATX', 'DDR4', 8, 4, 256);")

  -- Добавление в таблицу Товары (category_id = 1)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(1, 1, 35000), " ++   -- ASUS ROG Maximus XIII Hero
    "(1, 2, 27000), " ++   -- MSI MPG Z590 Gaming Carbon WiFi
    "(1, 3, 32000), " ++   -- Gigabyte Z590 AORUS Ultra
    "(1, 4, 22000), " ++   -- ASUS TUF Gaming X570-Plus
    "(1, 5, 19000), " ++   -- MSI MAG B550 TOMAHAWK
    "(1, 6, 12000), " ++   -- ASRock B450M Steel Legend
    "(1, 7, 30000), " ++   -- Gigabyte X570 AORUS Master
    "(1, 8, 18000), " ++   -- ASUS Prime Z390-A
    "(1, 9, 15000), " ++   -- MSI Z490-A PRO
    "(1, 10, 9000), " ++   -- Gigabyte B460M DS3H
    "(1, 11, 26000), " ++  -- ASUS ROG Strix Z490-E Gaming
    "(1, 12, 23000), " ++  -- MSI MEG X570 Unify
    "(1, 13, 14000), " ++  -- Gigabyte B550M AORUS Elite
    "(1, 14, 16000), " ++  -- ASRock X570 Phantom Gaming 4
    "(1, 15, 8000), " ++   -- ASUS Prime B450M-A
    "(1, 16, 11000), " ++  -- MSI B450 TOMAHAWK MAX
    "(1, 17, 50000), " ++  -- Gigabyte X299X Designare 10G
    "(1, 18, 80000), " ++  -- ASUS ROG Zenith II Extreme Alpha
    "(1, 19, 60000), " ++  -- MSI TRX40 PRO 10G
    "(1, 20, 62000);"      -- Gigabyte TRX40 AORUS Master
    )

  -- Процессоры
  execute_ conn (fromString $
    "INSERT INTO processors (name, cores, clock_speed, socket_type, manufacture_id) VALUES " ++
    "('Intel Core i9-12900K', 16, 3.2, 'LGA1200', 1), " ++
    "('Intel Core i7-12700K', 12, 3.6, 'LGA1200', 1), " ++
    "('Intel Core i5-12600K', 10, 3.7, 'LGA1200', 1), " ++
    "('AMD Ryzen 9 5950X', 16, 3.4, 'AM4', 2), " ++
    "('AMD Ryzen 7 5800X', 8, 3.8, 'AM4', 2), " ++
    "('AMD Ryzen 5 5600X', 6, 3.7, 'AM4', 2), " ++
    "('Intel Core i9-11900K', 8, 3.5, 'LGA1200', 1), " ++
    "('Intel Core i7-11700K', 8, 3.6, 'LGA1200', 1), " ++
    "('Intel Core i5-11600K', 6, 3.9, 'LGA1200', 1), " ++
    "('AMD Ryzen 9 5900X', 12, 3.7, 'AM4', 2), " ++
    "('AMD Ryzen 7 5700G', 8, 3.8, 'AM4', 2), " ++
    "('Intel Core i5-6500', 4, 3.2, 'LGA1151', 1), " ++
    "('AMD Ryzen 5 3500X', 6, 3.6, 'AM4', 2), " ++
    "('AMD Ryzen Threadripper 3960X', 24, 3.8, 'sTRX4', 2), " ++
    "('Intel Xeon W-2295', 18, 3.0, 'LGA2066', 1), " ++
    "('AMD Ryzen 5 3400G', 4, 3.7, 'AM4', 2), " ++
    "('Intel Core i9-10900X', 10, 3.7, 'LGA2066', 1), " ++
    "('AMD Ryzen 3 3100', 4, 3.6, 'AM4', 2), " ++
    "('Intel Core i7-10700K', 8, 3.8, 'LGA1200', 1), " ++
    "('AMD Ryzen Threadripper 3990X', 64, 2.9, 'sTRX4', 2);")

  -- Добавление в таблицу Товары (category_id = 2)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(2, 1, 53000), " ++    -- Intel Core i9-12900K
    "(2, 2, 43000), " ++    -- Intel Core i7-12700K
    "(2, 3, 32000), " ++    -- Intel Core i5-12600K
    "(2, 4, 61000), " ++    -- AMD Ryzen 9 5950X
    "(2, 5, 38000), " ++    -- AMD Ryzen 7 5800X
    "(2, 6, 27000), " ++    -- AMD Ryzen 5 5600X
    "(2, 7, 48000), " ++    -- Intel Core i9-11900K
    "(2, 8, 39000), " ++    -- Intel Core i7-11700K
    "(2, 9, 27000), " ++    -- Intel Core i5-11600K
    "(2, 10, 54000), " ++   -- AMD Ryzen 9 5900X
    "(2, 11, 36000), " ++   -- AMD Ryzen 7 5700G
    "(2, 12, 5000), " ++    -- Intel Core i5-6500
    "(2, 13, 21000), " ++   -- AMD Ryzen 5 3500X
    "(2, 14, 120000), " ++  -- AMD Ryzen Threadripper 3960X
    "(2, 15, 85000), " ++   -- Intel Xeon W-2295
    "(2, 16, 18000), " ++   -- AMD Ryzen 5 3400G
    "(2, 17, 62000), " ++   -- Intel Core i9-10900X
    "(2, 18, 13000), " ++   -- AMD Ryzen 3 3100
    "(2, 19, 37000), " ++   -- Intel Core i7-10700K
    "(2, 20, 300000);"      -- AMD Ryzen Threadripper 3990X
    )

  -- ОЗУ
  execute_ conn (fromString $
    "INSERT INTO ram (name, ram_type, capacity, frequency, manufacture_id) VALUES " ++
    "('Corsair Vengeance LPX 16GB', 'DDR4', 16, 3200, 7), " ++
    "('Kingston HyperX Fury 16GB', 'DDR4', 16, 3200, 8), " ++
    "('G.Skill Trident Z RGB 32GB', 'DDR4', 32, 3600, 7), " ++
    "('Corsair Dominator Platinum RGB 16GB', 'DDR4', 16, 3600, 7), " ++
    "('G.Skill Ripjaws V 16GB', 'DDR4', 16, 3200, 7), " ++
    "('Kingston HyperX Predator 32GB', 'DDR4', 32, 3600, 8), " ++
    "('Corsair Vengeance RGB Pro 32GB', 'DDR4', 32, 3200, 7), " ++
    "('G.Skill Trident Z Neo 64GB', 'DDR4', 64, 3600, 7), " ++
    "('Kingston Fury Beast 16GB', 'DDR4', 16, 3000, 8), " ++
    "('Corsair Vengeance LPX 8GB', 'DDR4', 8, 3200, 7), " ++
    "('G.Skill Ripjaws V 32GB', 'DDR4', 32, 3600, 7), " ++
    "('Kingston Fury Renegade 64GB', 'DDR4', 64, 3600, 8), " ++
    "('Corsair Dominator Platinum RGB 32GB', 'DDR4', 32, 3200, 7), " ++
    "('G.Skill Trident Z RGB 16GB', 'DDR4', 16, 3000, 7), " ++
    "('Kingston Fury Beast 32GB', 'DDR4', 32, 3200, 8), " ++
    "('Corsair Vengeance RGB Pro 64GB', 'DDR4', 64, 3600, 7), " ++
    "('G.Skill Ripjaws V 8GB', 'DDR4', 8, 3000, 7), " ++
    "('Corsair Vengeance LPX 32GB', 'DDR4', 32, 3600, 7), " ++
    "('Kingston HyperX Fury 8GB', 'DDR4', 8, 2400, 8), " ++
    "('G.Skill Trident Z Neo 32GB', 'DDR4', 32, 3200, 7);")

  -- Добавление в таблицу Товары (category_id = 3)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(3, 1, 7000), " ++    -- Corsair Vengeance LPX 16GB
    "(3, 2, 6500), " ++    -- Kingston HyperX Fury 16GB
    "(3, 3, 14000), " ++   -- G.Skill Trident Z RGB 32GB
    "(3, 4, 12000), " ++   -- Corsair Dominator Platinum RGB 16GB
    "(3, 5, 7200), " ++    -- G.Skill Ripjaws V 16GB
    "(3, 6, 16000), " ++   -- Kingston HyperX Predator 32GB
    "(3, 7, 14500), " ++   -- Corsair Vengeance RGB Pro 32GB
    "(3, 8, 34000), " ++   -- G.Skill Trident Z Neo 64GB
    "(3, 9, 6000), " ++    -- Kingston Fury Beast 16GB
    "(3, 10, 3500), " ++   -- Corsair Vengeance LPX 8GB
    "(3, 11, 15000), " ++  -- G.Skill Ripjaws V 32GB
    "(3, 12, 33000), " ++  -- Kingston Fury Renegade 64GB
    "(3, 13, 17000), " ++  -- Corsair Dominator Platinum RGB 32GB
    "(3, 14, 10000), " ++  -- G.Skill Trident Z RGB 16GB
    "(3, 15, 12500), " ++  -- Kingston Fury Beast 32GB
    "(3, 16, 32000), " ++  -- Corsair Vengeance RGB Pro 64GB
    "(3, 17, 3000), " ++   -- G.Skill Ripjaws V 8GB
    "(3, 18, 15000), " ++  -- Corsair Vengeance LPX 32GB
    "(3, 19, 4000), " ++   -- Kingston HyperX Fury 8GB
    "(3, 20, 18000);"      -- G.Skill Trident Z Neo 32GB
    )

  -- Видеокарты
  execute_ conn (fromString $
    "INSERT INTO graphics_cards (name, memory_size, interface, manufacture_id) VALUES " ++
    "('NVIDIA GeForce RTX 3080', 10, 'GDDR6X', 3), " ++
    "('NVIDIA GeForce RTX 3090', 24, 'GDDR6X', 3), " ++
    "('AMD Radeon RX 6900 XT', 16, 'GDDR6', 2), " ++
    "('AMD Radeon RX 6800 XT', 16, 'GDDR6', 2), " ++
    "('NVIDIA GeForce RTX 3070', 8, 'GDDR6', 3), " ++
    "('NVIDIA GeForce RTX 3060 Ti', 8, 'GDDR6', 3), " ++
    "('AMD Radeon RX 6700 XT', 12, 'GDDR6', 2), " ++
    "('Gigabyte Radeon RX 6800', 16, 'GDDR6', 4), " ++
    "('MSI GeForce RTX 3080 GAMING X TRIO', 10, 'GDDR6X', 6), " ++
    "('ASUS ROG Strix GeForce RTX 3070', 8, 'GDDR6', 5), " ++
    "('MSI Radeon RX 6900 XT GAMING X TRIO', 16, 'GDDR6', 6), " ++
    "('NVIDIA GeForce RTX 3050', 8, 'GDDR6', 3), " ++
    "('AMD Radeon RX 6600 XT', 8, 'GDDR6', 2), " ++
    "('NVIDIA GeForce RTX 3060', 12, 'GDDR6', 3), " ++
    "('Gigabyte GeForce RTX 3090', 24, 'GDDR6X', 4), " ++
    "('ASUS TUF Gaming Radeon RX 6800 XT', 16, 'GDDR6', 5), " ++
    "('MSI GeForce RTX 3090 SUPRIM X', 24, 'GDDR6X', 6), " ++
    "('Gigabyte GeForce RTX 3080 Ti', 12, 'GDDR6X', 4), " ++
    "('AMD Radeon RX 5700 XT', 8, 'GDDR6', 2), " ++
    "('NVIDIA GeForce GTX 1660 Super', 6, 'GDDR6', 3);")

  -- Добавление в таблицу Товары (category_id = 4)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(4, 1, 70000), " ++   -- NVIDIA GeForce RTX 3080
    "(4, 2, 120000), " ++  -- NVIDIA GeForce RTX 3090
    "(4, 3, 95000), " ++   -- AMD Radeon RX 6900 XT
    "(4, 4, 75000), " ++   -- AMD Radeon RX 6800 XT
    "(4, 5, 60000), " ++   -- NVIDIA GeForce RTX 3070
    "(4, 6, 45000), " ++   -- NVIDIA GeForce RTX 3060 Ti
    "(4, 7, 55000), " ++   -- AMD Radeon RX 6700 XT
    "(4, 8, 72000), " ++   -- Gigabyte Radeon RX 6800
    "(4, 9, 73000), " ++   -- MSI GeForce RTX 3080 GAMING X TRIO
    "(4, 10, 64000), " ++  -- ASUS ROG Strix GeForce RTX 3070
    "(4, 11, 98000), " ++  -- MSI Radeon RX 6900 XT GAMING X TRIO
    "(4, 12, 32000), " ++  -- NVIDIA GeForce RTX 3050
    "(4, 13, 40000), " ++  -- AMD Radeon RX 6600 XT
    "(4, 14, 38000), " ++  -- NVIDIA GeForce RTX 3060
    "(4, 15, 125000), " ++ -- Gigabyte GeForce RTX 3090
    "(4, 16, 78000), " ++  -- ASUS TUF Gaming Radeon RX 6800 XT
    "(4, 17, 130000), " ++ -- MSI GeForce RTX 3090 SUPRIM X
    "(4, 18, 105000), " ++ -- Gigabyte GeForce RTX 3080 Ti
    "(4, 19, 45000), " ++  -- AMD Radeon RX 5700 XT
    "(4, 20, 22000);"      -- NVIDIA GeForce GTX 1660 Super
    )

  -- Блоки питания
  execute_ conn (fromString $
    "INSERT INTO power_supplies (name, power_output, efficiency_rating, manufacture_id) VALUES " ++
    "('Corsair RM850x', 850, '80+ Gold', 7), " ++
    "('Seasonic Focus GX-750', 750, '80+ Gold', 7), " ++
    "('EVGA SuperNOVA 850 G5', 850, '80+ Gold', 7), " ++
    "('Cooler Master V850', 850, '80+ Gold', 7), " ++
    "('Corsair SF750', 750, '80+ Platinum', 7), " ++
    "('MSI MPG A850GF', 850, '80+ Gold', 6), " ++
    "('Be Quiet! Straight Power 11', 850, '80+ Platinum', 7), " ++
    "('Seasonic Prime TX-850', 850, '80+ Titanium', 7), " ++
    "('Corsair RM750', 750, '80+ Gold', 7), " ++
    "('EVGA SuperNOVA 750 G3', 750, '80+ Gold', 7), " ++
    "('Cooler Master V750', 750, '80+ Gold', 7), " ++
    "('Corsair HX1200', 1200, '80+ Platinum', 7), " ++
    "('MSI MPG A750GF', 750, '80+ Gold', 6), " ++
    "('Seasonic Focus GX-850', 850, '80+ Gold', 7), " ++
    "('Be Quiet! Dark Power Pro 12', 1200, '80+ Titanium', 7), " ++
    "('Corsair SF600', 600, '80+ Platinum', 7), " ++
    "('EVGA SuperNOVA 1000 G5', 1000, '80+ Gold', 7), " ++
    "('Cooler Master MWE Gold 750', 750, '80+ Gold', 7), " ++
    "('Seasonic Prime PX-850', 850, '80+ Platinum', 7), " ++
    "('Corsair HX850', 850, '80+ Platinum', 7);")

  -- Добавление в таблицу Товары (category_id = 5)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(5, 1, 12000), " ++   -- Corsair RM850x
    "(5, 2, 11000), " ++   -- Seasonic Focus GX-750
    "(5, 3, 12500), " ++   -- EVGA SuperNOVA 850 G5
    "(5, 4, 11500), " ++   -- Cooler Master V850
    "(5, 5, 13000), " ++   -- Corsair SF750
    "(5, 6, 11800), " ++   -- MSI MPG A850GF
    "(5, 7, 14500), " ++   -- Be Quiet! Straight Power 11
    "(5, 8, 16000), " ++   -- Seasonic Prime TX-850
    "(5, 9, 10000), " ++   -- Corsair RM750
    "(5, 10, 10500), " ++  -- EVGA SuperNOVA 750 G3
    "(5, 11, 9800), " ++   -- Cooler Master V750
    "(5, 12, 21000), " ++  -- Corsair HX1200
    "(5, 13, 9500), " ++   -- MSI MPG A750GF
    "(5, 14, 12500), " ++  -- Seasonic Focus GX-850
    "(5, 15, 25000), " ++  -- Be Quiet! Dark Power Pro 12
    "(5, 16, 9500), " ++   -- Corsair SF600
    "(5, 17, 18000), " ++  -- EVGA SuperNOVA 1000 G5
    "(5, 18, 8500), " ++   -- Cooler Master MWE Gold 750
    "(5, 19, 15000), " ++  -- Seasonic Prime PX-850
    "(5, 20, 16000);"      -- Corsair HX850
    )

  -- Накопители данных
  execute_ conn (fromString $
    "INSERT INTO storage (name, storage_type, capacity, interface, manufacture_id) VALUES " ++
    "('Samsung 970 EVO Plus 1TB', 'SSD', 1024, 'NVMe', 9), " ++
    "('Seagate Barracuda 2TB', 'HDD', 2048, 'SATA', 10), " ++
    "('Western Digital Blue 1TB', 'HDD', 1024, 'SATA', 10), " ++
    "('Samsung 860 EVO 500GB', 'SSD', 500, 'SATA', 9), " ++
    "('Crucial MX500 1TB', 'SSD', 1024, 'SATA', 8), " ++
    "('Samsung 980 Pro 1TB', 'SSD', 1024, 'NVMe', 9), " ++
    "('Western Digital Black 2TB', 'SSD', 2048, 'NVMe', 10), " ++
    "('Seagate FireCuda 520 1TB', 'SSD', 1024, 'NVMe', 10), " ++
    "('Crucial P5 1TB', 'SSD', 1024, 'NVMe', 8), " ++
    "('Samsung 970 EVO Plus 500GB', 'SSD', 500, 'NVMe', 9), " ++
    "('Seagate Barracuda 1TB', 'HDD', 1024, 'SATA', 10), " ++
    "('Western Digital Blue 2TB', 'HDD', 2048, 'SATA', 10), " ++
    "('Crucial MX500 500GB', 'SSD', 500, 'SATA', 8), " ++
    "('Samsung 980 Pro 500GB', 'SSD', 500, 'NVMe', 9), " ++
    "('Seagate FireCuda 2TB', 'HDD', 2048, 'SATA', 10), " ++
    "('Crucial P5 500GB', 'SSD', 500, 'NVMe', 8), " ++
    "('Western Digital Black 1TB', 'SSD', 1024, 'NVMe', 10), " ++
    "('Seagate FireCuda 520 2TB', 'SSD', 2048, 'NVMe', 10), " ++
    "('Crucial MX500 2TB', 'SSD', 2048, 'SATA', 8), " ++
    "('Samsung 970 EVO Plus 2TB', 'SSD', 2048, 'NVMe', 9);")

  -- Добавление в таблицу Товары (category_id = 6)
  execute_ conn (fromString $
    "INSERT INTO products (category_id, box_id, price) VALUES " ++
    "(6, 1, 15000), " ++   -- Samsung 970 EVO Plus 1TB
    "(6, 2, 4500), " ++    -- Seagate Barracuda 2TB
    "(6, 3, 4000), " ++    -- Western Digital Blue 1TB
    "(6, 4, 6000), " ++    -- Samsung 860 EVO 500GB
    "(6, 5, 9000), " ++    -- Crucial MX500 1TB
    "(6, 6, 17000), " ++   -- Samsung 980 Pro 1TB
    "(6, 7, 18000), " ++   -- Western Digital Black 2TB
    "(6, 8, 16000), " ++   -- Seagate FireCuda 520 1TB
    "(6, 9, 14000), " ++   -- Crucial P5 1TB
    "(6, 10, 8000), " ++   -- Samsung 970 EVO Plus 500GB
    "(6, 11, 3500), " ++   -- Seagate Barracuda 1TB
    "(6, 12, 5500), " ++   -- Western Digital Blue 2TB
    "(6, 13, 5000), " ++   -- Crucial MX500 500GB
    "(6, 14, 11000), " ++  -- Samsung 980 Pro 500GB
    "(6, 15, 8500), " ++   -- Seagate FireCuda 2TB
    "(6, 16, 7000), " ++   -- Crucial P5 500GB
    "(6, 17, 12000), " ++  -- Western Digital Black 1TB
    "(6, 18, 25000), " ++  -- Seagate FireCuda 520 2TB
    "(6, 19, 18000), " ++  -- Crucial MX500 2TB
    "(6, 20, 30000);"      -- Samsung 970 EVO Plus 2TB
    )

  putStrLn "Курсовой проект выполнен Ерёминым Б.Е. гр. 421-2."
  putStrLn "База данных инициализирована и готова к работе."
  close conn
