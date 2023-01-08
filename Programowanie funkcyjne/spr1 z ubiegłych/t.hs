main = do
  -- Wczytujemy wiersz tekstu
  line <- getLine
  -- Jeśli wiersz zawiera jedynie kropkę, kończymy działanie programu
  if line == "."
  then return ()
  -- W przeciwnym razie wypisujemy wiersz w odwrotnej kolejności i powtarzamy
  else do
    putStrLn (reverse line)
    main
