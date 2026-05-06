loopLeitura :: [(String, String)] -> IO ()
loopLeitura agenda = do
    linha <- getLine
    let ws = words linha
    let cmd = if null ws then "" else head ws
    
    if cmd == "sair"
        then putStrLn "Encerrando."
        else do
            agendaAtt <- qualComando linha agenda
            loopLeitura agendaAtt

qualComando :: String -> [(String, String)] -> IO [(String, String)]
qualComando linha agenda = case cmd of
    "adicionar" -> adicionarCtt agenda nome tel
    "buscar"    -> buscarCtt agenda nome
    "listar"    -> listar agenda
    "remover"   -> removerCtt agenda nome
    where 
        ws = words linha
        cmd = if null ws then "" else head ws
        resto = if null ws then [] else tail ws
        nome = if null resto then "" else head resto
        tel = if length resto > 1 then resto !! 1 else ""

adicionarCtt :: [(String, String)] -> String -> String -> IO [(String, String)]
adicionarCtt agenda nome tel = return $ 
    if nome `elem` map fst agenda
        then map atualizar agenda
        else (nome, tel) : agenda
    where atualizar (n, t) = if n == nome
                             then (n, tel)
                             else (n, t)

buscarCtt :: [(String, String)] -> String -> IO [(String, String)]
buscarCtt agenda nome = do
    case lookup nome agenda of
        Just tel -> putStrLn (nome ++ " - " ++ tel)
        Nothing  -> putStrLn "Contato nao encontrado."
    return agenda

listar :: [(String, String)] -> IO [(String, String)]
listar [] = do
    putStrLn "Agenda vazia"
    return []
listar agenda = do
    listarAux agenda
    return agenda
    where
        listarAux [] = return ()
        listarAux ((n, t):resto) = do
            putStrLn (n ++ " - " ++ t)
            listarAux resto

removerCtt :: [(String, String)] -> String -> IO [(String, String)]
removerCtt agenda nome = do
    let novaAgenda = remover agenda
    if length novaAgenda == length agenda
        then putStrLn "Contato nao encontrado."
        else putStrLn "Contato removido."
    return novaAgenda
    where
        remover [] = []
        remover ((n, t):resto) = if n == nome
                                then remover resto
                                else (n, t) : remover resto

main :: IO ()
main = loopLeitura []