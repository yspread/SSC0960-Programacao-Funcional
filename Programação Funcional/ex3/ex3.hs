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
adicionarCtt agenda nome tel = return ((nome, tel) : filter ((/= nome) . fst) agenda)

buscarCtt :: [(String, String)] -> String -> IO ([(String, String)])
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
removerCtt agenda nome =
    if nome `elem` map fst agenda          
        then do
            putStrLn "Contato removido."
            return $ filter ((/= nome) . fst) agenda
        else do
            putStrLn "Contato nao encontrado."
            return agenda

main :: IO ()
main = loopLeitura []