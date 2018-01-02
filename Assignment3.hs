-- Areege Chaudhary, Student #: 10197607
-- CISC260 Assignment 3
-- March 14, 2017

module Assignment3 where
import SImpL

--TASK 1: Takes string x, string y and a SImpL program and replaces all instances of x with y within the program

changeName :: String -> String -> StmtT -> StmtT
changeName x y (Seq []) = Seq []
changeName x y (Seq list) = Seq (map (changeName x y) list)
changeName x y (Assign name aExp)
     | x == name = Assign y (changeNameAExp x y aExp)
     | otherwise = Assign name (changeNameAExp x y aExp)
changeName x y (While bExp stmt) = While (changeNameBExp x y bExp) (changeName x y stmt)
changeName x y (If bExp stmt1 stmt2) = If (changeNameBExp x y bExp) (changeName x y stmt1) (changeName x y stmt2)

changeNameAExp x y (ALit z) = ALit z
changeNameAExp x y (AName name) 
     | x == name = AName y
     | otherwise = AName name
changeNameAExp x y (Add aExp1 aExp2) = Add (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)
changeNameAExp x y (Sub aExp1 aExp2) = Sub (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)
changeNameAExp x y (Mult aExp1 aExp2) = Mult (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)

changeNameBExp x y (BLit z) = BLit z
changeNameBExp x y (Eq aExp1 aExp2) = Eq (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)
changeNameBExp x y (Less aExp1 aExp2) = Less (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)
changeNameBExp x y (Greater aExp1 aExp2) = Greater (changeNameAExp x y aExp1) (changeNameAExp x y aExp2)
changeNameBExp x y (Not bExp) = Not (changeNameBExp x y bExp)
changeNameBExp x y (And bExp1 bExp2) = And (changeNameBExp x y bExp1) (changeNameBExp x y bExp2)
changeNameBExp x y (Or bExp1 bExp2) = Or (changeNameBExp x y bExp1) (changeNameBExp x y bExp2) 

--TASK 2: prints out a string representation of a SImpL program with indentation.

prettyPrint :: StmtT -> String
prettyPrint stmt = ppIndent stmt 0

ppIndent :: StmtT -> Int -> String
ppIndent (Seq []) level = ""
ppIndent (Seq (headS:tailS)) level =
     ppIndent headS level ++ ppIndent (Seq tailS) level
ppIndent (Assign name aExp) level =
     "\n" ++ (indent level) ++ "(Assign " ++ name ++ " " ++ ppAExp aExp ++ ")"
ppIndent (While bExp stmt) level = 
     "\n" ++ (indent level) ++ "(While " ++ ppBExp bExp ++ ppIndent stmt (level+1) ++ "\n" ++ (indent level) ++ ")" --fill in bExp part
ppIndent (If bExp stmt1 stmt2) level =
     "\n" ++ (indent level) ++ "(If " ++ ppBExp bExp ++ " " ++ ppIndent stmt1 (level+1) ++ ppIndent stmt2 (level+1) ++ "\n" ++ (indent level) ++ ")"

ppAExp :: AExprT -> String
ppAExp (ALit value) = "(ALit " ++ show value ++ ")"
ppAExp (AName name) = "(AName \"" ++ name ++ "\")"
ppAExp (Add aExp1 aExp2) = "(Add " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"
ppAExp (Sub aExp1 aExp2) = "(Sub " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"
ppAExp (Mult aExp1 aExp2) = "(Mult " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"

ppBExp :: BExprT -> String
ppBExp (Eq aExp1 aExp2) = "(Eq " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"
ppBExp (Less aExp1 aExp2) = "(Less " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"
ppBExp (Greater aExp1 aExp2) = "(Greater " ++ ppAExp aExp1 ++ " " ++ ppAExp aExp2 ++ ")"
ppBExp (Not bExp) = "(Not " ++ ppBExp bExp ++ ")"
ppBExp (And bExp1 bExp2) = "(And " ++ ppBExp bExp1 ++ " " ++ ppBExp bExp2 ++ ")"
ppBExp (Or bExp1 bExp2) = "(Or " ++ ppBExp bExp1 ++ " " ++ ppBExp bExp2 ++ ")"
ppBExp (BLit boolean)
     | (boolean == True) = "(BLit True)"
     | (boolean == False) = "(BLit False)"

indent :: Int -> String
indent 0 = ""
indent n = "    " ++ (indent (n-1))

--TASK 3: a SImpL program that calculates the factorial of a non-negative number

factProgram = Seq [
                    (Assign "fact" (ALit 1)),
                    (Assign "count" (ALit 1)),
                    (While (Less (AName "count") (Add (AName "n") (ALit 1)))  
                            (Seq [
                                (Assign "fact" (Mult (AName "fact") (AName "count"))),
                                (Assign "count" (Add (AName "count") (ALit 1)))
                            ]) --end Seq 
                    ) --end while
                ]-- end Seq

--TASK 4: returns a State after it has been executed with a SImpL program and a count of how many times an Assign statement was used within the program

execAndCount :: StateT -> ProgT -> (StateT, ValT)
execAndCount s program = eacHelper s program 0

eacHelper :: StateT -> ProgT -> ValT -> (StateT, ValT)
eacHelper s (Assign n ae) counter = ((set s n (evalA s ae)), (counter+1))
eacHelper s (Seq []) counter = (s, counter)
eacHelper s (Seq (statement:moreStatements)) counter = 
    eacHelper (getState (eacHelper s statement counter)) (Seq moreStatements) (getCounter (eacHelper s statement counter))
eacHelper s (If b s1 s2) counter
    | evalB s b = eacHelper s s1 counter
    | otherwise = eacHelper s s2 counter
eacHelper s (While b stmt) counter
    | not (evalB s b) = (s, counter)
    | otherwise = eacHelper (getState (eacHelper s stmt counter)) (While b stmt) (getCounter (eacHelper s stmt counter))

getCounter :: (StateT, ValT) -> ValT
getCounter (state, count) = count

getState :: (StateT, ValT) -> StateT
getState (state, count) = state

