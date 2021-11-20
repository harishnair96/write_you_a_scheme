import LispVal (Eval, LispVal (Bool, List, Nil, Number, String))

eval :: LispVal -> Eval LispVal
eval lispVal = case lispVal of
  str@(String _) -> return str
  num@(Number _) -> return num
  bool@(Bool _) -> return bool
  Nil -> return Nil
  List [] -> return Nil -- Empty lists are treated as Nil