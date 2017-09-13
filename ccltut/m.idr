namespace Exp
  data AddExp : Type -> Type where
    Var : var -> AddExp var
    Lit : Int -> AddExp var
    Add : AddExp var -> AddExp var -> AddExp var
data Value : (var : Type) -> Type where
  Var : var -> Value var
  Lit : Int -> Value var
data LlvmInstr : Type -> Type where
  Print1 : Value var -> LlvmInstr var
  Halt1 : LlvmInstr var
  Add1 : var -> Value var -> Value var -> LlvmInstr var
data Llvm : Type -> Type where
  Add : (v : Value var) -> (w : Value var) -> (program : (z : var) -> Llvm var) -> Llvm var
  Print : Value var -> Llvm var -> Llvm var
  Halt : Llvm var
compile : AddExp var -> ((tmp : Value var) -> Llvm var) -> Llvm var
compile (Var x) compileProg = compileProg (Var x)
compile (Lit z) compileProg = compileProg (Lit z)
compile (Add e1 e2) compileProg =
  compile e1 (\tmp1 =>
  compile e2 (\tmp2 =>
  Add tmp1 tmp2 (\result =>
  compileProg (Var result))))
llvmValue : Value String -> String
llvmValue (Var x) = x
llvmValue (Lit z) = show z

prettyLlvm : Int -> Llvm String -> String
prettyLlvm name (Add v w prog) =
  let result = "%" ++ show name
  in result ++ " = add nsw i32 " ++ llvmValue v ++ ", " ++ llvmValue w ++ "\n" ++
     prettyLlvm (name+1) (prog result) 
prettyLlvm name (Print z prog) =
  "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 " ++ llvmValue z ++ ")\n" ++
  prettyLlvm name prog
prettyLlvm name Halt = "call void @exit(i32 0)\n" ++
                       "unreachable"
prog0 : AddExp var
prog0 = Lit 3 `Add` (Lit 2 `Add` Lit 4)

llvm0 : Llvm var
llvm0 = compile prog0 (\result => Print result Halt)

renderedLlvm0 : String
renderedLlvm0 = prettyLlvm 1 llvm0
llvmHeader : String
llvmHeader = "@int_format = private constant [3 x i8] c\"%i\\00\"\n" ++
             "declare i32 @printf(i8* nocapture readonly, ...)\n" ++
             "declare void @exit(i32)\n\n"

llvmMain : String -> String
llvmMain prog = "define i32 @main() {\n" ++
                prog ++
                "\n}"
evalVal : Value Int -> Int
evalVal (Var z) = z
evalVal (Lit z) = z

eval : Llvm Int -> IO Unit
eval (Add x y prog) = eval (prog (evalVal x + evalVal y))
eval (Print x prog) = do print (evalVal x) 
                         eval prog
eval Halt = pure ()
