module X86.Types where

-- use GAS (AT&T) syntax

data Reg
  = Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rbp
  | Rsp
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq)

instance Show Reg where
  show Rax = "%rax"
  show Rbx = "%rbx"
  show Rcx = "%rcx"
  show Rdx = "%rdx"
  show Rsi = "%rsi"
  show Rdi = "%rdi"
  show Rbp = "%rbp"
  show Rsp = "%rsp"
  show R8 = "%r8"
  show R9 = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

data Op
  = Imm Int
  | Reg Reg
  | Mem
      { memBase :: Reg,
        memIndexScale :: Maybe (Reg, Int),
        memDisp :: Int
      }
  deriving (Eq)

instance Show Op where
  show (Imm i) = '$' : show i
  show (Reg r) = show r
  show Mem {memBase, memDisp = 0, memIndexScale = Nothing} = "(" ++ show memBase ++ ")"
  show Mem {memBase, memDisp, memIndexScale = Nothing} = show memDisp ++ "(" ++ show memBase ++ ")"
  show Mem {memBase, memIndexScale = Just (index, 1), memDisp = 0} =
    "(" ++ show memBase ++ ", " ++ show index ++ ")"
  show Mem {memBase, memIndexScale = Just (index, 1), memDisp} =
    show memDisp ++ "(" ++ show memBase ++ ", " ++ show index ++ ")"
  show Mem {memBase, memIndexScale = Just (index, scale), memDisp = 0} =
    "(" ++ show memBase ++ ", " ++ show index ++ ", " ++ show scale ++ ")"
  show Mem {memBase, memIndexScale = Just (index, scale), memDisp} =
    show memDisp ++ "(" ++ show memBase ++ ", " ++ show index ++ ", " ++ show scale ++ ")"

data JmpCond
  = Jnz
  | Jz
  | Je
  | Jne
  | Jl
  | Jle
  | Jg
  | Jge
  deriving (Eq)

instance Show JmpCond where
  show Jnz = "jnz"
  show Jz = "jz"
  show Je = "je"
  show Jl = "jl"
  show Jle = "jle"
  show Jg = "jg"
  show Jge = "jge"
  show Jne = "jne"

data Inst
  = Mov {movSrc :: Op, movDst :: Op}
  | Add {addSrc :: Op, addDst :: Op}
  | Sub {subSrc :: Op, subDst :: Op}
  | Imul {imulSrc :: Op, imulDst :: Op}
  | -- Note: Idiv stores the quotient in rax and the remainder in rdx
    Idiv {idivSrc :: Op}
  | Cqo -- Sign-extend rax into rdx before division
  | And {andSrc :: Op, andDst :: Op}
  | Or {orSrc :: Op, orDst :: Op}
  | Xor {xorSrc :: Op, xorDst :: Op}
  | Neg {negOp :: Op}
  | Not {notOp :: Op}
  | Cmp {cmpSrc :: Op, cmpDst :: Op}
  | Push {pushOp :: Op}
  | Pop {popOp :: Op}
  | Call {callName :: String}
  | Ret
  | Jmp {jmpLabel :: String}
  | JmpCond {jmpCond :: JmpCond, jmpCondLabel :: String}
  | Label {labelName :: String}
  | Syscall
  deriving (Eq)

instance Show Inst where
  show Mov {movSrc, movDst} = "\tmovq " ++ show movSrc ++ ", " ++ show movDst
  show Add {addSrc, addDst} = "\taddq " ++ show addSrc ++ ", " ++ show addDst
  show Sub {subSrc, subDst} = "\tsubq " ++ show subSrc ++ ", " ++ show subDst
  show Imul {imulSrc, imulDst} = "\timulq " ++ show imulSrc ++ ", " ++ show imulDst
  show Idiv {idivSrc} = "\tidivq " ++ show idivSrc
  show Cqo = "\tcqo"
  show And {andSrc, andDst} = "\tandq " ++ show andSrc ++ ", " ++ show andDst
  show Or {orSrc, orDst} = "\torq " ++ show orSrc ++ ", " ++ show orDst
  show Xor {xorSrc, xorDst} = "\txorq " ++ show xorSrc ++ ", " ++ show xorDst
  show Neg {negOp} = "\tnegq " ++ show negOp
  show Not {notOp} = "\tnotq " ++ show notOp
  show Cmp {cmpSrc, cmpDst} = "\tcmpq " ++ show cmpSrc ++ ", " ++ show cmpDst
  show Push {pushOp} = "\tpushq " ++ show pushOp
  show Pop {popOp} = "\tpopq " ++ show popOp
  show Call {callName} = "\tcall " ++ callName
  show Ret = "\tret"
  show Jmp {jmpLabel} = "\tjmp " ++ jmpLabel
  show JmpCond {jmpCond, jmpCondLabel} = "\t" ++ show jmpCond ++ " " ++ jmpCondLabel
  show Label {labelName} = labelName ++ ":"
  show Syscall = "\tsyscall"
