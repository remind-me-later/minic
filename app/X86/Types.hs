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

data Op
  = Imm Int
  | Reg Reg
  | Mem
      { base :: Reg,
        index_scale :: Maybe (Reg, Int),
        disp :: Int
      }
  deriving (Eq)

instance Show Op where
  show (Imm i) = '$' : show i
  show (Reg r) = show r
  show Mem {base, disp = 0, index_scale = Nothing} = "(" ++ show base ++ ")"
  show Mem {base, disp, index_scale = Nothing} = show disp ++ "(" ++ show base ++ ")"
  show Mem {base, index_scale = Just (index, 1), disp = 0} =
    "(" ++ show base ++ ", " ++ show index ++ ")"
  show Mem {base, index_scale = Just (index, 1), disp} =
    show disp ++ "(" ++ show base ++ ", " ++ show index ++ ")"
  show Mem {base, index_scale = Just (index, scale), disp = 0} =
    "(" ++ show base ++ ", " ++ show index ++ ", " ++ show scale ++ ")"
  show Mem {base, index_scale = Just (index, scale), disp} =
    show disp ++ "(" ++ show base ++ ", " ++ show index ++ ", " ++ show scale ++ ")"

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
  = Mov {src :: Op, dst :: Op}
  | Add {src :: Op, dst :: Op}
  | Sub {src :: Op, dst :: Op}
  | Imul {src :: Op, dst :: Op}
  | -- Note: Idiv stores the quotient in rax and the remainder in rdx
    Idiv {src :: Op}
  | Cqo -- Sign-extend rax into rdx before division
  | And {src :: Op, dst :: Op}
  | Or {src :: Op, dst :: Op}
  | Xor {src :: Op, dst :: Op}
  | Neg {op :: Op}
  | Not {op :: Op}
  | Cmp {src :: Op, dst :: Op}
  | Push {op :: Op}
  | Pop {op :: Op}
  | Call {name :: String}
  | Ret
  | Jmp {label :: String}
  | JmpCond {cond :: JmpCond, label :: String}
  | Label {label :: String}
  | Syscall
  deriving (Eq)

instance Show Inst where
  show Mov {src, dst} = "\tmovq " ++ show src ++ ", " ++ show dst
  show Add {src, dst} = "\taddq " ++ show src ++ ", " ++ show dst
  show Sub {src, dst} = "\tsubq " ++ show src ++ ", " ++ show dst
  show Imul {src, dst} = "\timulq " ++ show src ++ ", " ++ show dst
  show Idiv {src} = "\tidivq " ++ show src
  show Cqo = "\tcqo"
  show And {src, dst} = "\tandq " ++ show src ++ ", " ++ show dst
  show Or {src, dst} = "\torq " ++ show src ++ ", " ++ show dst
  show Xor {src, dst} = "\txorq " ++ show src ++ ", " ++ show dst
  show Neg {op} = "\tnegq " ++ show op
  show Not {op} = "\tnotq " ++ show op
  show Cmp {src, dst} = "\tcmpq " ++ show src ++ ", " ++ show dst
  show Push {op} = "\tpushq " ++ show op
  show Pop {op} = "\tpopq " ++ show op
  show Call {name} = "\tcall " ++ name
  show Ret = "\tret"
  show Jmp {label} = "\tjmp " ++ label
  show JmpCond {cond, label} = "\t" ++ show cond ++ " " ++ label
  show Label {label} = label ++ ":"
  show Syscall = "\tsyscall"
