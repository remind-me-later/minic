module X86.Types where

-- use NASM syntax

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
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"
  show Rsi = "rsi"
  show Rdi = "rdi"
  show Rbp = "rbp"
  show Rsp = "rsp"

data Op
  = Imm Int
  | Reg Reg
  | Mem Reg Int
  deriving (Eq)

instance Show Op where
  show (Imm i) = show i
  show (Reg r) = show r
  show (Mem r offset) =
    "["
      ++ show r
      ++ (if offset >= 0 then "+" else "")
      ++ show offset
      ++ "]"

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
  show Mov {src, dst} = "\tmov " ++ show dst ++ ", " ++ show src
  show Add {src, dst} = "\tadd " ++ show dst ++ ", " ++ show src
  show Sub {src, dst} = "\tsub " ++ show dst ++ ", " ++ show src
  show Imul {src, dst} = "\timul " ++ show dst ++ ", " ++ show src
  show Idiv {src} = "\tidiv " ++ show src
  show Cqo = "\tcqo"
  show And {src, dst} = "\tand " ++ show dst ++ ", " ++ show src
  show Or {src, dst} = "\tor " ++ show dst ++ ", " ++ show src
  show Xor {src, dst} = "\txor " ++ show dst ++ ", " ++ show src
  show Neg {op} = "\tneg " ++ show op
  show Not {op} = "\tnot " ++ show op
  show Cmp {src, dst} = "\tcmp " ++ show dst ++ ", " ++ show src
  show Push {op} = "\tpush " ++ show op
  show Pop {op} = "\tpop " ++ show op
  show Call {name} = "\tcall " ++ name
  show Ret = "\tret"
  show Jmp {label} = "\tjmp " ++ label
  show JmpCond {cond, label} = "\t" ++ show cond ++ " " ++ label
  show Label {label} = label ++ ":"
  show Syscall = "\tsyscall"
