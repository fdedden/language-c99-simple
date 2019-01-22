{- This module implements a simplified version of a C99 AST. It omits a lot of
 - specific and rarely used language constructs and features, which typically
 - are not used by code generators anyway. Some parts of the AST differ quite a
 - lot from the C99 one, and do not necessarily mimic their counterparts, even
 - if the names are similar.
 -
 - A total translation function proves that the simplified AST can be rewritten
 - in terms of the full AST, and thus is a subset.
-}

module Language.C99.Simple.AST where

type Ident    = String

data TransUnit = TransUnit [ExtDecln]

data ExtDecln = ExtFun FunDef
              | ExtDecln Decln

data FunDef -- TODO

data Decln = Decln (Maybe StorageSpec) Type Ident Init

data StorageSpec = Typedef
                 | Extern
                 | Static
                 | Auto
                 | Register

data Type = Type     Type
          | TypeSpec TypeSpec
          | Ptr      Type
          | Array    Type (Maybe Expr)

          | Const    Type
          | Restrict Type
          | Volatile Type

data TypeSpec = Void
              | Char
              | Signed_Char
              | Unsigned_Char

              | Short
              | Signed_Short
              | Short_Int
              | Signed_Short_Int

              | Unsigned_Short
              | Unsigned_Short_Int

              | Int
              | Signed
              | Signed_Int

              | Unsigned
              | Unsigned_Int

              | Long
              | Signed_Long
              | Long_Int
              | Signed_Long_Int

              | Unsigned_Long
              | Unsgined_Long_Int

              | Long_Long
              | Signed_Long_Long
              | Long_Long_int
              | Signed_Long_Long_Int

              | Unsigned_Long_Long
              | Unsigned_Long_Long_Int

              | Float
              | Double
              | Long_Double
              | Bool
              | Float_Complex
              | Double_Complex
              | Long_Double_Complex
              | TypedefName Ident

data Init -- TODO

data Expr = Ident     Ident
          | LitInt    Integer
          | LitDouble Double
          | LitString String

          | Index   Expr Expr
          | Funcall Expr [Expr]
          | Dot     Expr Ident
          | Arrow   Expr Ident
          | InitVal TypeName Init

          | UnaryOp UnaryOp Expr

          | Cast  TypeName Expr

          | BinaryOp BinaryOp Expr Expr

          | AssignOp AssignOp Expr Expr

data UnaryOp = Inc
             | Dec
             | Ref
             | DeRef
             | Plus
             | Min
             | BoolNot
             | Not

data BinaryOp = Mult
              | Div
              | Mod
              | Add
              | Sub
              | ShiftL
              | ShiftR
              | Lessthan
              | GreaterThan
              | LessThanEq
              | GreaterThanEq
              | Equal
              | NotEqual
              | And
              | XOr
              | Or
              | LAnd
              | LOr

data AssignOp = Assign
              | AssignMult
              | AssignDiv
              | AssignMod
              | AssignAdd
              | AssignSub
              | AssignShiftL
              | AssignShiftR
              | AssignAnd
              | AssignXOr
              | AssignOr

data TypeName -- TODO
