module Language.C99.Simple.Translate where

import GHC.Exts             (fromList)
import Control.Monad.State  (State, execState, get, put)
import Data.Maybe           (fromJust)

import           Language.C99.Simple.AST
import qualified Language.C99.AST         as C

import Language.C99.Util.Expr
import Language.C99.Util.IsList

import Language.C99.Simple.Util


translate = transtransunit

transtransunit :: TransUnit -> C.TransUnit
transtransunit (TransUnit decln) = undefined -- TODO

transfundef :: FunDef -> C.FunDef
transfundef = undefined -- TODO

transdecln :: Decln -> C.Decln
transdecln (Decln storespec ty name init) = undefined

transexpr :: Expr -> C.AssignExpr
transexpr _ = undefined -- TODO

getdeclr :: Type -> State C.Declr ()
getdeclr ty = case ty of
  Type      ty'     -> do
    getdeclr ty'
    declr <- get
    put $ C.Declr Nothing (C.DirectDeclrDeclr declr)

  TypeSpec  ty' -> return ()

  Ptr       ty' -> do
    let (quals, ty'') = gettypequals ty'
    getdeclr ty''
    declr <- get
    put $ insertptr (C.PtrBase quals) declr

  Array ty' len -> do
    let lenexpr = transexpr <$> len
    (C.Declr ptr declr) <- get
    put $ C.Declr ptr (C.DirectDeclrArray1 declr Nothing lenexpr)

  Const    ty' -> getdeclr ty'
  Restrict ty' -> getdeclr ty'
  Volatile ty' -> getdeclr ty'


getdeclnspecs :: Type -> Maybe C.DeclnSpecs
getdeclnspecs ty = case ty of
  Type     ty'   -> getdeclnspecs ty'
  TypeSpec ty'   -> Just $ foldtypespecs $ spec2spec ty'
  Ptr      ty'   -> getdeclnspecs (snd $ (gettypequals ty'))
  Array    ty' _ -> getdeclnspecs ty'
  Const    ty'   -> Just $ C.DeclnSpecsQual C.QConst    (getdeclnspecs ty')
  Restrict ty'   -> Just $ C.DeclnSpecsQual C.QRestrict (getdeclnspecs ty')
  Volatile ty'   -> Just $ C.DeclnSpecsQual C.QVolatile (getdeclnspecs ty')


spec2spec :: TypeSpec -> [C.TypeSpec]
spec2spec ts = case ts of
  Void                -> [C.TVoid]
  Char                -> [C.TChar]
  Signed_Char         -> [C.TSigned, C.TChar]
  Unsigned_Char       -> [C.TUnsigned, C.TChar]

  Short               -> [C.TShort]
  Signed_Short        -> [C.TSigned, C.TShort]
  Short_Int           -> [C.TShort, C.TInt]
  Signed_Short_Int    -> [C.TSigned, C.TShort, C.TInt]

  Unsigned_Short      -> [C.TUnsigned, C.TShort]
  Unsigned_Short_Int  -> [C.TUnsigned, C.TShort, C.TInt]

  Int                 -> [C.TInt]
  Signed              -> [C.TSigned]
  Signed_Int          -> [C.TSigned, C.TInt]

  Unsigned            -> [C.TUnsigned]
  Unsigned_Int        -> [C.TUnsigned, C.TInt]

  Long                -> [C.TLong]
  Signed_Long         -> [C.TSigned, C.TLong]
  Long_Int            -> [C.TLong, C.TInt]
  Signed_Long_Int     -> [C.TSigned, C.TLong, C.TInt]

  Unsigned_Long       -> [C.TUnsigned, C.TLong]
  Unsgined_Long_Int   -> [C.TUnsigned, C.TLong, C.TInt]

  Long_Long           -> [C.TLong, C.TLong]
  Signed_Long_Long    -> [C.TSigned, C.TLong, C.TLong]
  Long_Long_Int       -> [C.TLong, C.TLong, C.TInt]
  Signed_Long_Long_Int-> [C.TSigned, C.TLong, C.TLong, C.TInt]

  Unsigned_Long_Long      -> [C.TUnsigned, C.TLong, C.TLong]
  Unsigned_Long_Long_Int  -> [C.TUnsigned, C.TLong, C.TLong, C.TInt]

  Float               -> [C.TFloat]
  Double              -> [C.TDouble]
  Long_Double         -> [C.TLong, C.TDouble]
  Bool                -> [C.TBool]
  Float_Complex       -> [C.TComplex, C.TFloat]
  Double_Complex      -> [C.TComplex, C.TDouble]
  Long_Double_Complex -> [C.TLong, C.TDouble, C.TComplex]
  TypedefName name -> [C.TTypedef $ C.TypedefName $ ident name]
  Struct      name -> [C.TStructOrUnion $ C.StructOrUnionForwDecln C.Struct (ident name)]
  StructDecln name declns -> [C.TStructOrUnion $ C.StructOrUnionDecln C.Struct (ident <$> name) declns'] where
    declns' = fromList $ map transstructdecln declns


transstructdecln = undefined -- TODO
