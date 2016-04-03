module IBCTypes where

import Idris.Core.TT
import Idris.AbsSyntaxTree

type NameRef = Int
type TermRef = Int
type TextRef = Int
type PTermRef = Int

data FBinder =
    FLam TermRef |
    FPi (Maybe ImplicitInfo) TermRef TermRef |
    FLet TermRef TermRef |
    FNLet TermRef TermRef |
    FHole TermRef |
    FGHole Int [NameRef] TermRef |
    FGuess TermRef TermRef |
    FPVar TermRef |
    FPVTy TermRef
    deriving (Show, Eq, Ord)


data FTerm =
    FP NameType NameRef TermRef |
    FV Int |
    FBind NameRef FBinder TermRef |
    FApp FAppStatus TermRef TermRef |
    FConstant Const |
    FProj TermRef Int |
    FErased |
    FImpossible |
    FTType UExp |
    FUType Universe
    deriving (Show, Eq, Ord)

data FAppStatus =
    FComplete |
    FMaybeHoles |
    FHoles [NameRef]
    deriving (Show, Eq, Ord)

data FPTerm =
    FPQuote Raw |
    FPRef FFC [FFC] NameRef |
    FPInferDef FFC [FFC] NameRef |
    FPPatvar FFC NameRef |
    FPLam FFC NameRef FFC PTermRef PTermRef |
    FPPi Plicity NameRef FFC PTermRef PTermRef |
    FPLet FFC NameRef FFC PTermRef PTermRef PTermRef |
    FPTyped PTermRef PTermRef |
    FPApp FFC PTermRef [FPArg] |
    FPWithApp FFC PTermRef PTermRef |
    FPAppImpl PTermRef [ImplicitInfo] |
    FPAppBind FFC PTermRef [FPArg] |
    FPMatchApp FFC NameRef |
    FPIfThenElse FFC PTermRef PTermRef PTermRef |
    FPCase FFC PTermRef [(PTermRef, PTermRef)] |
    FPTrue FFC PunInfo |
    FPResolveTC FFC |
    FPRewrite FFC PTermRef PTermRef (Maybe PTermRef) |
    FPPair FFC [FFC] PunInfo PTermRef PTermRef |
    FPDPair FFC [FFC] PunInfo PTermRef PTermRef PTermRef |
    FPAs FFC NameRef PTermRef |
    FPAlternative [(NameRef, NameRef)] PAltType [PTermRef] |
    FPHidden PTermRef |
    FPType FFC |
    FPUniverse Universe |
    FPGoal FFC PTermRef NameRef PTermRef |
    FPConstant FFC Const |
    FPlaceHolder |
    FPDoBlock [FPDo] |
    FPIdiom FFC PTermRef |
    FPReturn FFC |
    FPMetavar FFC NameRef |
    FPProof [FPTactic] |
    FPTactics [FPTActic] |
    FPElabError Err |
    FPImpossible |
    FPCoerced PTermRef |
    FPDisamb [[TextRef]] PTermRef |
    FPUnifyLog PTermRef |
    FPNoImplicits PTermRef |
    FPQuasiquote PTermRef (Maybe PTermRef) |
    FPUnquote PTermRef |
    FPQuoteName NameRef Bool FFC |
    FPRunElab FFC PTermRef [TextRef] |
    FPConstSugar FFC PTermRef
    deriving (Eq, Show, Ord)
