module IBCTypes where

import qualified Idris.Core.TT as T
import qualified Idris.AbsSyntaxTree as A

type NameRef = Int
type TermRef = Int
type TextRef = Int
type PTermRef = Int

data FBinder =
    Lam TermRef |
    Pi (Maybe ImplicitInfo) TermRef TermRef |
    Let TermRef TermRef |
    NLet TermRef TermRef |
    Hole TermRef |
    GHole Int [NameRef] TermRef |
    Guess TermRef TermRef |
    PVar TermRef |
    PVTy TermRef
    deriving (Show, Eq, Ord)


data FTerm =
    P NameType NameRef TermRef |
    V Int |
    Bind NameRef FBinder TermRef |
    App FAppStatus TermRef TermRef |
    Constant Const |
    Proj TermRef Int |
    Erased |
    Impossible |
    TType UExp |
    UType Universe
    deriving (Show, Eq, Ord)

data FAppStatus =
    Complete |
    MaybeHoles |
    Holes [NameRef]
    deriving (Show, Eq, Ord)

type FFC = Maybe (Word64, Word64)

data FPTerm =
    PQuote Raw |
    PRef FFC [FFC] NameRef |
    PInferDef FFC [FFC] NameRef |
    PPatvar FFC NameRef |
    PLam FFC NameRef FFC PTermRef PTermRef |
    PPi Plicity NameRef FFC PTermRef PTermRef |
    PLet FFC NameRef FFC PTermRef PTermRef PTermRef |
    PTyped PTermRef PTermRef |
    PApp FFC PTermRef [FPArg] |
    PWithApp FFC PTermRef PTermRef |
    PAppImpl PTermRef [ImplicitInfo] |
    PAppBind FFC PTermRef [FPArg] |
    PMatchApp FFC NameRef |
    PIfThenElse FFC PTermRef PTermRef PTermRef |
    PCase FFC PTermRef [(PTermRef, PTermRef)] |
    PTrue FFC PunInfo |
    PResolveTC FFC |
    PRewrite FFC PTermRef PTermRef (Maybe PTermRef) |
    PPair FFC [FFC] PunInfo PTermRef PTermRef |
    PDPair FFC [FFC] PunInfo PTermRef PTermRef PTermRef |
    PAs FFC NameRef PTermRef |
    PAlternative [(NameRef, NameRef)] PAltType [PTermRef] |
    PHidden PTermRef |
    PType FFC |
    PUniverse Universe |
    PGoal FFC PTermRef NameRef PTermRef |
    PConstant FFC Const |
    PlaceHolder |
    PDoBlock [FPDo] |
    PIdiom FFC PTermRef |
    PReturn FFC |
    PMetavar FFC NameRef |
    PProof [FPTactic] |
    PTactics [FPTActic] |
    PElabError Err |
    PImpossible |
    PCoerced PTermRef |
    PDisamb [[TextRef]] PTermRef |
    PUnifyLog PTermRef |
    PNoImplicits PTermRef |
    PQuasiquote PTermRef (Maybe PTermRef) |
    PUnquote PTermRef |
    PQuoteName NameRef Bool FFC |
    PRunElab FFC PTermRef [TextRef] |
    PConstSugar FFC PTermRef
    deriving (Eq, Show, Ord)
