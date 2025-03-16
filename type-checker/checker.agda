open import Data.List
open import Data.Product
open import Data.Maybe

postulate Id : Set
postulate Name : Set

data E : Set where
  Hole : E
  Bind : Id → E
  Ann : E → E → E
  Uni : E

  Let : List (Id × E) → E → E

  Pi : Maybe Id → E → E → E
  Lam : Id → E → E → E
  App : E → E → E 

  Sigma : List (Name × Maybe Id × E) → E
  Obj : List (Name × E) → E
  Prop : E → Name → E
