{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module DependencyInjection where

import Prelude hiding (lookup)

import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.State
import Data.Dynamic
import Data.Functor.Identity
import Data.Map hiding (foldl)

data Binding
  = Instance Dynamic
  -- | forall a. Typeable a => Requires (a -> Binding) -- can try this later
  | Requires [TypeRep] TypeRep ([Dynamic] -> IO Dynamic)
  -- | Creates Dynamic -- to be changed to (IO Dynamic) later?

data BindingError
  = DuplicateBinding TypeRep
  | UnsatisfiedDependency TypeRep [TypeRep] -- list is chain back to the instantiation request, with the last item being the instantiation request, or an empty list if there is no binding for the requested instantiation
  deriving (Eq, Ord, Show)

analyze :: Binding -> [Binding] -> ([TypeRep] {- dups -}, [[TypeRep]] {- unsatisfiable chains -},
  Map TypeRep Binding) -- map is undefined if either of first two lists are non-empty
analyze _ = undefined -- rec empty where
  -- rec context dups bindings

key :: Binding -> TypeRep
key b =
  case b of
    Instance d -> dynTypeRep d
    Requires _ t _ -> t

dependencies :: Binding -> [TypeRep]
dependencies b =
  case b of
    Instance _ -> []
    Requires ts _ _ -> ts

mkMap :: [Binding] -> TypeRep -> [Binding]
mkMap = foldl (\mp b tr -> (if key b == tr then [b] else []) ++ mp tr) (const []) where

create :: (TypeRep -> [Binding]) -> TypeRep -> IO (Either BindingError Dynamic)
create allBindings b = fst <$> (flip runStateT empty . runExceptT . m $ b) where
  m :: TypeRep -> ExceptT BindingError (StateT (Map TypeRep Dynamic) IO) Dynamic
  m tr = do
    mbDyn <- gets (lookup tr)
    case mbDyn of
      Just d -> return d
      Nothing -> do
        case allBindings tr of
          [] -> throwError (UnsatisfiedDependency tr [] {- todo -})
          (_:_:_) -> throwError (DuplicateBinding tr)
          [Instance d] -> do
            modify (insert tr d)
            return d
          [Requires deps _ builder] -> do
            dyns <- mapM m deps
            result <- liftIO $ builder dyns
            modify (insert tr result)
            return result

{-
create :: (TypeRep -> [Binding]) -> Map TypeRep Dynamic -> TypeRep -> Dynamic
create allBindings discoveredBindings target =
  case lookup target discoveredBindings of
    Just d -> d
    Nothing ->
      case allBindings target of
        [] -> error $ "no bindings for: " ++ show target
        (_:_:_) -> error $ "multiple bindings for: " ++ show target
        [Instance d] -> d
        [Requires deps _ builder] -> builder (foldl (\db tr -> insert tr (create allBindings db tr) db) discoveredBindings deps)
        -}



{-
checkCompleteness :: Binding -> (TypeRep -> [Binding]) -> [BindingError]
checkCompleteness binding bindings = nub $ rec mempty bindings binding where
  rec context bs b =
    let k = key target in
    case bs k of
      [] ->
        -}
{-
create :: (TypeRep -> [Binding]) -> Binding -> Either [BindingError] Dynamic
create bindings binding = rec mempty (fmap (,Nothing) bindings) binding where
  rec context bindingsFor target =
    let k = key target in
    case bindingsFor k of
      [] -> Left [UnsatisfiedDependency k context]
      (_:_:_) -> Left [DuplicateBinding k]
      [(_,Just d)] -> Right d
      [(b,Nothing)] ->
        let needed = dependencies b
            found = foldl nextDep bindingsFor needed
            nextDep bf dep
            find dep = rec (k:context) bindingsFor
        case rec (k:context)
      Nothing -> Left [UnsatisfiedDependency k context]
      Just (_, Just d) -> Right d
      Just ([], _) -> error "this should never happen"
      Just ([b],
      Just ((_:_:_), _) -> Left [DuplicatingBinding k]
      -}
{-
  rec mp bb =
    case bb of
      [] -> empty
      (b:bs) ->
        let k = key b in
        case lookup k mp of
          Nothing -> rec (insert k b mp) bs
          Just _ -> error $ "duplicate binding: " ++ show k
          -}

mkGraph :: [Binding] -> [(Binding, TypeRep, [TypeRep])]
mkGraph = fmap (\b -> (b, key b, dependencies b))

constructed :: (Typeable a) => a -> Binding
constructed = Instance . toDyn

class Constructor c where
  inputs :: c -> [TypeRep]
  output :: c -> TypeRep
  creator :: c -> [Dynamic] -> IO Dynamic
  -- | Requires [TypeRep] TypeRep ([Dynamic] -> IO Dynamic)

instance (Typeable a) => Constructor (Identity a) where
  inputs _ = []
  output = typeRep
  creator (Identity v) _ = return (toDyn v)

instance (Typeable a) => Constructor (IO a) where
  inputs _ = []
  output = typeRep
  creator v _ = toDyn <$> v

instance (Typeable b, Constructor a) => Constructor (b -> a)

{-
class IOConstructor f

instance (Typeable a) => IOConstructor (IO a)
instance (Typeable b, IOConstructor a) => IOConstructor (b -> a)

solve that later
-}

constructor :: Constructor c => c -> Binding
constructor = factory 0 -- well, this works in pure land, but not in IO land; will revisit later

factory :: Constructor c => Int -> c -> Binding
factory = undefined

instantiate :: Typeable a => [Binding] -> IO a
instantiate _ = undefined -- rec -- mempty bs where

-- factory takes two args
--   a function signature
--   a function which has that function signature as a suffix
--   returns a binding
-- factory :: b
